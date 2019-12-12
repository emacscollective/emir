;;; emir.el --- maintain the Emacsmirror          -*- lexical-binding: t -*-

;; Copyright (C) 2016-2019  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: https://github.com/emacscollective/emir
;; Keywords: local

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3 of the License,
;; or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a copy of the GPL see https://www.gnu.org/licenses/gpl.txt.

;;; Commentary:

;; This is the package I use to maintain the Emacsmirror.  It isn't
;; very useful for anybody else.  Epkg is the user client which you
;; should be using instead.

;;; Code:

(require 'borg)
(require 'cl-lib)
(require 'closql)
(require 'dash)
(require 'dash-functional)
(require 'eieio)
(require 'elx)
(require 'emacsql-sqlite)
(require 'epkg)
(require 'epkg-org)
(require 'epkg-utils)
(require 'finder)
(require 'ghub)
(require 'magit)
(require 'org)
(require 'packed)
(require 'subr-x)

(defvar finder-no-scan-regexp)
(defvar finder--builtins-alist)
(declare-function org-publish 'ox-publish)

(defconst emir-github-token-scopes '(repo delete_repo))

(cl-pushnew (expand-file-name
             "bin" (file-name-directory (or load-file-name buffer-file-name)))
            exec-path :test #'equal)

;;; Variables

;; Values are set in "<epkg-repository>/emir.org".

(defvar emir-emacs-reference nil
  "The Emacs reference used to extract builtin packages.")

(defvar emir-pending-packages nil
  "List of packages that might eventually be imported.
These package only will be imported if and when upstream
has fixed known outstanding issues.")

(defvar emir-secondary-packages nil
  "List of packages that share a repository with another package.
In most cases this is detected automatically.  This variable
should only be used if the Emacsmirror and Melpa import from
a different repository.")

(defvar emir-suspended-packages nil
  "List of packages that are temporarily not being updated.")

(defvar emir-renamed-files nil
  "List of files that have to be renamed after fetching with curl.
This variable should only be used as a last resort.")

;;; Repositories

(defconst emir-emacs-repository "~/git/src/emacs/emacsmirror/")
(defconst emir-gelpa-repository "~/git/emacs/gelpa/")
(defconst emir-melpa-repository "~/git/emacs/melpa/")
(defconst emir-ewiki-repository "~/git/emacs/ewiki/")
(defconst emir-stats-repository "~/git/emacs/stats/")

(defmacro with-emir-repository (arg &rest body)
  "Evaluate BODY in the repository specified by ARG.
Determine the repository by calling function `epkg-repository'
with ARG as only argument.  When ARG is t then evaluate in the
repository specified by variable `epkg-repository'."
  (declare (indent defun))
  `(let ((default-directory
           ,(if (eq arg t)
                'epkg-repository
              `(or (epkg-repository ,arg)
                   (error "Need package or string")))))
     ;; BODY could call `magit-git', which could cause the
     ;; `magit-process-mode' buffer to be created, which could
     ;; cause a prompt about unsafe directory-local variables.
     ;; Prevent that by creating the process buffer upfront
     ;; local variables disabled.
     (let ((enable-local-variables nil))
       (magit-process-buffer t))
     ,@body))

(cl-defmethod epkg-repository ((_pkg epkg-builtin-package))
  emir-emacs-repository)
(cl-defmethod epkg-repository ((_class (subclass epkg-elpa-package)))
  emir-gelpa-repository)
(cl-defmethod epkg-repository ((_class (subclass epkg-elpa-branch-package)))
  emir-gelpa-repository)
(cl-defmethod epkg-repository ((_class (subclass epkg-wiki-package)))
  emir-ewiki-repository)
(cl-defmethod epkg-repository ((_class (subclass epkg-builtin-package)))
  emir-emacs-repository)

;;; Commands
;;;; Import

;;;###autoload
(defun emir-import-emacs-packages ()
  (interactive)
  (let ((default-directory emir-emacs-repository)
        (alist (emir--builtin-packages-alist)))
    (magit-git "checkout" emir-emacs-reference)
    (emacsql-with-transaction (epkg-db)
      (pcase-dolist (`(,name . ,value) alist)
        (message "Updating %s..." name)
        (let ((pkg (epkg name)))
          (if pkg
              (let ((old (oref pkg builtin-libraries)))
                (setq old (cl-sort old #'string< :key #'car))
                (dolist (o (cl-set-difference old value :test #'equal))
                  (message "  - %S" o))
                (dolist (n (cl-set-difference value old :test #'equal))
                  (message "  + %S" n)))
            (message "  = %S" value)
            (setq pkg (epkg-builtin-package :name name))
            (emir-add pkg))
          (oset pkg builtin-libraries value)
          (emir-update pkg))
        (message "Updating %s...done" name))
      (dolist (pkg (epkgs))
        (let ((name (oref pkg name)))
          (when (and (oref pkg builtin-libraries)
                     (not (assoc name alist)))
            (message "Deleting %s..." name)
            (if (epkg-builtin-package-p pkg)
                (closql-delete pkg)
              (oset pkg builtin-libraries nil))
            (message "Deleting %s...done" name)))))))

;;;###autoload
(defun emir-import-ewiki-packages (&optional drew-only)
  (interactive "P")
  (with-emir-repository 'epkg-wiki-package
    (magit-git "checkout" "master")
    (magit-git "pull" "--ff-only" "origin")
    (magit-process-buffer)
    (if drew-only
        (--each (epkg-sql [:select :distinct [packages:name]
                           :from [packages authors]
                           :where (and (= packages:name authors:package)
                                       (= packages:class 'wiki)
                                       (= authors:name "Drew Adams"))])
          (emir-import (epkg-wiki-package :name (car it))))
      (message "Importing wiki packages asynchronously...")
      (magit-run-git-async "filter-emacswiki" "--tag" "--notes"))))

;;;###autoload
(defun emir-import-gelpa-packages ()
  (interactive)
  (emir-pull 'epkg-elpa-package)
  (dolist (name (gelpa-recipes 'name 'gelpa-subtree-recipe))
    (emir-import (epkg-elpa-package :name name))))

;;;; Add

;;;###autoload
(defun emir-add-package (name url class &rest plist)
  (interactive
   (let* ((url  (emir-read-url "Add package from url"))
          (type (epkg-read-type "Package type: "
                                (--when-let (emir--url-to-class url)
                                  (substring (symbol-name it) 5 -8))))
          (name (magit-read-string
                 "Package name"
                 (--when-let (emir--url-get url 'upstream-name)
                   (->> it
                        (replace-regexp-in-string "\\`emacs-" "")
                        (replace-regexp-in-string "\\`elisp-" "")
                        (replace-regexp-in-string "[.-]el\\'" ""))))))
     (list name url (intern (format "epkg-%s-package" type)))))
  (cond ((epkg name)
         (user-error "Package %s already exists" name))
        ((assoc name emir-pending-packages)
         (user-error "Package %s is on hold" name)))
  (let ((pkg (apply class :name name :url url plist)))
    (emir-add pkg)
    (with-emir-repository t
      (borg--sort-submodule-sections (magit-git-dir "config"))
      (borg--sort-submodule-sections ".gitmodules")
      (magit-call-git "add" "epkg.sqlite" ".gitmodules"
                      (epkg-repository pkg)))))

;;;###autoload
(defun emir-add-gelpa-packages (&optional dry-run)
  (interactive "P")
  (emir-pull 'epkg-elpa-package)
  (pcase-dolist (`(,name ,class ,url)
                 (gelpa-recipes [name class url]))
    (let ((pkg (epkg name)))
      (when (and (not (assoc name emir-pending-packages))
                 (or (not pkg)
                     (and (epkg-builtin-package-p pkg)
                          (eq class 'core)
                          (or dry-run
                              (y-or-n-p (format "\
%s is already being tracked as an `epkg-builtin-package'.
Mirror as an `epkg-elpa-core-package' instead? " name))))))
        (message "Adding %s..." name)
        (unless dry-run
          (let ((libs (and (epkg-builtin-package-p pkg)
                           (oref pkg builtin-libraries))))
            (when libs
              (closql-delete pkg))
            (cl-ecase class
              (subtree (setq pkg (epkg-elpa-package :name name)))
              (external (setq pkg (epkg-elpa-branch-package :name name)))
              (core
               (setq pkg (epkg-elpa-core-package :name name :library url))
               ;; Allowing multiple libraries is a hack on top of a
               ;; hack.  We want to avoid this second-order hack if
               ;; the single library was improperly specified in
               ;; "externals-list" using a list instead of an atom.
               (when (and (listp url) (not (cdr url)))
                 (setq url (car url)))
               (if (atom url)
                   (oset pkg url (emir--format-url pkg 'url-format))
                 ;; Keep this in sync with `emir--format-url'.
                 (oset pkg url (--map (format-spec
                                       (oref pkg url-format)
                                       `((?m . ,(oref pkg mirror-name))
                                         (?n . ,(oref pkg upstream-name))
                                         (?u . ,(oref pkg upstream-user))
                                         (?l . ,it)))
                                      url)))))
            (emir-add pkg)
            (when libs
              (oset pkg builtin-libraries libs)))
          (oset (gelpa-get name) epkg-package name))
        (message "Adding %s...done" name))))
  (unless dry-run
    (emir--commit "add")))

;;;###autoload
(defun emir-add-melpa-packages (&optional dry-run)
  (interactive "P")
  (let ((mirrored (epkgs 'url)))
    (pcase-dolist (`(,name ,class ,url ,branch)
                   (melpa-recipes [name class url branch]))
      (unless (or (epkg name)
                  (member url mirrored)
                  (assoc name emir-pending-packages)
                  (assoc name emir-secondary-packages))
        (message "Adding %s..." name)
        (unless dry-run
          (apply #'emir-add-package name url
                 (intern (format "epkg-%s-package" class))
                 (and branch (list :upstream-branch branch)))
          (oset (melpa-get name) epkg-package name))
        (message "Adding %s...done" name)))
    (emir--commit "add")))

;;;; Update

(defvar emir-failed nil)

;;;###autoload
(defun emir-update-package (name &optional force)
  (interactive (list (epkg-read-package "Update package: ")
                     current-prefix-arg))
  (let* ((pkg (epkg name))
         (tip (oref pkg hash)))
    (condition-case err
        (with-emir-repository pkg
          (when (or force (cl-typep pkg 'epkg-mirrored-package))
            (emir-pull pkg))
          (emir-update pkg)
          (unless (epkg-builtin-package-p pkg)
            (with-emir-repository t
              (magit-call-git "add" (epkg-repository pkg))))
          (when (or force (not (equal (oref pkg hash) tip)))
            (emir-gh-update pkg)
            (emir-push pkg)))
      (error
       (push (oref pkg name) emir-failed)
       (message "Update error: %s" (error-message-string err))))))

;;;###autoload
(defun emir-update-github-packages ()
  (interactive)
  (emir-gh-foreach-query
   (lambda (pkg)
     `((ref [(qualifiedName
              ,(concat "refs/heads/" (or (oref pkg upstream-branch) "master")))]
            (target oid))
       (stargazers totalCount)))
   (lambda (data)
     (pcase-dolist (`(,name . ,data) data)
       (let-alist data
         (let ((pkg (epkg name))
               (rev .ref.target.oid))
           (if (not rev)
               (message "Skipping removed %s" name)
             (oset pkg stars (or .stargazers.totalCount 0))
             ;; FIXME This is always true for patched packages.
             (unless (equal rev (oref pkg hash))
               (message "Updating %s..." name)
               (emir-update-package name)
               (message "Updating %s...done" name))))))
     (emir--commit "update"))))

;;;###autoload
(defun emir-update-other-packages (&optional from recreate)
  (interactive (list (and current-prefix-arg
                          (epkg-read-package "Limit to packages after: "))))
  (dolist (pkg (epkgs nil 'epkg-mirrored-package--eieio-childp))
    (unless (cl-typep pkg 'epkg-github-package)
      (let ((name (oref pkg name)))
        (when (or (not from) (string< from name))
          (if (assoc name emir-suspended-packages)
              (message "Skipping suspended %s" name)
            (message "Updating %s..." name)
            (if recreate
                (emir-update (epkg name) t)
              (emir-update-package name))
            (message "Updating %s...done" name))))))
  (emir--commit "update"))

;;;###autoload
(defun emir-update-licenses (&optional all)
  (interactive "P")
  (dolist (pkg (epkgs))
    (let ((name (oref pkg name)))
      (when (or all
                (member (oref pkg license)
                        '(nil "failure" "pending" "none" "custom")))
        (message "Updating %s..." name)
        (let* ((pkg (epkg name))
               (default-directory (epkg-repository pkg)))
          (--when-let (emir--main-library pkg)
            (with-temp-buffer
              (insert-file-contents it)
              (oset pkg license (emir--license pkg)))))
        (message "Updating %s...done" name)))))

;;;; Patch

;;;###autoload
(defun emir-join-provided (pkg feature reason)
  (interactive
   (let* ((pkg      (epkg (epkg-read-package "Package: ")))
          (features (oref pkg provided))
          (feature  (intern (read-string "Join provide: ")))
          (reason   (read-string "Reason: " (nth 2 (assq feature features)))))
     (list pkg feature reason)))
  (let* ((val (oref pkg provided))
         (elt (assq feature val)))
    (if elt
        (progn (setf (nth 2 elt) reason)
               (oset pkg provided val))
      (oset pkg provided (cons (list feature nil reason) val)))))

;;;###autoload
(defun emir-drop-provided (pkg feature reason)
  (interactive
   (let* ((pkg      (epkg (epkg-read-package "Package: ")))
          (features (oref pkg provided))
          (feature  (intern (completing-read "Drop provide: "
                                             (mapcar #'car features)
                                             nil t)))
          (reason   (read-string "Reason: " (nth 1 (assq feature features)))))
     (list pkg feature
           (and (not (equal reason "")) reason))))
  (let* ((val (oref pkg provided))
         (elt (assq feature val)))
    (setf (nth 1 elt) reason)
    (oset pkg provided val)))

;;;###autoload
(defun emir-drop-required (pkg feature reason)
  (interactive
   (let* ((pkg      (epkg (epkg-read-package "Package: ")))
          (features (oref pkg required))
          (feature  (intern (completing-read "Drop required: "
                                             (mapcar #'car features)
                                             nil t)))
          (reason   (read-string "Reason: " (nth 3 (assq feature features)))))
     (list pkg feature
           (and (not (equal reason "")) reason))))
  (let* ((val (oref pkg required))
         (elt (assq feature val)))
    (setf (nth 3 elt) reason)
    (oset pkg required val)))

;;;; Shelve

;;;###autoload
(defun emir-shelve-package (name)
  (interactive (list (epkg-read-package "Shelve package: ")))
  (let ((pkg (epkg name)))
    (with-demoted-errors "Error: %S"
      ;; The Github api does not support repository transfers.
      (emir-gh-delete pkg))
    (with-emir-repository pkg
      (magit-git "reset" "--hard" "HEAD"))
    (with-emir-repository t
      (magit-git "mv"
                 (concat "mirror/" name)
                 (concat "attic/" name)))
    (closql--set-object-class (epkg-db) pkg 'epkg-shelved-package)
    (oset pkg mirrorpage (emir--format-url pkg 'mirrorpage-format))
    (let ((url (emir--format-url pkg 'mirror-url-format)))
      (oset pkg mirror-url url)
      (with-emir-repository pkg
        (magit-git "remote" "set-url" "mirror" url))
      (with-emir-repository t
        (magit-git "config" "-f" ".gitmodules"
                   (concat "submodule." name ".url")
                   url)
        (magit-git "add" ".gitmodules" "epkg.sqlite")))
    (emir-update  pkg)
    (emir-gh-init pkg)
    (emir-push    pkg)))

;;;; Remove

;;;###autoload
(defun emir-remove-package (name)
  (interactive (list (epkg-read-package "Remove package: ")))
  (let ((pkg (epkg name)))
    (unless (epkg-builtin-package-p pkg)
      (with-emir-repository t
        (magit-git "add" ".gitmodules")
        (let ((module-dir (epkg-repository pkg)))
          (when (file-exists-p module-dir)
            (magit-git "rm" "-f" module-dir))))
      (with-demoted-errors "Error: %S"
        (emir-gh-delete pkg)))
    (closql-delete pkg)
    (with-emir-repository t
      (magit-call-git "add" "epkg.sqlite"))
    (when (epkg-wiki-package-p pkg)
      (with-emir-repository 'epkg-wiki-package
        (magit-call-git "branch" "-D" name)))
    (with-demoted-errors "Error: %S"
      (with-emir-repository t
        (delete-directory (magit-git-dir (concat "modules/" name)) t)))))

;;;; Setup

;;;###autoload
(defun emir-setup-module (name)
  (interactive (list (epkg-read-package "Update package: ")))
  (condition-case err
      (progn (message "Setup module %s..." name)
             (emir-setup (epkg name))
             (message "Setup module %s...done" name))
    (error (push name emir-failed)
           (message "Update error: %s" (error-message-string err)))))

;;;###autoload
(defun emir-setup-modules ()
  (interactive)
  (setq emir-failed nil)
  (setq message-log-max 20000)
  (mapc #'emir-setup-module
        (epkgs 'name '(epkg-mirrored-package--eieio-childp
                       epkg-shelved-package--eieio-childp))))

;;; Git
;;;; Import

(cl-defmethod emir-import ((pkg epkg-wiki-package))
  (with-emir-repository 'epkg-wiki-package
    (with-slots (name) pkg
      (message "Importing %s..." name)
      (magit-git "filter-emacswiki" "--tag" "--notes" name)
      (message "Importing %s...done" name))))

(cl-defmethod emir-import ((pkg epkg-elpa-package))
  (with-emir-repository 'epkg-elpa-package
    (with-slots (name) pkg
      (message "Importing %s..." name)
      (magit-git "branch" "-f" (concat "directory/" name) "master")
      (magit-git "filter-elpa" name)
      (message "Importing %s...done" name))))

;;;; Clone

(cl-defmethod emir-clone :before ((pkg epkg-elpa-package))
  (emir-import pkg))

(cl-defmethod emir-clone :before ((pkg epkg-wiki-package))
  (emir-import pkg))

(cl-defmethod emir-clone ((pkg epkg-mirrored-package))
  (let* ((name   (oref pkg name))
         (mirror (oref pkg mirror-url))
         (origin (oref pkg url))
         (branch (or (oref pkg upstream-branch) "master"))
         (module (concat "mirror/" name)))
    (with-emir-repository t
      (cl-typecase pkg
        (epkg-wiki-package
         (setq origin (file-relative-name emir-ewiki-repository))
         (setq branch name))
        (epkg-elpa-package
         (setq origin (file-relative-name emir-gelpa-repository))
         (setq branch (concat "directory/" name)))
        (epkg-elpa-branch-package
         (setq origin (file-relative-name emir-gelpa-repository))
         (setq branch (concat "externals/" name))))
      (magit-git "clone"
                 (and (or (cl-typep pkg 'epkg-subtree-package)
                          (cl-typep pkg 'epkg-subset-package))
                      "--no-tags")
                 "--single-branch" "--branch" branch origin module)
      (magit-git "submodule" "add" "--name" name mirror module)
      (magit-git "submodule" "absorbgitdirs" module))
    (with-emir-repository pkg
      (magit-git "remote" "add" "mirror" mirror)
      (magit-git "config" "remote.pushDefault" "mirror")
      (unless (equal branch "master")
        (magit-git "branch" "--move" branch "master")))))

(cl-defmethod emir-clone ((pkg epkg-file-package))
  (let* ((name   (oref pkg name))
         (mirror (oref pkg mirror-url))
         (module (concat "mirror/" name)))
    (with-emir-repository t
      (magit-git "init" module))
    (emir-pull pkg t)
    (with-emir-repository t
      (magit-git "submodule" "add" "--name" name mirror module)
      (magit-git "submodule" "absorbgitdirs" module))
    (with-emir-repository pkg
      (magit-git "remote" "add" "mirror" mirror)
      (magit-git "config" "remote.pushDefault" "mirror"))))

(cl-defmethod emir-clone :after ((pkg epkg-subtree-package))
  (with-emir-repository pkg
    (magit-git "branch" "--unset-upstream"))
  (emir-pull pkg))

;;;; Pull

(cl-defmethod emir-pull ((pkg epkg-mirrored-package))
  (with-emir-repository pkg
    (if (oref pkg patched)
        (progn (magit-git "fetch" "origin")
               (magit-git "rebase" "@{upstream}"))
      (magit-git "pull" "--ff-only" "origin"))))

(cl-defmethod emir-pull ((pkg epkg-file-package) &optional force)
  (with-emir-repository pkg
    (let ((name (oref pkg name))
          (url  (oref pkg url)))
      (let ((magit-process-raise-error t))
        ;; `epkg-elpa-core-package's are allowed
        ;; to consist of more than one library.
        (dolist (url (if (listp url) url (list url)))
          (magit-call-process "curl" "-O" url)
          (--when-let (cadr (assoc name emir-renamed-files))
            (rename-file it (concat name ".el") t))))
      (when (or (magit-anything-modified-p) force)
        (magit-git "add" ".")
        (let ((process-environment process-environment)
              (mainlib (or (and (not (cl-typep pkg 'epkg-elpa-core-package))
                                (oref pkg library))
                           (ignore-errors
                             (packed-main-library
                              default-directory name t t)))))
          (when mainlib
            (with-temp-buffer
              (insert-file-contents mainlib)
              (let ((maintainer (car (elx-maintainers))))
                (push (concat "GIT_AUTHOR_NAME="  (or (car maintainer) "unknown"))
                      process-environment)
                (push (concat "GIT_AUTHOR_EMAIL=" (or (cdr maintainer) "unknown"))
                      process-environment)))
            (push "GIT_COMMITTER_NAME=Emacsmirror" process-environment)
            (push "GIT_COMMITTER_EMAIL=import@emacsmirror.org" process-environment)
            (magit-git "commit" "-m" "updates")))))))

(cl-defmethod emir-pull ((pkg epkg-subtree-package))
  (with-emir-repository pkg
    (let ((name (oref pkg name))
          (branch (or (oref pkg upstream-branch) "master")))
      (magit-git "fetch" "origin")
      (magit-git "checkout" (concat "origin/" branch))
      (let ((time (current-time)))
        (message "Filtering subtree of %s... (%s)" name
                 (format-time-string "%T"))
        (magit-git "branch" "--force" branch
                   (or (magit-git-string "subtree" "-P"
                                         (oref pkg upstream-tree) "split")
                       (error "git-subtree failed or is missing")))
        (message "Filtering subtree of %s...done (%.1fs)" name
                 (float-time (time-subtract (current-time) time))))
      (magit-git "checkout" branch))))

(cl-defmethod emir-pull ((pkg epkg-shelved-package))
  (with-emir-repository pkg
    (magit-git "pull" "--ff-only" "mirror" "master")))

(cl-defmethod emir-pull ((class (subclass epkg-elpa-package)))
  (with-emir-repository class
    (magit-git "checkout" "master")
    (magit-git "pull" "--ff-only" "origin")))

;;;; Push

(cl-defmethod emir-push ((pkg epkg-package))
  (with-emir-repository pkg
    (magit-git "push"
               (and (oref pkg patched) "--force")
               (and (not (cl-typep pkg 'epkg-subset-package)) "--follow-tags")
               "mirror" "master")))

;;;; Commit

(defun emir--commit (verb)
  (with-emir-repository t
    (let ((count (length (magit-staged-files nil "mirror"))))
      (when (> count 0)
        (magit-git "commit"
                   "-m" (format "%s %s %s" verb count
                                (if (> count 1) "packages" "package"))
                   "-i" ".gitmodules" "epkg.sqlite")))))

;;;; Setup

(cl-defmethod emir-setup ((pkg epkg-package))
  (let ((name   (oref pkg name))
        (hash   (oref pkg hash))
        (origin (oref pkg url))
        (branch (or (oref pkg upstream-branch) "master")))
    (with-emir-repository pkg
      (cl-typecase pkg
        (epkg-wiki-package
         (setq origin (file-relative-name emir-ewiki-repository))
         (setq branch name))
        (epkg-elpa-package
         (setq origin (file-relative-name emir-gelpa-repository))
         (setq branch (concat "directory/" name)))
        (epkg-elpa-branch-package
         (setq origin (file-relative-name emir-gelpa-repository))
         (setq branch (concat "externals/" name))))
      (magit-git "reset" "--hard" "HEAD")
      (magit-git "update-ref" "refs/heads/master" hash)
      (magit-git "checkout" "master")
      (magit-git "remote" "rename" "origin" "mirror")
      (magit-git "config" "remote.pushDefault" "mirror")
      (cl-typecase pkg
        (epkg-shelved-package
         (magit-git "branch" "--unset-upstream"))
        (epkg-file-package
         (magit-git "branch" "--unset-upstream"))
        (epkg-subtree-package
         (magit-git "remote" "add" "-f" "--no-tags"
                    "-t" branch "origin" origin)
         (magit-git "branch" "--unset-upstream"))
        (epkg-subset-package
         (magit-git "remote" "add" "-f" "--no-tags"
                    "-t" branch "origin" origin)
         (magit-git "branch" (concat "--set-upstream-to=origin/" branch)))
        (t
         (magit-git "remote" "add" "-f"
                    "-t" branch "origin" origin)
         (magit-git "branch" (concat "--set-upstream-to=origin/" branch)))))))

;;; Database
;;;; Add

(cl-defmethod emir-add ((pkg epkg-mirrored-package))
  (emir--set-urls pkg)
  (oset pkg mirror-name
        (replace-regexp-in-string "\\+" "-plus" (oref pkg name)))
  (when (epkg-orphaned-package-p pkg)
    (oset pkg upstream-user "emacsorphanage"))
  (oset pkg mirror-url (emir--format-url pkg 'mirror-url-format))
  (oset pkg mirrorpage (emir--format-url pkg 'mirrorpage-format))
  (oset pkg repopage   (emir--format-url pkg 'repopage-format))
  (oset pkg homepage   (emir--format-url pkg 'homepage-format))
  (closql-insert (epkg-db) pkg)
  (emir-gh-init   pkg)
  (emir-clone     pkg)
  (emir-push      pkg)
  (emir-update    pkg)
  (emir-gh-update pkg t))

(cl-defmethod emir-add ((pkg epkg-builtin-package))
  (oset pkg mirror-name (oref pkg name))
  (oset pkg url      (emir--format-url pkg 'url-format))
  (oset pkg repopage (emir--format-url pkg 'repopage-format))
  (oset pkg homepage (emir--format-url pkg 'homepage-format))
  (closql-insert (epkg-db) pkg)
  (emir-update pkg))

;;;; Update

(cl-defmethod emir-update ((pkg epkg-package) &optional recreate)
  (with-emir-repository pkg
    (with-slots (name hash builtin-libraries library) pkg
      (unless recreate
        (setf hash (magit-rev-parse "HEAD"))
        (when (and (epkg-builtin-package-p pkg)
                   (not (equal name "emacs")))
          (setf library
                (or (let ((main (concat "/" name ".el")))
                      (car (--first (string-suffix-p main (car it))
                                    builtin-libraries)))
                    library))))
      (--if-let (emir--main-library pkg)
          (with-temp-buffer
            (insert-file-contents it)
            (oset pkg summary     (elx-summary nil t))
            (oset pkg keywords    (elx-keywords-list nil t t))
            (oset pkg license     (emir--license pkg))
            (oset pkg created     (elx-created))
            (oset pkg updated     (emir--updated pkg))
            (oset pkg authors     (emir--authors))
            (oset pkg maintainers (emir--maintainers))
            (oset pkg commentary  (elx-commentary nil t))
            (oset pkg homepage    (emir--homepage pkg))
            (oset pkg wikipage    (emir--wikipage pkg)))
        (unless (or (epkg-shelved-package-p pkg)
                    (equal name "emacs"))
          (error "Cannot determine main library"))))
    (pcase-let ((`(,required ,provided)
                 (emir--features pkg)))
      (oset pkg required required)
      (oset pkg provided provided))
    (--when-let (magit-get-mode-buffer 'magit-process-mode)
      (kill-buffer it))))

;;; Extract

(cl-defmethod emir--main-library ((pkg epkg-package))
  (or (and (not (cl-typep pkg 'epkg-elpa-core-package))
           (oref pkg library))
      (let ((name (oref pkg name))
            (load-suffixes '(".el" ".el.in" ".el.tmpl"))
            (load-file-rep-suffixes '("")))
        (or (ignore-errors
              (packed-main-library default-directory name nil t))
            (and (epkg-shelved-package-p pkg)
                 (let ((file (expand-file-name (concat name ".el"))))
                   (and (file-exists-p file) file)))))))

(cl-defmethod emir--license ((pkg epkg-package))
  (let ((name (oref pkg name)))
    (or (elx-license nil (epkg-repository pkg) name)
        (and (string-match "-theme" name)
             (let ((file (expand-file-name
                          (concat (substring name 0 (match-beginning 0))
                                  ".el"))))
               (and (file-exists-p file)
                    (elx-license file))))
        (let ((license (oref pkg license)))
          (and (member license '("failure" "pending" "none" "custom"))
               license)))))

(cl-defmethod emir--updated ((_pkg epkg-package))
  (replace-regexp-in-string
   "-" "" (substring (magit-rev-format "%ci" "HEAD") 0 10)))

(cl-defmethod emir--updated ((_pkg epkg-file-package))
  (or (elx-updated)
      (cl-call-next-method)))

(cl-defmethod emir--updated ((_pkg epkg-builtin-package)))

(cl-defmethod emir--homepage ((pkg epkg-package))
  (or (caar (epkg-sql [:select page :from pkg-homepages
                       :where [(= package $s1)]]
                      (oref pkg name)))
      (--when-let (lm-homepage)
        (if (string-match-p "/$" it)
            (substring it 0 -1)
          it))
      (emir--format-url pkg 'homepage-format)))

(cl-defmethod emir--wikipage ((pkg epkg-package))
  (--when-let (or (and (epkg-wiki-package-p pkg) (elx-wikipage))
                  (caar (epkg-sql [:select page :from pkg-wikipages
                                   :where (= package $s1)]
                                  (oref pkg name)))
                  (let ((name (emir--normalize-wikipage (oref pkg name))))
                    (or (caar (epkg-sql [:select page :from raw-wikipages
                                         :where (= normalized $s1)] name))
                        (caar (epkg-sql [:select page :from raw-wikipages
                                         :where (= normalized $s1)]
                                (if (string-match-p "mode$" name)
                                    (substring name 0 -4)
                                  (concat name "mode")))))))
    (concat "https://emacswiki.org/" it)))

(defun emir--authors ()
  (cl-delete-duplicates (--map (list (car it) (cdr it)) (elx-authors))
                        :test #'equal :key #'car))

(defun emir--maintainers ()
  (cl-delete-duplicates (--map (list (car it) (cdr it)) (elx-maintainers))
                        :test #'equal :key #'car))

(cl-defmethod emir--features ((pkg epkg-package))
  (with-emir-repository pkg
    (let ((enable-local-variables nil)
          (load-suffixes '(".el" ".el.in" ".el.tmpl"))
          (load-file-rep-suffixes '(""))
          (name (oref pkg name))
          provided hard soft)
      (dolist (lib (if (epkg-builtin-package-p pkg)
                       (mapcar #'car (oref pkg builtin-libraries))
                     (packed-libraries default-directory)))
        (with-temp-buffer
          (insert-file-contents lib)
          (setq buffer-file-name lib)
          (set-buffer-modified-p nil)
          (with-syntax-table emacs-lisp-mode-syntax-table
            (-let (((h s) (packed-required))
                   (p     (packed-provided)))
              (dolist (h h) (cl-pushnew h hard))
              (dolist (s s) (cl-pushnew s soft))
              (dolist (p p) (cl-pushnew p provided))))))
      (list
       (let ((drop (epkg-sql [:select [feature drop] :from required
                              :where (and (= package $s1) (notnull drop))
                              :order-by [(asc feature)]]
                             name)))
         (setq hard (-difference hard provided))
         (setq soft (-difference soft provided))
         (setq soft (-difference soft hard))
         (nconc (--map (list it t   nil (cadr (assoc it drop))) hard)
                (--map (list it nil nil (cadr (assoc it drop))) soft)))
       (let ((drop (epkg-sql [:select [feature drop] :from provided
                              :where (and (= package $s1) (notnull drop))
                              :order-by [(asc feature)]]
                             name))
             (join (epkg-sql [:select [feature join] :from provided
                              :where (and (= package $s1) (notnull join))
                              :order-by [(asc feature)]]
                             name)))
         (nconc (--map (list it (cadr (assoc it drop)) nil)
                       provided)
                (-keep (-lambda ((feature reason))
                         (unless (memq feature provided)
                           (push feature provided)
                           (list feature nil reason)))
                       join)))))))

(defun emir--builtin-packages-alist ()
  (let ((default-directory emir-emacs-repository))
    (mapcar
     (lambda (elt)
       (cons (car elt) (mapcar #'cdr (cdr elt))))
     (-group-by
      #'car
      (cl-sort
       (cl-mapcan
        (lambda (file)
          (message "Importing %s..." file)
          (and (string-suffix-p ".el" file)
               (not (string-match-p finder-no-scan-regexp file))
               (not (member file
                            '("lisp/gnus/.dir-locals.el"
                              ;; Old versions:
                              "lisp/obsolete/old-emacs-lock.el"
                              "lisp/obsolete/old-whitespace.el"
                              "lisp/obsolete/otodo-mode.el"
                              ;; Moved to GNU Elpa:
                              "lisp/obsolete/crisp.el"
                              "lisp/obsolete/landmark.el")))
               (with-temp-buffer
                 (insert-file-contents file)
                 (let ((package
                        (cond
                         ((not features) "emacs")
                         ((string-prefix-p "lisp/term/"     file) "emacs")
                         ((string-prefix-p "lisp/leim/"     file) "emacs")
                         ((string-prefix-p "lisp/obsolete/" file) "emacs")
                         ((lm-header "Package"))
                         ((--when-let (assoc (-> file
                                                 file-name-directory
                                                 directory-file-name
                                                 file-name-nondirectory)
                                             finder--builtins-alist)
                            (symbol-name (cdr it))))
                         ((-> file
                              file-name-nondirectory
                              file-name-sans-extension)))))
                   (--map (list package file it)
                          (or (packed-provided)
                              (list nil)))))))
        (magit-git-items "ls-tree" "-z" "-r" "--name-only" "HEAD" "lisp/"))
       #'string< :key #'car)))))

;;; Github

(cl-defmethod emir-gh ((pkg epkg-package)
                       method resource
                       &optional params
                       &key callback errorback)
  (setq resource
        (format-spec resource
                     (format-spec-make
                      ?u (oref pkg upstream-user)
                      ?n (oref pkg upstream-name)
                      ?o (if (epkg-shelved-package-p pkg)
                             "emacsattic"
                           "emacsmirror")
                      ?m (oref pkg mirror-name))))
  (let ((url-show-status nil))
    (if (equal method "WAIT")
        (ghub-wait resource nil :auth 'emir)
      (ghub-request method resource params
                    :silent t
                    :auth 'emir
                    :callback callback
                    :errorback errorback))))

(cl-defmethod emir-gh-init ((pkg epkg-package))
  (emir-gh pkg "POST" "/orgs/%o/repos" `((name . ,(oref pkg mirror-name))))
  (emir-gh pkg "WAIT" "/repos/%o/%m"))

(cl-defmethod emir-gh-init ((pkg epkg-github-package))
  (if (cl-flet ((forked
                 (rsp key)
                 (--when-let (cdr (assq 'full_name (cdr (assq key rsp))))
                   (cl-find-if
                    (lambda (fork)
                      (equal (cdr (assq 'login (cdr (assq 'owner fork))))
                             "emacsmirror"))
                    (emir-gh pkg "GET" (format "/repos/%s/forks" it))))))
        (let ((rsp (emir-gh pkg "GET" "/repos/%u/%n")))
          (or (forked rsp 'source)
              (forked rsp 'parent))))
      (cl-call-next-method)
    (emir-gh pkg "POST" "/repos/%u/%n/forks" '((organization . "emacsmirror")))
    (emir-gh pkg "WAIT" "/repos/%o/%n")
    (unless (equal (oref pkg mirror-name)
                   (oref pkg upstream-name))
      (emir-gh pkg "PATCH" "/repos/%o/%n" `((name . ,(oref pkg mirror-name)))))))

(cl-defmethod emir-gh-unsubscribe :after ((pkg epkg-package))
  (emir-gh pkg "DELETE" "/repos/%o/%m/subscription"
           :errorback (lambda (err &rest _)
                        (message "Error: Failed to unsubscribe from %s/%s: %S"
                                 (oref pkg upstream-user)
                                 (oref pkg upstream-name) err))))

(cl-defmethod emir-gh-update ((pkg epkg-package) &optional _clone)
  (emir-gh pkg "PATCH" "/repos/%o/%m"
           `((name           . ,(oref pkg mirror-name))
             (description    . ,(oref pkg summary))
             (homepage       . ,(oref pkg homepage))
             (has_issues     . nil)
             (has_wiki       . nil)
             (has_downloads  . nil))
           :errorback (lambda (err &rest _)
                        (message "Error: Failed to publish metadata for %s: %S"
                                 (oref pkg mirror-name) err))))

(cl-defmethod emir-gh-update :after ((pkg epkg-github-package) &optional clone)
  (when clone
    (with-emir-repository pkg
      (--when-let (delete "master" (magit-list-remote-branches "mirror"))
        (magit-git "push" "mirror" (--map (concat ":" it) it))))))

(cl-defmethod emir-gh-delete ((pkg epkg-package))
  (emir-gh pkg "DELETE" "/repos/%o/%m"))

(defun emir-gh-foreach-query (query callback)
  (let* ((page 0)
         (result nil)
         (groups
          (-partition-all
           100 ; Should be small enough to prevent timeout.
           (mapcar (lambda (pkg)
                     ;; Package names may contain characters that are
                     ;; invalid here.  Identifiers also may not begin
                     ;; with a number or contain an equal sign.
                     `((,(concat "_"
                                 (replace-regexp-in-string
                                  "=" "_"
                                  (base64-encode-string (oref pkg name))))
                        repository)
                       [(owner ,(oref pkg upstream-user))
                        (name  ,(oref pkg upstream-name))]
                       ,@(if (functionp query)
                             (funcall query pkg)
                           query)))
                   (epkgs nil 'epkg-github-package--eieio-childp))))
         (length (length groups)))
    (cl-labels
        ((cb (&optional data _headers _status _req)
             (setq result (nconc result (cdar data)))
             (cond
              (groups
               (message "Fetching page %s/%s..." (cl-incf page) length)
               (ghub-graphql
                (gsexp-encode
                 (ghub--graphql-prepare-query
                  (cons 'query (pop groups))))
                nil :callback #'cb :auth 'emir))
              (t
               (message "Fetching page %s/%s...done" page length)
               (dolist (elt result)
                 (setcar elt (base64-decode-string
                              (replace-regexp-in-string
                               "_" "=" (substring (symbol-name (car elt)) 1)))))
               (funcall callback result)))))
      (cb))))

;;; Urls

(cl-defmethod emir--set-urls ((pkg epkg-mirrored-package))
  (-if-let (url (oref pkg url))
      (progn
        (let ((conflict (and url (cadr (assoc url (epkgs [url name]))))))
          (when (and conflict (not (equal conflict (oref pkg name))))
            (user-error "Another package, %s, is already mirrored from %s"
                        conflict url)))
        (-when-let (url-format (oref pkg url-format))
          (pcase-dolist (`(,slot . ,value) (emir--match-url url-format url))
            (eieio-oset pkg slot value))))
    (oset pkg url (oref-default pkg url-format))))

(cl-defmethod emir--set-urls :after ((pkg epkg-hg-package))
  (let ((url (oref pkg url)))
    (unless (string-prefix-p "hg::" url)
      (oset pkg url (concat "hg::" url)))))

(cl-defmethod emir--set-urls ((_pkg epkg-elpa-core-package))
  ) ; Noop.  May be multiple urls.  See `emir-add-gelpa-packages'.

(cl-defmethod emir--format-url ((pkg epkg-package) slot)
  (--when-let (eieio-oref-default pkg slot)
    (format-spec it `((?m . ,(oref pkg mirror-name))
                      (?n . ,(oref pkg upstream-name))
                      (?u . ,(oref pkg upstream-user))
                      (?l . ,(oref pkg library))))))

(defun emir--match-url (format url)
  (with-temp-buffer
    (insert (regexp-quote format))
    (goto-char (point-min))
    (let (slots)
      (save-match-data
        (while (re-search-forward "%\\(.\\)" nil t)
          (push (cdr (assq (string-to-char (match-string 1))
                           '((?m . mirror-name)
                             (?n . upstream-name)
                             (?u . upstream-user))))
                slots)
          (replace-match "\\([^/]+\\)" t t))
        (and (string-match (concat "^" (buffer-string) "$") url)
             (let ((i 0))
               (--map (cons it (match-string (cl-incf i) url))
                      (nreverse slots))))))))

(defun emir--url-to-class (url)
  (--first (ignore-errors (emir--match-url (oref-default it url-format) url))
           (closql--list-subclasses 'epkg-package)))

(defun emir--url-get (url slot)
  (-when-let* ((class (emir--url-to-class url))
               (slots (emir--match-url (oref-default class url-format) url)))
    (cdr (assoc slot slots))))

(defun emir--lookup-url (url)
  (caar (epkg-sql [:select name :from packages :where (= url $s1)] url)))

(defvar emir-url-history nil)

(defun emir-read-url (prompt)
  (magit-read-string prompt nil 'emir-url-history nil nil t))

(defun emir--normalize-wikipage (string)
  (->> string
    (replace-regexp-in-string "\\+" "plus")
    (replace-regexp-in-string "-" "")
    downcase))

;;; _
(provide 'emir)
(require 'emir-gelpa)
(require 'emir-melpa)
(require 'emir-utils)
;;; emir.el ends here
