;;; emir.el --- maintain the Emacsmirror          -*- lexical-binding: t -*-

;; Copyright (C) 2016-2017  Jonas Bernoulli

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
(require 'epkg-util)
(require 'finder)
(require 'ghub)
(require 'magit)
(require 'org)
(require 'packed)
(require 'subr-x)

(defvar finder-no-scan-regexp)
(defvar finder--builtins-alist)
(declare-function org-publish 'ox-publish)

(cl-pushnew (expand-file-name
             "bin" (file-name-directory (or load-file-name buffer-file-name)))
            exec-path :test #'equal)

;;; Options

(defgroup emir nil
  "Maintain the Emacsmirror."
  :group 'local)

(defcustom emir-emacs-reference "emacs-25.2"
  "The Emacs reference used to extract builtin packages."
  :group 'emir
  :type 'string)

(defcustom emir-pending-packages nil
  "List of packages that might eventually be imported.
These package only will be imported if and when upstream
has fixed known outstanding issues."
  :group 'emir
  :type '(repeat (list (string :tag "Name")
                       (string :tag "Reason"))))

(defcustom emir-secondary-packages nil
  "List of packages that share a repository with another package.
In most cases this is detected automatically.  This variable
should only be used if the Emacsmirror and Melpa import from
a different repository."
  :group 'emir
  :type '(repeat (list (string :tag "Secondary package")
                       (string :tag "Primary package"))))

(defcustom emir-suspended-packages nil
  "List of packages that are temporarily not being updated."
  :group 'emir
  :type '(repeat (list (string :tag "Name")
                       (string :tag "Reason"))))

(defcustom emir-renamed-files nil
  "List of files that have to be renamed after fetching with curl.
This variable should only be used as a last resort."
  :group 'emir
  :type '(repeat (list (string :tag "Name")
                       (string :tag "Filename"))))

;;; Repositories

(defconst emir-emacs-repository "~/git/src/emacs/")
(defconst emir-gelpa-repository "~/git/emacs/gelpa/")
(defconst emir-melpa-repository "~/git/emacs/melpa/")
(defconst emir-ewiki-repository "~/git/emacs/ewiki/")
(defconst emir-stats-repository "~/git/emacs/stats/")

(defmacro with-epkg-repository (arg &rest body)
  (declare (indent defun))
  `(let ((default-directory
           ,(if (eq arg t)
                'epkg-repository
              `(or (epkg-repository ,arg)
                   (error "Need package or string")))))
     ,@body))

(cl-defmethod epkg-repository ((pkg epkg-mirrored-package))
  (expand-file-name (format "mirror/%s/" (oref pkg name)) epkg-repository))

(cl-defmethod epkg-repository ((pkg epkg-shelved-package))
  (expand-file-name (format "attic/%s/" (oref pkg name)) epkg-repository))

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
  (let ((default-directory emir-emacs-repository)
        (alist (emir--builtin-packages-alist)))
    (magit-git "checkout" emir-emacs-reference)
    (pcase-dolist (`(,name . ,value) alist)
      (oset (or (epkg name)
                (emir-add (epkg-builtin-package :name name)))
            builtin-libraries value))
    (dolist (pkg (epkgs))
      (when (and (oref pkg builtin-libraries)
                 (not (assoc (oref pkg name) alist)))
        (message "Deleting %s..." (oref pkg name))
        (if (epkg-builtin-package-p pkg)
            (closql-delete pkg)
          (oset pkg builtin-libraries nil))))))

;;;###autoload
(defun emir-import-ewiki-packages (&optional drew-only)
  (interactive "p")
  (with-epkg-repository 'epkg-wiki-package
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
  (--if-let (epkg name)
      (user-error "Package %s already exists" name)
    (when (assoc name emir-pending-packages)
      (user-error "Package %s is on hold" name)))
  (let ((pkg (apply class :name name :url url plist)))
    (emir-add pkg)
    (with-epkg-repository t
      (borg--sort-submodule-sections (magit-git-dir "config"))
      (borg--sort-submodule-sections ".gitmodules")
      (magit-call-git "add" "epkg.sqlite" ".gitmodules"
                      (epkg-repository pkg)))))

;;;###autoload
(defun emir-add-gelpa-packages (&optional dry-run)
  (interactive "P")
  (emir-pull 'epkg-elpa-package)
  (pcase-dolist (`(,name ,class)
                 (gelpa-recipes [name class]
                                '(gelpa-subtree-recipe
                                  gelpa-external-recipe)))
    (unless (or (epkg name)
                (assoc name emir-pending-packages))
      (message "Adding %s..." name)
      (unless dry-run
        (emir-add (cl-case class
                    (subtree  (epkg-elpa-package        :name name))
                    (external (epkg-elpa-branch-package :name name))))
        (oset (gelpa-get name) epkg-package name))
      (message "Adding %s...done" name)))
  (unless dry-run
    (emir--commit "add")))

;;;###autoload
(defun emir-add-melpa-packages (&optional dry-run)
  (interactive "P")
  (let ((mirrored (epkgs 'url)))
    (pcase-dolist (`(,name ,class ,url ,branch)
                   (melpa-recipes [name class url branch]))
      (unless (or (epkg name)
                  (memq class '(bzr cvs darcs fossil svn))
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

(defvar emir-failed-updates nil)

;;;###autoload
(defun emir-update-package (package &optional force)
  (interactive (list (epkg-read-package "Update package: ")
                     current-prefix-arg))
  (let* ((pkg (epkg package))
         (tip (oref pkg hash)))
    (condition-case err
        (with-epkg-repository pkg
          (when (cl-typep pkg 'epkg-mirrored-package)
            (emir-pull pkg))
          (emir-update pkg)
          (unless (epkg-builtin-package-p pkg)
            (with-epkg-repository t
              (magit-call-git "add" (epkg-repository pkg))))
          (when (or force (not (equal (oref pkg hash) tip)))
            (emir-gh-update pkg)
            (emir-push pkg)))
      (error
       (push (oref pkg name) emir-failed-updates)
       (message "Update error: %s" (error-message-string err))))))

;;;###autoload
(defun emir-update-packages (&optional from recreate)
  (interactive (list (and current-prefix-arg
                          (epkg-read-package "Limit to packages after: "))))
  (dolist (name (epkgs 'name 'epkg-mirrored-package--eieio-childp))
    (when (or (not from) (string< from name))
      (if (assoc name emir-suspended-packages)
          (message "Skipping suspended %s" name)
        (message "Updating %s..." name)
        (if recreate
            (emir-update (epkg name) t)
          (emir-update-package name))
        (message "Updating %s...done" name))))
  (emir--commit "update"))

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
          (feature  (intern (completing-read "Drop provide: " features nil t)))
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
          (feature  (intern (completing-read "Drop required: " features nil t)))
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
    (with-epkg-repository pkg
      (magit-git "reset" "--hard" "HEAD"))
    (with-epkg-repository t
      (magit-git "mv"
                 (concat "mirror/" name)
                 (concat "attic/" name)))
    (closql--set-object-class (epkg-db) pkg 'epkg-shelved-package)
    (oset pkg mirror-url (emir--format-url pkg 'mirror-url-format))
    (oset pkg mirrorpage (emir--format-url pkg 'mirrorpage-format))
    (with-epkg-repository pkg
      (magit-git "remote" "rename" "mirror" "attic")
      (magit-git "remote" "set-url" "attic" (oref pkg mirror-url)))
    (with-epkg-repository t
      (magit-git "config" "-f" ".gitmodules"
                 (concat "submodule." name ".url")
                 (oref pkg mirror-url))
      (magit-git "add" ".gitmodules" "epkg.sqlite"))
    (emir-update  pkg)
    (emir-gh-init pkg)
    (emir-push    pkg)))

;;;; Remove

;;;###autoload
(defun emir-remove-package (name)
  (interactive (list (epkg-read-package "Remove package: ")))
  (let ((pkg (epkg name)))
    (unless (epkg-builtin-package-p pkg)
      (with-epkg-repository t
        (magit-git "add" ".gitmodules")
        (let ((module-dir (epkg-repository pkg)))
          (when (file-exists-p module-dir)
            (magit-git "rm" "-f" module-dir))))
      (with-demoted-errors "Error: %S"
        (emir-gh-delete pkg)))
    (closql-delete pkg)
    (with-epkg-repository t
      (magit-call-git "add" "epkg.sqlite"))
    (when (epkg-wiki-package-p pkg)
      (with-epkg-repository 'epkg-wiki-package
        (magit-call-git "branch" "-D" name)))
    (with-demoted-errors "Error: %S"
      (with-epkg-repository t
        (delete-directory (magit-git-dir (concat "modules/" name)) t)))))

;;;; Convenience

;;;###autoload
(defun emir-find-file (filename &optional wildcards)
  (interactive (emir-find-file-read-args "Find file: "))
  (emir-find-file-noselect filename #'switch-to-buffer wildcards))

;;;###autoload
(defun emir-find-file-other-window (filename &optional wildcards)
  (interactive (emir-find-file-read-args "Find file in other window: "))
  (emir-find-file-noselect filename #'switch-to-buffer-other-window wildcards))

;;;###autoload
(defun emir-find-file-other-frame (filename &optional wildcards)
  (interactive (emir-find-file-read-args "Find file in other frame: "))
  (emir-find-file-noselect filename #'switch-to-buffer-other-frame wildcards))

(defun emir-find-file-read-args (prompt)
  (with-epkg-repository (epkg (epkg-read-package "Find file of package: "))
    (find-file-read-args prompt (confirm-nonexistent-file-or-buffer))))

(defun emir-find-file-noselect (filename:s switch &optional wildcards)
  (let ((value (find-file-noselect filename:s nil nil wildcards)))
    (if (listp value)
	(let ((buffers (nreverse value)))
	  (funcall switch (car buffers))
	  (mapc #'switch-to-buffer (cdr buffers))
	  buffers)
      (funcall switch value))))

;;;###autoload
(defun emir-find-org-file (name)
  (interactive
   (list (completing-read "Find org file: "
                          '("maint" "config"
                            "compare" "emacsorphanage" "issues"
                            "kludges" "melpa-missing" "notes"))))
  (find-file-other-frame
   (cond ((equal name "maint")
          "~/.emacs.d/lib/emir/emir.org")
         ((equal name "config")
          (expand-file-name "emir.org" epkg-repository))
         (t
          (expand-file-name (concat name ".org") emir-stats-repository)))))

;;;###autoload
(defun emir-describe-package (package)
  (interactive
   (list (epkg-read-package "Describe package: "
                            (or (tabulated-list-get-id)
                                (--when-let (symbol-at-point)
                                  (symbol-name it))))))
  (help-setup-xref (list #'emir-describe-package package)
                   (called-interactively-p 'interactive))
  (with-help-window (help-buffer)
    (with-current-buffer standard-output
      (let ((epkg-describe-package-slots-width 14))
        (epkg-describe-package-1
         (epkg package)
         '(type
           hash
           url
           mirror-url
           mirror-name
           upstream-user
           upstream-name
           upstream-branch
           upstream-tree
           library
           epkg-insert-repopage
           epkg-insert-homepage
           epkg-insert-mirrorpage
           epkg-insert-wikipage
           license
           created
           updated
           summary
           libraries
           provided
           required
           epkg-insert-keywords
           epkg-insert-authors
           epkg-insert-maintainers
           epkg-insert-commentary
           epkg-insert-dependencies
           epkg-insert-reverse-dependencies))))))

;;;###autoload
(defun emir-generate-reports ()
  (interactive)
  (let ((org-confirm-babel-evaluate nil))
    (org-publish
     `("emir"
       :base-extension      "org"
       :base-directory      ,emir-stats-repository
       :publishing-function org-html-publish-to-html)
     t)))

;;; Git
;;;; Import

(cl-defmethod emir-import ((pkg epkg-wiki-package))
  (with-epkg-repository 'epkg-wiki-package
    (with-slots (name) pkg
      (message "Importing %s..." name)
      (magit-git "filter-emacswiki" "--tag" "--notes" name)
      (message "Importing %s...done" name))))

(cl-defmethod emir-import ((pkg epkg-elpa-package))
  (with-epkg-repository 'epkg-elpa-package
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
  (with-epkg-repository t
    (let* ((name  (oref pkg name))
           (repo  (cl-typecase pkg
                    (epkg-wiki-package
                     (file-relative-name emir-ewiki-repository))
                    (epkg-elpa-package
                     (file-relative-name emir-gelpa-repository))
                    (epkg-elpa-branch-package
                     (file-relative-name emir-gelpa-repository))
                    (t
                     (oref pkg url))))
           (path  (concat "mirror/" name)))
      (magit-git
       "clone"
       "--single-branch"
       "--branch" (cl-typecase pkg
                    (epkg-wiki-package        name)
                    (epkg-elpa-package        (concat "directory/" name))
                    (epkg-elpa-branch-package (concat "externals/" name))
                    (t (or (oref pkg upstream-branch) "master")))
       "--origin" (cl-typecase pkg
                    (epkg-subset-package  "import")
                    (t                    "origin"))
       repo
       path)
      (magit-git "submodule" "add" "--name" name repo path)
      (magit-git "submodule" "absorbgitdirs" path)
      (magit-git "config" "-f" ".gitmodules"
                 (concat "submodule." name ".url")
                 (oref pkg mirror-url))))
  (with-epkg-repository pkg
    (magit-git "remote" "add" "mirror" (oref pkg mirror-url))))

(cl-defmethod emir-clone :after ((pkg epkg-subtree-package))
  (with-epkg-repository pkg
    (magit-git "branch" "--unset-upstream" "master"))
  (emir-pull pkg))

(cl-defmethod emir-clone ((pkg epkg-file-package))
  (let* ((name (oref pkg name))
         (repo (oref pkg mirror-url))
         (path (concat "mirror/" name)))
    (with-epkg-repository t
      (magit-git "init" path))
    (emir-pull pkg t)
    (with-epkg-repository t
      (magit-git "submodule" "add" "--name" name repo path))
    (with-epkg-repository pkg
      (magit-git "remote" "add" "mirror" repo)
      (magit-git "config" "branch.master.remote" "mirror")
      (magit-git "config" "branch.master.merge" "refs/heads/master"))))

;;;; Pull

(cl-defmethod emir-pull ((pkg epkg-mirrored-package))
  (with-epkg-repository pkg
    (magit-git "pull" "--ff-only" "origin")))

(cl-defmethod emir-pull ((pkg epkg-file-package) &optional force)
  (with-epkg-repository pkg
    (let ((name (oref pkg name)))
      (let ((magit-process-raise-error t))
        (magit-call-process "curl" "-O" (oref pkg url))
        (--when-let (cadr (assoc name emir-renamed-files))
          (rename-file it (concat name ".el") t)))
      (when (or (magit-anything-unstaged-p) force)
        (magit-git "add" ".")
        (let ((process-environment process-environment)
              (mainlib (or (oref pkg library)
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
  (with-epkg-repository pkg
    (magit-git "fetch"    "origin")
    (magit-git "checkout" "origin/master")
    (message "Filtering subtree...")
    (magit-git "branch" "-f" "master"
               (magit-git-string "subtree" "-P"
                                 (oref pkg upstream-tree) "split"))
    (message "Filtering subtree...done")
    (magit-git "checkout" "master")))

(cl-defmethod emir-pull ((pkg epkg-subset-package))
  (with-epkg-repository pkg
    (magit-git "pull" "--ff-only" "import")))

(cl-defmethod emir-pull ((pkg epkg-elpa-branch-package))
  (with-epkg-repository pkg
    (magit-git "pull" "--ff-only" "import"
               (concat "externals/" (oref pkg name)))))

(cl-defmethod emir-pull ((class (subclass epkg-elpa-package)))
  (with-epkg-repository class
    (magit-git "checkout" "master")
    (magit-git "pull" "--ff-only" "origin")))

;;;; Push

(cl-defmethod emir-push ((pkg epkg-mirrored-package))
  (with-epkg-repository pkg
    (magit-git "push" "mirror")))

(cl-defmethod emir-push ((pkg epkg-subset-package))
  (with-epkg-repository (eieio-object-class pkg)
    (magit-git "push" (oref pkg mirror-url)
               (format (cl-typecase pkg
                         (epkg-wiki-package                  "%s:master")
                         (epkg-elpa-package        "directory/%s:master")
                         (epkg-elpa-branch-package "externals/%s:master"))
                       (oref pkg name)))))

(cl-defmethod emir-push ((pkg epkg-shelved-package))
  (with-epkg-repository pkg
    (magit-git "push" "attic")))

;;;; Commit

(defun emir--commit (verb)
  (with-epkg-repository t
    (let ((count (length (magit-staged-files nil "mirror"))))
      (when (> count 0)
        (magit-git "commit"
                   "-m" (format "%s %s %s" verb count
                                (if (> count 1) "packages" "package"))
                   "-i" ".gitmodules" "epkg.sqlite")))))

;;; Database
;;;; Add

(cl-defmethod emir-add ((pkg epkg-mirrored-package))
  (if-let (url (oref pkg url))
      (progn
        (when-let (conflict (and url (cadr (assoc url (epkgs [url name])))))
          (user-error "Another package, %s, is already mirrored from %s"
                      conflict url))
        (when-let (url-format (oref pkg url-format))
          (pcase-dolist (`(,slot . ,value) (emir--match-url url-format url))
            (eieio-oset pkg slot value))))
    (oset pkg url (oref-default pkg url-format)))
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
  (oset pkg repopage (emir--format-url pkg 'repopage-format))
  (oset pkg homepage (emir--format-url pkg 'homepage-format))
  (closql-insert (epkg-db) pkg)
  (emir-update pkg))

;;;; Update

(cl-defmethod emir-update ((pkg epkg-package) &optional recreate)
  (with-epkg-repository pkg
    (with-slots (name hash libraries library) pkg
      (unless recreate
        (setf hash (magit-rev-parse "HEAD"))
        (when (epkg-builtin-package-p pkg)
          (setf libraries
                (mapcar #'car (epkg-sql [:select library
                                         :from builtin-libraries
                                         :where (= name $s1)]
                                        name)))
          (unless (equal name "emacs")
            (setf library
                  (or (let ((main (concat "/" name ".el")))
                        (--first (string-suffix-p main it) libraries))
                      library)))))
      (--if-let (or library
                    (ignore-errors
                      (let ((load-suffixes '(".el" ".el.in" ".el.tmpl"))
                            (load-file-rep-suffixes '("")))
                        (packed-main-library default-directory name nil t))))
          (with-temp-buffer
            (insert-file-contents it)
            (oset pkg summary     (elx-summary nil t))
            (oset pkg keywords    (elx-keywords-list nil t t))
            (oset pkg license     (elx-license))
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
    (--when-let (magit-mode-get-buffer 'magit-process-mode)
      (kill-buffer it))))

;;; Extract

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
                                   :where [(= package $s1)]]
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
  (with-epkg-repository pkg
    (let ((enable-local-variables nil)
          (load-suffixes '(".el" ".el.in" ".el.tmpl"))
          (load-file-rep-suffixes '(""))
          (name (oref pkg name))
          provided hard soft)
      (dolist (lib (if (epkg-builtin-package-p pkg)
                       (oref pkg libraries)
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
      #'string< :key #'car))))

;;; Github

(cl-defmethod emir-gh-init ((pkg epkg-package))
  (let ((org  (if (epkg-shelved-package-p pkg) "emacsattic" "emacsmirror"))
        (name (oref pkg mirror-name)))
    (ghub-post (format "/orgs/%s/repos" org) nil `((name . ,name)))
    (ghub-wait (format "/repos/%s/%s" org name))))

(cl-defmethod emir-gh-init ((pkg epkg-github-package))
  (with-slots (mirror-name upstream-user upstream-name) pkg
    (ghub-post (format "/repos/%s/%s/forks" upstream-user upstream-name)
               nil '((organization . "emacsmirror")))
    (ghub-wait (format "/repos/emacsmirror/%s" upstream-name))
    (unless (equal mirror-name upstream-name)
      (ghub-patch (format "/repos/emacsmirror/%s" upstream-name)
                  nil `((name . ,mirror-name))))))

(cl-defmethod emir-gh-unsubscribe :after ((pkg epkg-package))
  (let ((org  (if (epkg-shelved-package-p pkg) "emacsattic" "emacsmirror"))
        (name (oref pkg mirror-name)))
    (with-demoted-errors
        (format "Failed to unsubscribe from %s/%s: %%S" org name)
      (ghub-delete (format "/repos/%s/%s/subscription" org name)))))

(cl-defmethod emir-gh-update ((pkg epkg-package) &optional _clone)
  (let ((org  (if (epkg-shelved-package-p pkg) "emacsattic" "emacsmirror"))
        (name (oref pkg mirror-name)))
    (with-demoted-errors
        (format "Failed to update metadata for %s/%s: %%S" org name)
      (ghub-patch (format "/repos/%s/%s" org name)
                  nil `((name           . ,name)
                        (description    . ,(oref pkg summary))
                        (homepage       . ,(oref pkg homepage))
                        (has_issues     . nil)
                        (has_wiki       . nil)
                        (has_downloads  . nil))))))

(cl-defmethod emir-gh-update :after ((pkg epkg-github-package) &optional clone)
  (when clone
    (with-epkg-repository pkg
      (--when-let (delete "master" (magit-list-remote-branches "mirror"))
        (magit-git "push" "mirror" (--map (concat ":" it) it))))))

(cl-defmethod emir-gh-delete ((pkg epkg-package))
  (ghub-delete (format "/repos/%s/%s"
                       (if (epkg-shelved-package-p pkg)
                           "emacsattic"
                         "emacsmirror")
                       (oref pkg mirror-name))))

;;; Urls

(cl-defmethod emir--format-url ((pkg epkg-package) slot)
  (--when-let (eieio-oref-default pkg slot)
    (format-spec it `((?m . ,(oref pkg mirror-name))
                      (?n . ,(oref pkg upstream-name))
                      (?u . ,(oref pkg upstream-user))))))

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

(provide 'emir)
(require 'emir-gelpa)
(require 'emir-melpa)
;;; emir.el ends here
