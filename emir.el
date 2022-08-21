;;; emir.el --- Maintain the Emacsmirror  -*- lexical-binding:t -*-

;; Copyright (C) 2016-2022 Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: https://github.com/emacscollective/emir
;; Keywords: local

;; Package-Requires: ((emacs "28.1"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is the package I use to maintain the Emacsmirror.  It isn't
;; very useful for anybody else.  Epkg is the user client which you
;; should be using instead.

;;; Code:

(require 'borg)
(require 'cl-lib)
(require 'closql)
(require 'eieio)
(require 'elx)
(require 'emacsql-sqlite)
(require 'epkg)
(require 'epkg-org)
(require 'epkg-utils)
(require 'f)
(require 'finder)
(require 'ghub)
(require 'l)
(require 'magit)
(require 'org)
(require 'packed)
(require 'subr-x)
(require 'transient)

(declare-function emir-melpa--migrated-packages "emir-melpa" ())

(defvar finder-no-scan-regexp)
(defvar finder--builtins-alist)
(declare-function org-publish "ox-publish" (project &optional force async))

(defconst emir-github-token-scopes '(repo delete_repo))

(cl-pushnew (expand-file-name
             "bin" (file-name-directory (or load-file-name buffer-file-name)))
            exec-path :test #'equal)

(setq epkg--db-prefer-binary t)

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

(defvar emir-kept-packages nil
  "List of packages that we keep mirrored despite them being archived.")

(defvar emir-suspended-packages nil
  "List of packages that are temporarily not being updated.")

(defvar emir--homepage-alist nil
  "Alist of packages and their homepages.")

(defvar emir--wikipage-alist nil
  "Alist of packages and their wikipages.")

(defvar emir--archived-packages nil
  "List of packages whose upstream repositories are archived.")

(defvar emir--moved-packages nil
  "List of packages whose upstream repositories were moved or removed.")

;;; Repositories

(defconst emir-emacs-repository "~/git/src/emacs/emacsmirror/")
(defconst emir-gelpa-repository (expand-file-name "gelpa/" epkg-repository))
(defconst emir-melpa-repository (expand-file-name "melpa/" epkg-repository))
(defconst emir-ewiki-repository (expand-file-name "ewiki/" epkg-repository))
(defconst emir-stats-repository (expand-file-name "stats/" epkg-repository))

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
(cl-defmethod epkg-repository ((_class (subclass epkg-gnu-elpa-package)))
  emir-gelpa-repository)
(cl-defmethod epkg-repository ((_class (subclass epkg-wiki-package)))
  emir-ewiki-repository)
(cl-defmethod epkg-repository ((_class (subclass epkg-builtin-package)))
  emir-emacs-repository)

;;; Commands
;;;; Import

;;;###autoload (autoload 'emir-import-recipes "emir" nil t)
(transient-define-prefix emir-import-recipes ()
  "Import Gelpa or Melpa recipes."
  :value '("--fetch")
  ["Arguments"
   ("a" "Re-import all recipes" "--all")
   ("f" "Fetch before import"   "--fetch")]
  ["Import"
   ("m" "Melpa" emir-import-melpa-recipes)
   ("g" "Gelpa" emir-import-gelpa-recipes)])

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
          (if value
              (emir-update pkg)
            (message "  ! nothing left")))
        (message "Updating %s...done" name))
      (dolist (pkg (epkgs))
        (let ((name (oref pkg name)))
          (when (and (oref pkg builtin-libraries)
                     (not (assoc name alist)))
            (message "Deleting %s..." name)
            (if (epkg-builtin-package-p pkg)
                (closql-delete pkg)
              (oset pkg builtin-libraries nil))
            (message "Deleting %s...done" name)))))
    (emir-commit "Update built-in packages" nil :dump)))

;;;###autoload
(defun emir-import-ewiki-packages (&optional drew-only)
  (interactive "P")
  (with-emir-repository 'epkg-wiki-package
    (magit-git "checkout" "master")
    (magit-git "pull" "--ff-only" "origin")
    (magit-process-buffer)
    (if drew-only
        (pcase-dolist (`(,name)
                       (epkg-sql [:select :distinct [packages:name]
                                  :from [packages authors]
                                  :where (and (= packages:name authors:package)
                                              (= packages:class 'wiki)
                                              (= authors:name "Drew Adams"))]))
          (emir-import (epkg-wiki-package :name name)))
      (message "Importing wiki packages asynchronously...")
      (with-environment-variables
          (("PATH" (format "%sbin:%s"
                           (file-name-directory (locate-library "emir.el"))
                           (getenv "PATH"))))
        (magit-run-git-async "filter-emacswiki" "--tag" "--notes"))))
  (emir-commit "Update Emacswiki module"))

;;;; Add

;;;###autoload
(defun emir-add-package (name url class &rest plist)
  (interactive
   (let* ((url (emir-read-url "Add package from url"))
          (class (emir--read-class url))
          (name (magit-read-string
                 "Package name"
                 (and-let* ((name (emir--url-get url 'upstream-name)))
                   (thread-last name
                     (replace-regexp-in-string "\\`emacs-" "")
                     (replace-regexp-in-string "\\`elisp-" "")
                     (replace-regexp-in-string "[.-]el\\'" ""))))))
     (list name url class)))
  (cond ((epkg name)
         (user-error "Package %s already exists" name))
        ((assoc name emir-pending-packages)
         (user-error "Package %s is on hold" name)))
  (emir-add (apply class :name name :url url plist))
  (when-let ((recipe (melpa-get name)))
    (oset recipe epkg-package name))
  (when-let ((recipe (gelpa-get name)))
    (oset recipe epkg-package name))
  (emir-commit (format "Add %S package" name) name :dump :sort))

;;;###autoload
(defun emir-add-gelpa-packages (&optional dry-run)
  (interactive "P")
  (magit-git "fetch" "origin")
  (pcase-dolist (`(,name ,url ,class)
                 (gelpa-recipes [name url class]))
    (let ((pkg (epkg name)))
      (when (and (not (assoc name emir-pending-packages))
                 (not (assoc name emir-secondary-packages))
                 (or (not pkg)
                     (and (epkg-builtin-package-p pkg)
                          (eq class 'core)
                          (or dry-run
                              (y-or-n-p (format "\
%s is already being tracked as an `epkg-builtin-package'.
Mirror as an `epkg-core-package' instead? " name))))))
        (message "Adding %s..." name)
        (unless dry-run
          (let ((libs (and (epkg-builtin-package-p pkg)
                           (oref pkg builtin-libraries))))
            (when libs
              (closql-delete pkg))
            (cl-ecase class
              (external
               (let ((class (emir--read-class
                             url "gnu-elpa"
                             (format "Package type for %s from %s: "
                                     name url))))
                 (setq pkg (if (eq class 'epkg-gnu-elpa-package)
                               (epkg-gnu-elpa-package :name name)
                             (funcall class :name name :url url)))))
              (core (setq pkg (epkg-core-package :name name))
                    (oset pkg url (emir--format-url pkg 'url-format))))
            (emir-add pkg)
            (when-let ((u (emir--format-url pkg 'url-format)))
              (oset pkg url u))
            (when libs
              (oset pkg builtin-libraries libs)))
          (oset (gelpa-get name) epkg-package name)
          (emir-commit (format "Add %S package" name) name :dump :sort))
        (message "Adding %s...done" name)))))

;;;###autoload
(defun emir-add-melpa-packages (&optional dry-run)
  (interactive "P")
  (pcase-dolist (`(,name ,class ,url ,branch)
                 (melpa-recipes [name class url branch]))
    (unless (or (epkg name)
                (emir--lookup-url url)
                (assoc name emir-pending-packages)
                (assoc name emir-secondary-packages))
      (message "Adding %s..." name)
      (unless dry-run
        (apply #'emir-add-package name url
               (or (emir--url-to-class url)
                   (intern (format "epkg-%s-package" class)))
               (and branch (list :upstream-branch branch))))
      (message "Adding %s...done" name))))

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
          (when (or force (not (equal (oref pkg hash) tip)))
            (unless (epkg-builtin-package-p pkg)
              (emir-stage name :dump))
            (emir-gh-update pkg)
            (emir-push pkg)))
      (error
       (let ((name (oref pkg name)))
         (push name emir-failed)
         (message "Update error (%s): %s" name (error-message-string err)))))))

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
     (emir-commit (emir--update-message) nil :dump))
   50))

;;;###autoload
(defun emir-update-wiki-packages (&optional from recreate)
  (interactive (list (and current-prefix-arg
                          (epkg-read-package "Limit to packages after: "))))
  (dolist (pkg (epkgs nil [wiki]))
    (let ((name (oref pkg name)))
      (when (or (not from) (string< from name))
        (if (assoc name emir-suspended-packages)
            (message "Skipping suspended %s" name)
          (message "Updating %s..." name)
          (if recreate
              (emir-update (epkg name) t)
            (emir-update-package name))
          (message "Updating %s...done" name)))))
  (emir-commit (emir--update-message) nil :dump))

;;;###autoload
(defun emir-update-other-packages (&optional from recreate)
  (interactive (list (and current-prefix-arg
                          (epkg-read-package "Limit to packages after: "))))
  (dolist (pkg (epkgs nil [mirrored* !github* !wiki]))
    (let ((name (oref pkg name)))
      (when (or (not from) (string< from name))
        (if (assoc name emir-suspended-packages)
            (message "Skipping suspended %s" name)
          (message "Updating %s..." name)
          (if recreate
              (emir-update (epkg name) t)
            (emir-update-package name))
          (message "Updating %s...done" name)))))
  (emir-commit (emir--update-message) nil :dump))

;;;###autoload
(defun emir-regenerate-metadata (&optional from)
  (interactive (list (and current-prefix-arg
                          (epkg-read-package "Limit to packages after: "))))
  (dolist (pkg (epkgs))
    (let ((name (oref pkg name)))
      (when (or (not from) (string< from name))
        (message "Regenerating %s..." name)
        (emir-update pkg t)
        (message "Regenerating %s...done" name))))
  (emir-commit "Regenerate metadata" nil :dump))

;;;###autoload
(defun emir-update-licenses (&optional all)
  (interactive "P")
  (dolist (pkg (epkgs))
    (with-slots (name) pkg
      (when (or all
                (member (oref pkg license)
                        '(nil "failure" "pending" "none" "custom")))
        (message "Updating %s..." name)
        (with-emir-repository pkg
          (when-let ((lib (emir--main-library pkg)))
            (with-temp-buffer
              (insert-file-contents lib)
              (oset pkg license (emir--license pkg)))))
        (message "Updating %s...done" name))))
  (emir-commit "Update licenses" nil :dump))

;;;###autoload
(defun emir-update-branches (from)
  (interactive (list (and current-prefix-arg
                          (epkg-read-package "Limit to packages after: "))))
  (dolist (name (epkgs 'name '(epkg-git-package-p
                               epkg-gitlab-package-p
                               epkg-github-package-p)))
    (when (or (not from) (string< from name))
      (message "Updating %s..." name)
      (emir-update-branch name)
      (message "Updating %s...done" name)))
  (emir-commit (emir--update-message) nil :dump))

;;;###autoload
(defun emir-update-branch (name)
  (interactive (list (epkg-read-package "Update branch of: " nil
                                        epkg--trivial-package-predicates)))
  (let* ((pkg (epkg name))
         (url (oref pkg url))
         (value (oref pkg upstream-branch)))
    (with-emir-repository pkg
      (if-let ((head (emir--remote-head url)))
          (if (equal value "master")
              (when (equal head "master")
                (setf value nil))
            (unless (or (equal head "master")
                        (equal head value))
              (message "  %s => %s" value head)
              ;; TODO File bug report: This fails if previously unset.
              (magit-git "remote" "set-branches" "origin" head)
              ;; TODO File bug report: Pruning doesn't work here.
              (magit-git "fetch" "--prune" "origin")
              (magit-git "update-ref" "-d"
                         (concat "refs/remotes/origin/" (or value "master")))
              (magit-git "remote" "set-head" "origin" "--auto")
              (magit-git "branch" "master"
                         (concat "--set-upstream-to=origin/" head))
              (setq value head)
              (emir-update-package name)))
        (message "Error: cannot determine head"))))
  (when (called-interactively-p 'any)
    (emir-dump-database)))

;;;; Patch

;;;###autoload
(defun emir-join-provided (name feature reason)
  (interactive
   (let* ((name     (epkg-read-package "Package: "))
          (pkg      (epkg name))
          (features (oref pkg provided))
          (feature  (intern (read-string "Join provide: ")))
          (reason   (read-string "Reason: " (nth 2 (assq feature features)))))
     (list name feature reason)))
  (let* ((pkg (epkg name))
         (val (oref pkg provided))
         (elt (assq feature val)))
    (if elt
        (progn (setf (nth 2 elt) reason)
               (oset pkg provided val))
      (oset pkg provided (cons (list feature nil reason) val))))
  (emir-stage name :dump))

;;;###autoload
(defun emir-drop-provided (name feature reason)
  (interactive
   (let* ((name     (epkg-read-package "Package: "))
          (pkg      (epkg name))
          (features (oref pkg provided))
          (feature  (intern (completing-read "Drop provide: "
                                             (mapcar #'car features)
                                             nil t)))
          (reason   (read-string "Reason: " (nth 1 (assq feature features)))))
     (list name feature
           (and (not (equal reason "")) reason))))
  (let* ((pkg (epkg name))
         (val (oref pkg provided))
         (elt (assq feature val)))
    (setf (nth 1 elt) reason)
    (oset pkg provided val))
  (emir-stage name :dump))

;;;###autoload
(defun emir-drop-required (name feature reason)
  (interactive
   (let* ((name     (epkg-read-package "Package: "))
          (pkg      (epkg name))
          (features (oref pkg required))
          (feature  (intern (completing-read "Drop required: "
                                             (mapcar #'car features)
                                             nil t)))
          (reason   (read-string "Reason: " (nth 3 (assq feature features)))))
     (list name feature
           (and (not (equal reason "")) reason))))
  (let* ((pkg (epkg name))
         (val (oref pkg required))
         (elt (assq feature val)))
    (setf (nth 3 elt) reason)
    (oset pkg required val))
  (emir-stage name :dump))

;;;; Shelve

;;;###autoload
(defun emir-shelve-package (name)
  (interactive
   (list (epkg-read-package "Shelve package: " nil
                            '(epkg-mirrored-package--eieio-childp))))
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
                   url)))
    (with-emir-repository t
      (emir-update  pkg)
      (emir-gh-init pkg)
      (emir-push    pkg)
      (emir-commit (format "Shelve %S package" name) name :dump))
    (let ((default-directory emir-melpa-repository)
          (rcp (concat "recipes/" name)))
      (when-let ((msg (and (file-exists-p rcp)
                           (ignore-errors
                             (read-string
                              "Also remove from Melpa with message: "
                              (format "Remove archived %S package" name))))))
        (magit-git "rm" rcp)
        (magit-git "commit" "-m" msg "--" rcp)))))

;;;###autoload
(defun emir-shelve-archived-github-packages ()
  (interactive)
  (when emir--archived-packages
    (dolist (name emir--archived-packages)
      (when (epkg name)
        (let ((kept (cadr (assoc name emir-kept-packages)))
              (deps (epkg-reverse-dependencies name)))
          (if (or kept deps)
              (message "Skipping %s... %s" name
                       (if kept
                           "(member of emir-kept-packages)"
                         (format "needed by %s" deps)))
            (message "Shelve %s..." name)
            (emir-shelve-package name)
            (message "Shelve %s...done" name)))))))

;;;; Remove

;;;###autoload
(defun emir-remove-package (name)
  (interactive (list (epkg-read-package "Remove package: ")))
  (let ((pkg (epkg name)))
    (unless (epkg-builtin-package-p pkg)
      (emir--remove-module-worktree name)
      (with-demoted-errors "Error: %S"
        (emir-gh-delete pkg)))
    (closql-delete pkg)
    ;; Do not pass `name'.  The module was removed above.
    (emir-commit (format "Remove %S package" name) nil :dump)
    (with-demoted-errors "Error: %S"
      (emir--stash-module-gitdir name))
    (with-demoted-errors "Error: %S"
      (when (epkg-wiki-package-p pkg)
        (with-emir-repository 'epkg-wiki-package
          (magit-call-git "branch" "-D" name))))))

(defun emir--remove-module-worktree (name)
  (with-emir-repository t
    (magit-git "add" ".gitmodules")
    (let ((module-dir (epkg-repository (epkg name))))
      (when (file-exists-p module-dir)
        (magit-git "rm" "-f" module-dir)))))

(defun emir--stash-module-gitdir (name)
  (with-emir-repository t
    (let ((default-directory (magit-git-dir)))
      (make-directory "removed/" t)
      (rename-file (format "modules/%s" name)
                   (format "removed/%s-%s" name
                           (format-time-string "%FT%T"))))))

;;;; Setup

;;;###autoload
(defun emir-setup-module (name)
  (interactive (list (epkg-read-package "Setup package module: ")))
  (condition-case err
      (progn (message "Setup module %s..." name)
             (emir-setup (epkg name))
             (message "Setup module %s...done" name))
    (error
     (push name emir-failed)
     (message "Update error (%s): %s" name (error-message-string err)))))

;;;###autoload
(defun emir-setup-modules ()
  (interactive)
  (setq emir-failed nil)
  (setq message-log-max 20000)
  (mapc #'emir-setup-module
        (epkgs 'name [mirrored* shelved])))

;;;; Migrate

(defun emir-migrate-package (name url class)
  (interactive
   (let* ((name (epkg-read-package "Migrate package: "))
          (melpa-url (nth 4 (assoc name (emir-melpa--migrated-packages))))
          (url (emir-read-url "New repository url" melpa-url))
          (class (emir--read-class url)))
     (list name url class)))
  (let ((pkg (epkg name)))
    (unless pkg
      (user-error "Package does not %s exist yet" name))
    (oset pkg url url)
    (oset pkg upstream-user nil)
    (oset pkg upstream-name nil)
    (oset pkg stars nil)
    (dolist (slot '(upstream-branch upstream-tree library libraries patched))
      (when-let ((value (slot-value pkg slot)))
        (when (y-or-n-p (format "Reset %s (%S)" slot value))
          (setf (slot-value pkg slot) nil))))
    (closql--set-object-class (epkg-db) pkg class)
    (emir--remove-module-worktree name)
    (emir--stash-module-gitdir name)
    (emir-add pkg t)
    (emir-melpa-migrate-recipe name "Update %s's repository")
    (setf (alist-get name emir--moved-packages nil t #'equal) nil)
    (customize-save-variable 'emir--moved-packages emir--moved-packages)
    (emir-commit (format "Migrate %S package" name) name :dump :sort)))

(defun emir-migrate-github-packages ()
  (interactive)
  (when emir--moved-packages
    (pcase-dolist (`(,name ,_old ,new) emir--moved-packages)
      (when new
        (message "Migrating %s..." name)
        (emir-migrate-github-package name new t)
        (message "Migrating %s...done" name)))))

(defun emir-migrate-github-package (name new &optional redirected)
  (interactive (list (epkg-read-package "Migrate github package: ")
                     (emir-read-url "New repository url")))
  (let ((pkg (epkg name))
        (url (if (string-match "\\`git@github\\.com:\\(.+\\)\\.git\\'" new)
                 new
               (format "git@github.com:%s.git" new))))
    (oset pkg url url)
    (emir--set-urls pkg)
    (oset pkg repopage (emir--format-url pkg 'repopage-format))
    (with-emir-repository pkg
      (magit-call-git "config" "remote.origin.url" url))
    (emir-melpa-migrate-recipe name "Update url of %s's repository" redirected)
    (setf (alist-get name emir--moved-packages nil t #'equal) nil)
    (customize-save-variable 'emir--moved-packages emir--moved-packages)
    (emir-commit (format "Migrate %S package within github" name) name :dump)))

;;;; Stage

(defun emir-stage (&optional name dump sort)
  (interactive (list nil t t))
  (with-emir-repository t
    (when name
      (let ((pkg (epkg name)))
        (when (and pkg (not (epkg-builtin-package-p pkg)))
          (let ((module (directory-file-name
                         (file-relative-name (epkg-repository pkg)))))
            (magit-git "update-index" "--no-assume-unchanged" module)
            (magit-git "add" module)))))
    (when dump
      (emir-dump-database))
    (when sort
      (borg--sort-submodule-sections ".gitmodules"))
    (magit-git "add" ".gitmodules" "epkg.sql" "ewiki" "gelpa" "melpa")))

(defun emir-dump-database ()
  (interactive)
  (message "Dumping Epkg database...")
  (with-emir-repository t
    (let ((bin (expand-file-name "epkg.sqlite"))
          (txt (expand-file-name "epkg.sql")))
      (with-temp-file txt
        (unless (zerop (save-excursion
                         (call-process "sqlite3" nil t nil
                                       bin ".dump")))
          (error "Failed to dump %s" bin))
        (insert (format "PRAGMA user_version=%s;\n" epkg-db-version))
        ;; Here the value of the `foreign_keys' pragma does not actually
        ;; matter.  The dump *always* contains a line that disables it.
        ;; That would be the case even if we extended the above command
        ;; to explicitly enable it.  That is strange but does not really
        ;; matter because `closql-db' always enables foreign key support.
        ;; We do this just to avoid alarming observant users.
        (when (re-search-forward "^PRAGMA foreign_keys=\\(OFF\\);" 1000 t)
          (replace-match "ON" t t nil 1)))))
  (message "Dumping Epkg database...done"))

;;; Git
;;;; Import

(cl-defmethod emir-import ((pkg epkg-wiki-package))
  (with-emir-repository 'epkg-wiki-package
    (with-slots (name) pkg
      (message "Importing %s..." name)
      (magit-git "filter-emacswiki" "--tag" "--notes" name)
      (message "Importing %s...done" name))))

;;;; Clone

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
        (epkg-gnu-elpa-package
         (setq branch (concat "externals/" name))))
      (magit-git "clone"
                 (and (emir--ignore-tags-p pkg) "--no-tags")
                 "--single-branch" "--branch" branch origin module)
      (cl-typecase pkg
        (epkg-gnu-elpa-package
         (with-emir-repository pkg
           (magit-git "config" "remote.origin.url"
                      (file-relative-name emir-gelpa-repository)))))
      (magit-git "submodule" "add" "--name" name mirror module)
      (magit-git "submodule" "absorbgitdirs" module))
    (with-emir-repository pkg
      (magit-git "remote" "add" "mirror" mirror)
      (magit-git "config" "remote.pushDefault" "mirror")
      (unless (equal branch "master")
        (magit-git "branch" "--move" branch "master")))))

(cl-defmethod emir-clone ((pkg epkg-subrepo-package))
  (let* ((name   (oref pkg name))
         (mirror (oref pkg mirror-url))
         (origin (oref pkg url))
         (branch (or (oref pkg upstream-branch) "master"))
         (source (format ".git/modules/%s/unfiltered" name))
         (module (concat "mirror/" name)))
    (with-emir-repository t
      (magit-git "init" "--initial-branch" "master" module))
    (with-emir-repository pkg
      (magit-git "commit" "--allow-empty" "-m" "dummy"))
    (with-emir-repository t
      (magit-git "submodule" "add" "--name" name mirror module)
      (magit-git "submodule" "absorbgitdirs" module)
      (unless (cl-typep pkg 'epkg-core-package)
        (magit-git "clone" "--single-branch" "--branch" branch origin source)
        (unless (equal branch "master")
          (let ((default-directory (expand-file-name source)))
            (magit-git "branch" "--move" branch "master")))))
    (emir-pull pkg)
    (with-emir-repository pkg
      (magit-git "remote" "add" "mirror" mirror)
      (magit-git "config" "remote.pushDefault" "mirror"))))

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

(cl-defmethod emir-pull ((pkg epkg-gnu-elpa-package))
  (with-emir-repository pkg
    (magit-git "pull" "--ff-only" "origin"
               (concat "refs/remotes/origin/externals/" (oref pkg name)))))

(cl-defmethod emir-pull ((pkg epkg-subrepo-package))
  (let* ((name   (oref pkg name))
         (core   (cl-typep pkg 'epkg-core-package))
         (source (if core
                     (expand-file-name
                      "shallow/" (f-parent emir-emacs-repository))
                   (format ".git/modules/%s/unfiltered" name)))
         (target (format "mirror/%s" name)))
    (with-emir-repository t
      (unless core
        (let ((default-directory (expand-file-name source)))
          (magit-git "pull" "--ff-only" "origin")))
      (magit-git "filter-repo"
                 "--force"
                 "--source" source
                 "--target" target
                 (if core
                     (list "--refs" "refs/heads/master"
                           (emir-gelpa--core-filter-args name))
                   ;; For this class this is a list of arguments.
                   (oref pkg upstream-tree))
                 "--prune-empty=always"
                 "--prune-degenerate=always"))))

(cl-defmethod emir-pull ((pkg epkg-file-package) &optional force)
  (with-emir-repository pkg
    (let ((name (oref pkg name)))
      (let ((magit-process-raise-error t))
        (magit-call-process "curl" "-O" (oref pkg url)))
      (when (or (magit-anything-modified-p) force)
        (magit-git "add" ".")
        (let ((process-environment process-environment)
              (mainlib (or (and (not (cl-typep pkg 'epkg-core-package))
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

;;;; Push

(cl-defmethod emir-push ((pkg epkg-package) &optional force)
  (with-emir-repository pkg
    (magit-git "push"
               (and (or force (oref pkg patched)) "--force")
               (and (not (emir--ignore-tags-p pkg)) "--follow-tags")
               "mirror" "master")))

;;;; Commit

(defun emir-commit (message &optional name dump sort)
  (emir-stage name dump sort)
  (with-emir-repository t
    (if (magit-anything-staged-p)
        (magit-git "commit" "-m" message)
      (message "Nothing staged"))))

(defun emir--update-message ()
  (with-emir-repository t
    (let ((count (length
                  (cl-union
                   (mapcar (l'substring % 6) (magit-staged-files nil "attic"))
                   (mapcar (l'substring % 7) (magit-staged-files nil "mirror"))
                   :test #'equal))))
      (format "Update %s package%s" count (if (> count 1) "s" "")))))

;;;; Setup

;; TODO emir-setup: Handle epkg-subrepo-package
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
        (epkg-gnu-elpa-package
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
        (epkg-wiki-package
         (magit-git "remote" "add" "-f" "--no-tags"
                    "-t" branch "origin" origin)
         (magit-git "branch" (concat "--set-upstream-to=origin/" branch)))
        (epkg-gnu-elpa-package
         (magit-git "remote" "add" "-f" "--no-tags"
                    "-t" branch "origin" origin)
         (magit-git "branch" (concat "--set-upstream-to=origin/" branch)))
        (t
         (magit-git "remote" "add" "-f"
                    "-t" branch "origin" origin)
         (magit-git "branch" (concat "--set-upstream-to=origin/" branch)))))))

;;; Database
;;;; Add

(cl-defmethod emir-add ((pkg epkg-mirrored-package) &optional replace)
  (emir--set-urls pkg)
  (emir--set-upstream-branch pkg)
  (oset pkg mirror-name
        (string-replace "+" "-plus" (oref pkg name)))
  (when (epkg-orphaned-package-p pkg)
    (oset pkg upstream-user "emacsorphanage"))
  (oset pkg mirror-url (emir--format-url pkg 'mirror-url-format))
  (oset pkg mirrorpage (emir--format-url pkg 'mirrorpage-format))
  (oset pkg repopage   (emir--format-url pkg 'repopage-format))
  (oset pkg homepage   (emir--format-url pkg 'homepage-format))
  (unless replace
    (closql-insert (epkg-db) pkg)
    (emir-gh-init pkg))
  (emir-clone     pkg)
  (emir-push      pkg replace)
  (emir-update    pkg replace)
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
                      (car (cl-find-if (pcase-lambda (`(,lib))
                                         (string-suffix-p main lib))
                                       builtin-libraries)))
                    library))))
      (if-let ((lib (emir--main-library pkg)))
          (with-temp-buffer
            (insert-file-contents lib)
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
    (when-let ((buf (magit-get-mode-buffer 'magit-process-mode)))
      (kill-buffer buf))))

;;; Extract

(cl-defmethod emir--main-library ((pkg epkg-package))
  (or (and (not (cl-typep pkg 'epkg-core-package))
           (oref pkg library))
      (let ((name (oref pkg name))
            (load-suffixes '(".el" ".el.in" ".el.tmpl"))
            (load-file-rep-suffixes '("")))
        (or (ignore-errors
              (packed-main-library default-directory name nil t))
            (and (or (epkg-shelved-package-p pkg)
                     (equal name "gnu-elpa")) ; KLUDGE No feature.
                 (let ((file (expand-file-name (concat name ".el"))))
                   (and (file-exists-p file) file)))))))

(cl-defmethod emir--license ((pkg epkg-package))
  (let ((name (oref pkg name)))
    (or (elx-license nil (epkg-repository pkg) name)
        (and (string-search "-theme" name)
             (let ((file (expand-file-name
                          (concat (substring name 0 (match-beginning 0))
                                  ".el"))))
               (and (file-exists-p file)
                    (elx-license file))))
        (let ((license (oref pkg license)))
          (and (member license '("failure" "pending" "none" "custom"))
               license)))))

(cl-defmethod emir--updated ((_pkg epkg-package))
  (string-replace "-" "" (substring (magit-rev-format "%ci" "HEAD") 0 10)))

(cl-defmethod emir--updated ((_pkg epkg-file-package))
  (or (elx-updated)
      (cl-call-next-method)))

(cl-defmethod emir--updated ((_pkg epkg-builtin-package)))

(cl-defmethod emir--homepage ((pkg epkg-package))
  (or (cadr (assoc (oref pkg name) emir--homepage-alist))
      (and-let* ((url (lm-homepage)))
        (if (string-suffix-p "/" url)
            (substring url 0 -1)
          url))
      (emir--format-url pkg 'homepage-format)))

(cl-defmethod emir--wikipage ((pkg epkg-package))
  (and-let*
      ((page (or (and (epkg-wiki-package-p pkg) (elx-wikipage))
                 (cadr (assoc (oref pkg name) emir--wikipage-alist))
                 (let* ((norm (lambda (string)
                                (thread-last string
                                  (string-replace "+" "plus")
                                  (string-replace "-" "")
                                  downcase)))
                        (name (funcall norm (oref pkg name)))
                        (alist (with-emir-repository 'epkg-wiki-package
                                 (mapcar (l'cons (funcall norm %) %)
                                         (magit-list-files)))))
                   (or (cdr (assoc name alist))
                       (cdr (assoc (if (string-suffix-p "mode" name)
                                       (substring name 0 -4)
                                     (concat name "mode"))
                                   alist)))))))
    (concat "https://emacswiki.org/" page)))

(defun emir--authors ()
  (cl-delete-duplicates (mapcar (pcase-lambda (`(,name . ,email))
                                  (list name email))
                                (elx-authors))
                        :test #'equal :key #'car))

(defun emir--maintainers ()
  (cl-delete-duplicates (mapcar (pcase-lambda (`(,name . ,email))
                                  (list name email))
                                (elx-maintainers))
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
            (pcase-let ((`(,h ,s) (packed-required))
                        (p        (packed-provided)))
              (dolist (h h) (cl-pushnew h hard))
              (dolist (s s) (cl-pushnew s soft))
              (dolist (p p) (cl-pushnew p provided))))))
      (list
       (let ((drop (epkg-sql [:select [feature drop] :from required
                              :where (and (= package $s1) (notnull drop))
                              :order-by [(asc feature)]]
                             name)))
         (setq hard (cl-set-difference hard provided))
         (setq soft (cl-set-difference soft provided))
         (setq soft (cl-set-difference soft hard))
         (nconc (mapcar (l'list % t   nil (cadr (assoc % drop))) hard)
                (mapcar (l'list % nil nil (cadr (assoc % drop))) soft)))
       (let ((drop (epkg-sql [:select [feature drop] :from provided
                              :where (and (= package $s1) (notnull drop))
                              :order-by [(asc feature)]]
                             name))
             (join (epkg-sql [:select [feature join] :from provided
                              :where (and (= package $s1) (notnull join))
                              :order-by [(asc feature)]]
                             name)))
         (nconc (mapcar (l'list % (cadr (assoc % drop)) nil) provided)
                (mapcan (pcase-lambda (`(,feature ,reason))
                          (unless (memq feature provided)
                            (push feature provided)
                            (list (list feature nil reason))))
                        join)))))))

(defun emir--builtin-packages-alist ()
  (let ((default-directory emir-emacs-repository))
    (mapcar
     (lambda (elt)
       (cons (car elt) (mapcar #'cdr (cdr elt))))
     (seq-group-by
      #'car
      (cl-sort
       (cl-mapcan
        (lambda (file)
          (and (string-suffix-p ".el" file)
               (if (or (string-match-p finder-no-scan-regexp file)
                       (member file
                               '("lisp/gnus/.dir-locals.el"
                                 ;; Old versions:
                                 "lisp/obsolete/old-emacs-lock.el"
                                 "lisp/obsolete/otodo-mode.el"
                                 ;; Moved to GNU Elpa:
                                 "lisp/obsolete/crisp.el"
                                 "lisp/obsolete/landmark.el")))
                   (prog1 nil (message "Skipping %s...done" file))
                 (message "Importing %s..." file)
                 (with-temp-buffer
                   (insert-file-contents file)
                   (emacs-lisp-mode)
                   (let ((package
                          (cond
                           ((equal file "lisp/epa-ks.el") "epa")
                           ((equal file "lisp/emacs-lisp/shorthands.el") "emacs")
                           ((string-prefix-p "lisp/leim/"     file) "emacs")
                           ((string-prefix-p "lisp/obsolete/" file) "emacs")
                           ((lm-header "Package"))
                           ((and-let* ((elt (assoc (thread-first file
                                                     file-name-directory
                                                     directory-file-name
                                                     file-name-nondirectory)
                                                   finder--builtins-alist)))
                              (symbol-name (cdr elt))))
                           ((thread-first file
                              file-name-nondirectory
                              file-name-sans-extension)))))
                     (prog1 (mapcar (l'list package file %)
                                    (or (packed-provided)
                                        (list nil)))
                       (message "Importing %s...done" file)))))))
        (magit-git-items "ls-tree" "-z" "-r" "--name-only" "HEAD" "lisp/"))
       #'string< :key #'car)))))

;;; Github

(cl-defmethod emir-gh ((pkg epkg-package)
                       method resource
                       &optional params
                       &key callback errorback)
  (setq resource
        (save-match-data
          (format-spec resource
                       (format-spec-make
                        ?u (oref pkg upstream-user)
                        ?n (oref pkg upstream-name)
                        ?o (if (epkg-shelved-package-p pkg)
                               "emacsattic"
                             "emacsmirror")
                        ?m (oref pkg mirror-name)))))
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
  (if (cl-flet ((forked (rsp key)
                  (and-let* ((name (cdr (assq 'full_name (cdr (assq key rsp))))))
                    (cl-find-if
                     (lambda (fork)
                       (equal (cdr (assq 'login (cdr (assq 'owner fork))))
                              "emacsmirror"))
                     (emir-gh pkg "GET" (format "/repos/%s/forks" name))))))
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
      (when-let ((branches
                  (delete "master" (magit-list-remote-branches "mirror"))))
        (magit-git "push" "mirror" (mapcar (lambda (b) (concat ":" b)) branches))))))

(cl-defmethod emir-gh-delete ((pkg epkg-package))
  (emir-gh pkg "DELETE" "/repos/%o/%m"))

(defun emir-gh-foreach-query (query callback &optional per-page packages)
  (let* ((page 0)
         (result nil)
         (groups
          (seq-partition
           (mapcar (lambda (pkg)
                     ;; Package names may contain characters that are
                     ;; invalid here.  Identifiers also may not begin
                     ;; with a number or contain an equal sign.
                     `((,(concat "_"
                                 (string-replace
                                  "=" "_"
                                  (base64-encode-string (oref pkg name))))
                        repository)
                       [(owner ,(oref pkg upstream-user))
                        (name  ,(oref pkg upstream-name))]
                       ,@(if (functionp query)
                             (funcall query pkg)
                           query)))
                   (cond
                    ((not packages)
                     (epkgs nil [github*]))
                    ((integerp packages)
                     (take packages (epkgs nil [github*])))
                    ((mapcar #'epkg packages))))
           (or per-page 100)))
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
                            (string-replace
                             "_" "=" (substring (symbol-name (car elt)) 1)))))
             (funcall callback result)))))
      (cb))))

;;; Urls

(cl-defmethod emir--set-urls ((pkg epkg-mirrored-package))
  (if-let ((url (oref pkg url)))
      (progn
        (let ((conflict (and url (emir--lookup-url url))))
          (when (and conflict
                     (not (equal conflict (oref pkg name)))
                     (not (and (cl-typep pkg 'epkg-subrepo-package)
                               (let ((old (epkg conflict)))
                                 (cl-typep old 'epkg-subrepo-package)
                                 (cl-typep old 'epkg-builtin-package)))))
            (user-error "Another package, %s, is already mirrored from %s"
                        conflict url)))
        (when-let ((url-format (oref pkg url-format)))
          (pcase-dolist (`(,slot . ,value) (emir--match-url url-format url))
            (eieio-oset pkg slot value))))
    (oset pkg url (oref-default pkg url-format))))

(cl-defmethod emir--set-urls :after ((pkg epkg-hg-package))
  (let ((url (oref pkg url)))
    (unless (string-prefix-p "hg::" url)
      (oset pkg url (concat "hg::" url)))))

(cl-defmethod emir--format-url ((pkg epkg-package) slot)
  (and-let* ((format (if (stringp slot)
                         slot
                       (eieio-oref-default pkg slot))))
    (save-match-data
      (format-spec format
                   `((?m . ,(or (oref pkg mirror-name)
                                (oref pkg name)))
                     (?n . ,(or (oref pkg upstream-name)
                                (oref pkg name)))
                     (?u . ,(oref pkg upstream-user))
                     (?l . ,(oref pkg library)))))))

(defun emir--match-url (format url)
  (with-temp-buffer
    (thread-last format
      (regexp-quote)
      (replace-regexp-in-string "\\(git@\\([^:/]*\\)\\:\\)"
                                "\\\\(?:https://\\2/\\\\|\\1\\\\)")
      (replace-regexp-in-string "\\(\\\\.git\\)\\'"
                                "\\\\(\\\\.git\\\\)?")
      (insert))
    (goto-char (point-min))
    (let (slots)
      (save-match-data
        (while (re-search-forward "%\\(.\\)" nil t)
          (pcase-let ((`(,char . ,slot)
                       (assq (string-to-char (match-string 1))
                             '((?m . mirror-name)
                               (?n . upstream-name)
                               (?u . upstream-user)))))
            (push slot slots)
            (replace-match (if (= char ?u) "\\(.+\\)" "\\([^/]+?\\)") t t)))
        (and (string-match (concat "\\`" (buffer-string) "\\'") url)
             (let ((i 0))
               (mapcar (l'cons % (match-string (cl-incf i) url))
                       (nreverse slots))))))))

(defun emir--url-to-class (url)
  (cl-find-if (lambda (class)
                (ignore-errors
                  (emir--match-url (oref-default class url-format) url)))
              (closql--list-subclasses 'epkg-package)))

(defun emir--url-get (url slot)
  (and-let* ((class (emir--url-to-class url))
             (slots (emir--match-url (oref-default class url-format) url)))
    (cdr (assoc slot slots))))

(defun emir--lookup-url (url)
  (caar (epkg-sql [:select name :from packages
                   :where (or (= url $s1) (= url $s2))
                   :limit 1]
                  url
                  (if (string-suffix-p ".git" url)
                      (substring url 0 -4)
                    (concat url ".git")))))

(defvar emir-url-history nil)

(defun emir-read-url (prompt &optional initial-input)
  (magit-read-string prompt initial-input 'emir-url-history nil nil t))

(defun emir--read-class (url &optional default prompt)
  (intern (format "epkg-%s-package"
                  (epkg-read-type (or prompt "Package type: ")
                                  (if-let* ((sym (emir--url-to-class url)))
                                      (substring (symbol-name sym) 5 -8)
                                    (or default "git"))))))

;;; Miscellaneous

(cl-defmethod emir--set-upstream-branch ((pkg epkg-mirrored-package))
  (unless (oref pkg upstream-branch)
    (let ((branch (emir--remote-head (oref pkg url))))
      (unless (equal branch "master")
        (oset pkg upstream-branch branch)))))

(defun emir--remote-head (url)
  (and-let* ((line (cl-find-if
                    (lambda (line)
                      (string-match
                       "\\`ref: refs/heads/\\([^\s\t]+\\)[\s\t]HEAD\\'" line))
                    (magit-git-lines "ls-remote" "--symref" url))))
    (match-string 1 line)))

(defun emir--ignore-tags-p (pkg)
  (or (cl-typep pkg 'epkg-subtree-package)
      (cl-typep pkg 'epkg-wiki-package)
      (cl-typep pkg 'epkg-gnu-elpa-package)))

;;; _
(provide 'emir)
(require 'emir-gelpa)
(require 'emir-melpa)
(require 'emir-utils)
;;; emir.el ends here
