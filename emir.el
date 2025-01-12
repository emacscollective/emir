;;; emir.el --- Maintain the Emacsmirror  -*- lexical-binding:t -*-

;; Copyright (C) 2016-2025 Jonas Bernoulli

;; Author: Jonas Bernoulli <emacs.emir@jonas.bernoulli.dev>
;; Homepage: https://github.com/emacscollective/emir
;; Keywords: local

;; Package-Version: 3.0.3
;; Package-Requires: (
;;     (emacs "29.3")
;;     (compat "30.0.2.0")
;;     (borg "4.1.2")
;;     (elx "2.1.0")
;;     (emacsql "4.1.0")
;;     (epkg "4.0.3")
;;     (ghub "4.2.0")
;;     (llama "0.4.1")
;;     (magit "4.2.0")
;;     (org "9.7.19"))

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
(require 'emacsql)
(require 'epkg)
(require 'epkg-org)
(require 'epkg-utils)
(require 'finder)
(require 'ghub)
(require 'llama)
(require 'magit)
(require 'org)
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

(setq message-log-max (max message-log-max 30000))

(defconst emir-rewrite-threshold 16)
(defconst emir-rewrite-always-update '("ideasman42" "laishulu"))

;;; Repositories

;; Emacs' history, including very recent history, is littered with so
;; many twig merges, "git describe" cannot make sense of it anymore.
;; To force it to use the latest tag, use something like "git describe
;; --long --match 'emacs-30.0*'".  Except when we are very close to a,
;; tag the count never-the-less tends to be off by several magnitudes.
(defconst emir-emacs-reference "emacs-30.0.91-0-g9a1c76bf7ff")

(defconst emir-emacs-repository "~/src/emacs/emacs/master")
(defconst emir-gnu-elpa-repository (expand-file-name "gnu-elpa/" epkg-repository))
(defconst emir-nongnu-elpa-repository (expand-file-name "nongnu-elpa/" epkg-repository))
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

(defmacro emir-with-emacs-worktree (&rest body)
  (declare (indent defun))
  (let ((worktree (gensym "emacs-worktree-")))
    `(let ((,worktree nil))
       (unwind-protect
           (let* ((emir-emacs-repository
                   (let ((default-directory emir-emacs-repository))
                     (setq ,worktree (make-temp-file "emir-emacs-" t))
                     (magit-run-git "worktree" "add"
                                    (magit--expand-worktree ,worktree)
                                    emir-emacs-reference)
                     ,worktree))
                  (default-directory emir-emacs-repository))
             ,@body)
         (let ((default-directory emir-emacs-repository))
           (with-demoted-errors "Cleanup Emacs worktree: %S"
             (magit-worktree-delete ,worktree)))))))

(cl-defmethod epkg-repository ((_pkg epkg-builtin-package))
  emir-emacs-repository)
(cl-defmethod epkg-repository ((_class (subclass epkg-nongnu-elpa-package)))
  emir-nongnu-elpa-repository)
(cl-defmethod epkg-repository ((_class (subclass epkg-gnu-elpa-package)))
  emir-gnu-elpa-repository)
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
   ("-a" "Re-import all recipes" "--all")
   ("-f" "Fetch before import"   "--fetch")]
  ["Import"
   ("a" "All"         emir-import-all-elpa-recipes)
   ("m" "Melpa"       emir-import-melpa-recipes)
   ("n" "NonGNU Elpa" emir-import-nongnu-elpa-recipes)
   ("g" "GNU Elpa"    emir-import-gnu-elpa-recipes)])

;;;###autoload
(defun emir-import-emacs-packages ()
  "Import built-in packages."
  (interactive)
  (let ((alist (emir--builtin-packages-alist)))
    (emir-with-emacs-worktree
      (emacsql-with-transaction (epkg-db)
        (pcase-dolist (`(,name . ,value) alist)
          (message "Updating %s..." name)
          ;; While the table schemata allows for more than one feature
          ;; per library, Closql only allows for one (PKG LIB "VALUE"),
          ;; so one would randomly end up being stored in the database.
          ;; Nothing explicitly requires these features, so store none.
          (when (equal name "emacs")
            (setq value `(("lisp/bindings.el" nil)
                          ,@(cl-remove "lisp/bindings.el" value
                                       :key #'car :test #'equal))))
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
              (message "Deleting %s...done" name)))))))
  (emir-commit (format "Update built-in packages for %s"
                       emir-emacs-reference)
               nil :dump)
  (emir-import-gnu-elpa-recipes nil))

;;;###autoload
(defun emir-import-ewiki-packages (&optional drew-only)
  "Import packages from the Emacswiki
With a prefix argument, only import Drew's packages."
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

;;;###autoload
(defun emir-import-ewiki-package (name)
  "Import the package named NAME from the Emacswiki."
  (interactive (list (read-string "Package: ")))
  (with-emir-repository 'epkg-wiki-package
    (magit-process-buffer)
    (with-environment-variables
        (("PATH" (format "%sbin:%s"
                         (file-name-directory (locate-library "emir.el"))
                         (getenv "PATH"))))
      (magit-run-git-async "filter-emacswiki" "--tag" "--notes" name))))

;;;; Add

;;;###autoload
(defun emir-add-package (name url class &rest plist)
  "Mirror the package named NAME from URL using CLASS
Keys in PLIST must be supported by the CLASS constructor."
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
        ((emir--config name :delayed)
         (user-error "Package %s is on hold" name)))
  (emir-add (apply class :name name :url url plist))
  (when-let ((recipe (epkg-get-recipe 'melpa name)))
    (oset recipe epkg-package name))
  (when-let ((recipe (epkg-get-recipe 'gnu name)))
    (oset recipe epkg-package name))
  (emir-commit (format "Add %S package" name) name :dump :sort))

;;;###autoload
(defun emir-add-nongnu-elpa-packages (&optional dry-run)
  "Mirror new packages from NonGNU ELPA.
With a prefix argument, only show which packages would be added."
  (interactive "P")
  (emir--add-elpa-packages 'nongnu dry-run))

;;;###autoload
(defun emir-add-gnu-elpa-packages (&optional dry-run)
  "Mirror new packages from GNU ELPA.
With a prefix argument, only show which packages would be added."
  (interactive "P")
  (emir--add-elpa-packages 'gnu dry-run))

(defun emir--add-elpa-packages (elpa dry-run)
  (pcase-dolist (`(,name ,url ,class)
                 (epkg-list-recipes elpa [name url class]))
    (let ((pkg (epkg name)))
      (unless (or pkg
                  (emir--lookup-url url)
                  (eq class 'core)
                  (emir--config name :delayed)
                  (emir--config name :secondary)
                  (emir--config name :builtin-preferred))
        (message "Adding %s..." name)
        (unless dry-run
          (let ((libs (and (epkg-builtin-package-p pkg)
                           (oref pkg builtin-libraries))))
            (when libs
              (closql-delete pkg))
            (let ((class (emir--read-class
                          url
                          (format "%s-elpa" elpa)
                          (format "Package type for %s from %s: "
                                  name url))))
              (setq pkg (if (eq class 'epkg-gnu-elpa-package)
                            (epkg-gnu-elpa-package :name name)
                          (funcall class :name name :url url))))
            (emir-add pkg)
            (when-let ((u (emir--format-url pkg 'url-format)))
              (oset pkg url u))
            (when libs
              (oset pkg builtin-libraries libs)))
          (oset (epkg-get-recipe elpa name) epkg-package name)
          (emir-commit (format "Add %S package" name) name :dump :sort))
        (message "Adding %s...done" name)))))

;;;###autoload
(defun emir-add-melpa-packages (&optional dry-run interactive)
  "Mirror new packages from Melpa.
With a prefix argument, only show which packages would be added."
  (interactive (list current-prefix-arg t))
  (pcase-dolist
      (`(,name ,url ,class ,branch)
       (seq-keep (pcase-lambda (`(,url . ,pkgs))
                   (cond
                    ((not (cdr pkgs))
                     (car pkgs))
                    (interactive
                     (assoc (completing-read (format "Mirror %s as: " url)
                                             pkgs nil t)
                            pkgs))))
                 (seq-group-by
                  #'cadr
                  (cl-remove-if
                   (pcase-lambda (`(,name ,url))
                     (or (epkg name)
                         (emir--lookup-url url)
                         (emir--config name :delayed)
                         (emir--config name :secondary)))
                   (epkg-list-recipes 'melpa [name url class branch])))))
    (message "Adding %s..." name)
    (unless dry-run
      (when (equal branch "melpa")
        (setq branch nil))
      (apply #'emir-add-package name url
             (or (emir--url-to-class url)
                 (intern (format "epkg-%s-package" class)))
             (and branch (list :upstream-branch branch))))
    (message "Adding %s...done" name)))

;;;; Update

(defvar emir--force-push nil)

;;;###autoload
(defun emir-update-package (name &optional interactive)
  "Update the package named NAME.
With a prefix argument, update even if there are no new commits."
  (interactive (list (epkg-read-package "Update package: ")
                     (prefix-numeric-value current-prefix-arg)))
  (setq emir--force-push nil)
  (let* ((force (and interactive (>= interactive 4)))
         (pkg (epkg name))
         (tip (oref pkg hash)))
    (condition-case err
        (with-emir-repository pkg
          (emir--assert-clean-worktree)
          (when (and (cl-typep pkg 'epkg-mirrored-package)
                     (or interactive
                         (not (cl-typep pkg 'epkg-github-package))))
            (emir--update-branch pkg))
          (when (or force (cl-typep pkg 'epkg-mirrored-package))
            (emir-pull pkg force))
          (emir-update pkg)
          (when (or force (not (equal (oref pkg hash) tip)))
            (unless (epkg-builtin-package-p pkg)
              (emir-stage name (and interactive :dump)))
            (emir-gh-update pkg)
            (emir-push pkg))
          t)
      (error
       (message "Update error (%s): %s" name (error-message-string err))
       nil))))

;;;###autoload
(defun emir-update-github-packages ()
  "Update all mirrored packages that are distributed on Github."
  (interactive)
  (let ((start (current-time)))
    (emir-gh-foreach-query
     (lambda (pkg)
       `((stargazers totalCount)
         isArchived
         nameWithOwner
         ,@(and-let* ((forced (oref pkg upstream-branch)))
             `(((forced ref)
                [(qualifiedName ,(concat "refs/heads/" forced))]
                name)))
         ((tracked ref)
          [(qualifiedName ,(concat "refs/heads/" (oref pkg branch)))]
          name (target oid))
         ((default defaultBranchRef) name (target oid))))
     (lambda (data)
       (let ((total (length data))
             (skipped nil)
             (failed nil)
             (i 0))
         (pcase-dolist (`(,name . ,data) data)
           (cl-incf i)
           (cond
            ((not data)
             (oset (epkg name) upstream-state 'removed))
            ((emir--config name :suspended)
             (push name skipped))
            ((let-alist data
               (let ((pkg (epkg name))
                     (msg (format "Updating %s (%s/%s)..." name i total)))
                 ;; These updates are performed silently.
                 (oset pkg stars (or .stargazers.totalCount 0))
                 (oset pkg upstream-state (and .isArchived 'archived))
                 ;; These functions show a message if they do anything.
                 (emir--gh-maybe-migrate pkg .nameWithOwner)
                 (emir--update-branch pkg .default.name (not .forced))
                 (unless (equal (or .tracked.target.oid
                                    .default.target.oid)
                                (oref pkg hash))
                   (message "%s" msg)
                   (if (emir-update-package name)
                       (message "%sdone" msg)
                     (push name failed)
                     (message "%sfailed" msg))))))))
         (emir-commit (emir--update-message) nil :dump)
         (emir--show-update-report start total skipped failed)))
     50)))

;;;###autoload
(defun emir-update-wiki-packages (&optional from recreate)
  "Update all mirrored packages that are distributed on the Emacswiki.
With a prefix argument, read a package, and only update that and later
packages in alphabetic order.  If optional RECREATE is non-nil, update
each package, regardless of whether any new commits were fetched."
  (interactive (list (and current-prefix-arg
                          (epkg-read-package "Limit to packages after: "))))
  (emir--update-packages [wiki] from recreate))

;;;###autoload
(defun emir-update-slow-packages (&optional from recreate)
  "Update mirrored packages that are expensive to update.
With a prefix argument, read a package, and only update that and later
packages in alphabetic order.  If optional RECREATE is non-nil, update
each package, regardless of whether any new commits were fetched."
  (interactive (list (and current-prefix-arg
                          (epkg-read-package "Limit to packages after: "))))
  (emir--update-packages [subtree] from recreate))

;;;###autoload
(defun emir-update-other-packages (&optional from recreate)
  "Update mirrored packages that are not updated by another update command.
With a prefix argument, read a package, and only update that and later
packages in alphabetic order.  If optional RECREATE is non-nil, update
each package, regardless of whether any new commits were fetched."
  (interactive (list (and current-prefix-arg
                          (epkg-read-package "Limit to packages after: "))))
  (emir--update-packages [mirrored* !github* !wiki !subtree*]
                         from recreate))

(defun emir--update-packages (types from recreate)
  (let* ((start (current-time))
         (pkgs (epkgs nil types))
         (total (length pkgs))
         (skipped nil)
         (failed nil)
         (i 0))
    (dolist (pkg pkgs)
      (cl-incf i)
      (let ((name (oref pkg name)))
        (cond ((and from (string< from name))
               (push name skipped))
              ((and (emir--config name :suspended)
                    (not recreate))
               (push name skipped)
               (message "Skipping suspended %s...done" name))
              ((let ((msg (format "Updating %s (%s/%s)..." name i total)))
                 (message "%s" msg)
                 (if (if recreate
                         (condition-case err
                             (emir-update (epkg name) t)
                           (error
                            (message "Update error (%s): %s"
                                     name (error-message-string err))
                            nil))
                       (emir-update-package name))
                     (message "%sdone" msg)
                   (push name failed)
                   (message "%sfailed" msg)))))))
    (emir-commit (if (stringp recreate)
                     recreate
                   (emir--update-message))
                 nil :dump)
    (emir--show-update-report start total skipped failed)))

(defun emir--show-update-report (start total skipped failed)
  (let ((duration (/ (float-time (time-subtract (current-time) start)) 60)))
    (if (not (or skipped failed))
        (message "Successfully updated all %s packages (%.0fm)" total duration)
      (message "Successfully built %i of %s packages (%.0fm)"
               (- total (length skipped) (length failed))
               total duration)
      (when skipped
        (message "Skipped %i packages:\n%s"
                 (length skipped)
                 (mapconcat (##concat "  " %) (nreverse skipped) "\n")))
      (when failed
        (message "Building %i packages failed:\n%s"
                 (length failed)
                 (mapconcat (lambda (name)
                              (format "  %-20s %s" name
                                      (oref (epkg name) url)))
                            (nreverse failed) "\n"))))))

;;;###autoload
(defun emir-regenerate-metadata (&optional from)
  "Re-generate metadata for all mirrored packages.
With a prefix argument, read a package, and only update that and later
packages in alphabetic order."
  (interactive (list (and current-prefix-arg
                          (epkg-read-package "Limit to packages after: "))))
  (emir-with-emacs-worktree
    (emir--update-packages nil from "Regenerate metadata")))

;;;###autoload
(defun emir-update-licenses (&optional all)
  "Update license information for packages with problematic licenses.
With a prefix argument, update license information for all packages."
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

;;;; Patch

;;;###autoload
(defun emir-join-provided (name feature reason)
  "Declare that the package NAME provides FEATURE because of REASON."
  (interactive
   (let* ((name     (epkg-read-package "Package: "))
          (pkg      (epkg name))
          (features (oref pkg provided))
          (feature  (intern (completing-read "Join provide: "
                                             (mapcar #'car features))))
          (reason   (read-string "Reason: " (nth 2 (assq feature features)))))
     (list name feature
           (and (not (equal reason "")) reason))))
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
  "Declare that the package NAME does not provide FEATURE because of REASON."
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
  "Declare that the package NAME does not require FEATURE because of REASON."
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
(defun emir-shelve-package (name &optional melpa-msg)
  "Shelve the package named NAME."
  (interactive (list (epkg-read-package "Shelve package: " nil [mirrored*])))
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
    (oset pkg upstream-state nil)
    (with-emir-repository t
      (emir-update  pkg)
      (emir-gh-init pkg)
      (emir-push    pkg)
      (emir-commit (format "Shelve %S package" name) name :dump))
    (let ((default-directory emir-melpa-repository)
          (rcp (concat "recipes/" name)))
      (when (file-exists-p rcp)
        (let ((msg (ignore-errors
                     (read-string
                      "Also remove from Melpa with message (empty to skip): "
                      (format (or melpa-msg "Remove recipe for %s") name)))))
          (unless (equal msg "")
            (magit-git "rm" rcp)
            (magit-git "commit" "-m" msg "--" rcp)))))))

;;;###autoload
(defun emir-shelve-archived-github-packages ()
  "Shelve packages hosted on Github which have been archived by upstream."
  (interactive)
  (dolist (name (epkgs 'name [github*]))
    (let ((pkg (epkg name)))
      (when (eq (oref pkg upstream-state) 'archived)
        (let ((dependants (epkg-reverse-dependencies name)))
          (cond ((emir--config name :preserved)
                 (message "Skipping preserved %s..." name))
                (dependants
                 (message "Skipping %s needed by %s..." name dependants))
                (t
                 (message "Shelve %s..." name)
                 (emir-shelve-package name "Remove archived %S package")
                 (message "Shelve %s...done" name))))))))

;;;; Remove

;;;###autoload
(defun emir-remove-package (name)
  "Remove the package named NAME."
  (interactive
   (let ((name (epkg-read-package "Remove package: ")))
     (unless (y-or-n-p (format "Really REMOVE (not shelve) %s?" name))
       (user-error "Abort"))
     (list name)))
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
    (let* ((magit--refresh-cache nil)
           (default-directory (magit-gitdir)))
      (make-directory "removed/" t)
      (let ((gitdir (format "modules/%s" name)))
        (when (file-exists-p gitdir)
          (rename-file gitdir
                       (format "removed/%s-%s" name
                               (format-time-string "%FT%T"))))))))

;;;; Setup

(defun emir-setup-module (name)
  (condition-case err
      (emir-setup (epkg name))
    (error
     (message "Update error (%s): %s" name (error-message-string err))
     nil)))

(defun emir-setup-modules ()
  (let* ((start (current-time))
         (pkgs (epkgs 'name [mirrored* shelved]))
         (total (length pkgs))
         (failed nil)
         (i 0))
    (dolist (name pkgs)
      (cl-incf i)
      (let ((msg (format "Setup module for %s (%s/%s)..." name i total)))
        (message "%s" msg)
        (if (emir-setup-module name)
            (message "%sdone" msg)
          (push name failed)
          (message "%sfailed" msg))))
    (emir--show-update-report start total nil failed)))

;;;; Migrate

(defun emir-migrate-package (name url class)
  "Change the upstream repository from which to get the package named NAME."
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
    (oset pkg upstream-state nil)
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
    (emir-commit (format "Migrate \"%s\" package" name) name :dump :sort)))

(defun emir--gh-maybe-migrate (pkg new)
  (let* ((old (concat (oref pkg upstream-user) "/"
                      (oref pkg upstream-name)))
         (url (format "https://github.com/%s" new))
         (name (oref pkg name)))
    (unless (equal new old)
      (message "Migrating %s from %s to %s..." name old url)
      (when (string-prefix-p "emacsorphanage/" new)
        (closql--set-object-class (epkg-db) pkg 'epkg-orphaned-package))
      (oset pkg url url)
      (emir--set-urls pkg)
      (oset pkg repopage (emir--format-url pkg 'repopage-format))
      (with-emir-repository pkg
        (magit-call-git "config" "remote.origin.url" url))
      (oset pkg upstream-state nil)
      (emir-dump-database)
      (emir-melpa-migrate-recipe name "Update upstream of %s" t)
      (message "Migrating %s from %s to %s...done" name old url))))

;;;; Stage

(defun emir-stage (&optional name dump sort)
  "Stage changes in the Epkg repository.
If optional NAME is non-nil, stage the module of the package by that
name.  If optional DUMP is non-nil (which interactively it is, then
dump the Epkg database.  If optional SORT is non-nil, then sort the
\".gitmodules\" file."
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
    (magit-git "add" ".gitmodules" "epkg.sql"
               "ewiki" "gnu-elpa" "nongnu-elpa" "melpa")))

(defun emir-dump-database ()
  "Dump the Epkg database as SQL statements to a file."
  (interactive)
  (emacsql-sqlite-dump-database (oref (epkg-db) connection)))

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
         (branch (oref pkg upstream-branch))
         (module (concat "mirror/" name)))
    (with-emir-repository t
      (cl-typecase pkg
        (epkg-wiki-package
         (setq branch name))
        (epkg-nongnu-elpa-package
         (setq branch (concat "elpa/" name)))
        (epkg-gnu-elpa-package
         (setq branch (concat "externals/" name))))
      (magit-git "clone" "--reject-shallow" "--single-branch"
                 (and (emir--ignore-tags-p pkg) "--no-tags")
                 (and branch (list "--branch" branch))
                 origin module)
      (cl-typecase pkg
        (epkg-wiki-package
         (with-emir-repository pkg
           (magit-git "config" "remote.origin.url"
                      (file-relative-name emir-ewiki-repository))))
        (epkg-nongnu-elpa-package
         (with-emir-repository pkg
           (magit-git "config" "remote.origin.url"
                      (file-relative-name emir-nongnu-elpa-repository))))
        (epkg-gnu-elpa-package
         (with-emir-repository pkg
           (magit-git "config" "remote.origin.url"
                      (file-relative-name emir-gnu-elpa-repository)))))
      (magit-git "submodule" "add" "--name" name mirror module)
      (magit-git "submodule" "absorbgitdirs" module))
    (with-emir-repository pkg
      (magit-git "remote" "add" "mirror" mirror)
      (magit-git "config" "remote.pushDefault" "mirror")
      (unless (equal branch "master")
        (magit-git "branch" "--move"
                   (or branch (magit-get-current-branch))
                   "master"))
      (unless branch
        (save-match-data
          (let ((merge (magit-get "branch.master.merge")))
            (if (and merge (string-match "\\`refs/heads/\\(.+\\)" merge))
                (setq branch (match-string 1 merge))
              (error "BUG: No branch checked out")))))
      (cl-typecase pkg
        (epkg-subtree-package
         (magit-git "branch" "--unset-upstream")
         (emir-pull pkg))))
    (oset pkg branch branch)))

(cl-defmethod emir-clone ((pkg epkg-file-package))
  (let* ((name   (oref pkg name))
         (mirror (oref pkg mirror-url))
         (module (concat "mirror/" name)))
    (with-emir-repository t
      (magit-git "init" "--initial-branch" "master" module))
    (emir-pull pkg t)
    (with-emir-repository t
      (magit-git "submodule" "add" "--name" name mirror module)
      (magit-git "submodule" "absorbgitdirs" module))
    (with-emir-repository pkg
      (magit-git "remote" "add" "mirror" mirror)
      (magit-git "config" "remote.pushDefault" "mirror"))))

;;;; Pull

(cl-defmethod emir-pull ((pkg epkg-mirrored-package) &optional force)
  (with-emir-repository pkg
    (magit-git "fetch" "origin"
               (and force '("--tags" "--prune-tags" "--force")))
    (let ((upstream (concat "origin/" (oref pkg branch))))
      (cond
       ((oref pkg patched)
        (magit-git "rebase" "@{upstream}"))
       ((not (zerop (magit-git-exit-code "merge" "--ff-only" upstream)))
        (unless (or force
                    (zerop (magit-git-exit-code "merge-base" "master" upstream)))
          (error "history completely rewritten"))
        (pcase-let* ((`(,ours ,theirs) (magit-rev-diff-count "master" upstream))
                     (msg (format "history rewritten (master %s, %s %s)"
                                  ours upstream theirs)))
          (cond ((or force
                     (< (+ ours theirs) emir-rewrite-threshold)
                     (member (oref pkg upstream-user)
                             emir-rewrite-always-update))
                 (message "Update warning (%s): %s" (oref pkg name) msg)
                 (magit-git "reset" "--hard" upstream)
                 (setq emir--force-push t))
                ((error "%s" msg)))))))))

(cl-defmethod emir-pull ((pkg epkg-file-package) &optional force)
  (with-emir-repository pkg
    (let ((name (oref pkg name)))
      (let ((magit-process-raise-error t))
        (magit-call-process "curl" "-O" (oref pkg url)))
      (when (or (magit-anything-modified-p) force)
        (magit-git "add" ".")
        (let ((process-environment process-environment)
              (mainlib (ignore-errors
                         (elx-main-library
                          default-directory name t t))))
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

(cl-defmethod emir-pull ((pkg epkg-subtree-package) &optional _force)
  (with-emir-repository pkg
    (let ((name (oref pkg name))
          (branch (or (oref pkg upstream-branch)
                      (magit-remote-head (oref pkg url))))) ; FIXME
      (magit-git "fetch" "origin")
      (magit-git "checkout" (concat "origin/" branch))
      (let ((time (current-time)))
        (message "Filtering subtree of %s... (%s)" name
                 (format-time-string "%T"))
        (magit-git "branch" "--force" branch
                   (or (magit-git-string "subtree" "-P"
                                         (oref pkg upstream-tree) "split")
                       (error "git-subtree failed or is missing")))
        (message "Filtering subtree of %s...done (%.0fm)" name
                 (/ (float-time (time-subtract (current-time) time)) 60)))
      (magit-git "checkout" branch))))

(cl-defmethod emir-pull ((pkg epkg-shelved-package) &optional _force)
  (with-emir-repository pkg
    (magit-git "pull" "--ff-only" "mirror" "master")))

;;;; Push

(cl-defmethod emir-push ((pkg epkg-package) &optional force)
  (when (oref pkg mirrored)
    (with-emir-repository pkg
      (magit-git "push"
                 (and (or force emir--force-push (oref pkg patched)) "--force")
                 (and (not (emir--ignore-tags-p pkg)) "--follow-tags")
                 "mirror" "heads/master"))))

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
                   (mapcar (##substring % 6) (magit-staged-files nil "attic"))
                   (mapcar (##substring % 7) (magit-staged-files nil "mirror"))
                   :test #'equal))))
      (format "Update %s package%s" count (if (> count 1) "s" "")))))

;;;; Update

(defun emir--update-branch (pkg &optional default unset-forced)
  (unless (or (cl-typep pkg 'epkg-file-package)     ; no upstream repository
              (cl-typep pkg 'epkg-wiki-package)     ; constant name
              (cl-typep pkg 'epkg-gnu-elpa-package) ; constant name
              (cl-typep pkg 'epkg-subtree-package)) ; no tracked upstream
    (with-emir-repository pkg
      (let ((name (oref pkg name))
            (forced (oref pkg upstream-branch))
            (tracked (oref pkg branch)))
        (unless default
          (setq default
                (or (magit-remote-head "origin")
                    (and-let* ; Use cached value when gitlab takes a nap.
                        ((ref (magit-git-string "symbolic-ref"
                                                "refs/remotes/origin/HEAD")))
                      (substring ref 20))))
          (unless default
            (error "BUG: No default branch for %s" name))
          (when (and forced
                     (not (member
                           forced
                           (or (magit-remote-list-branches "origin")
                               (error "Could not list remote branches")))))
            (setq unset-forced t)))
        (cond
         ((not forced)
          (unless (equal tracked default)
            (message "Updating branch of %s (%S => %S)" name tracked default)
            (emir--update-branch-1 pkg tracked default)
            (emir-dump-database)))
         (unset-forced
          (message "Updating branch of %s (%S (invalid forced) => %S (default))"
                   name forced default)
          (oset pkg upstream-branch nil)
          (emir--update-branch-1 pkg forced default)
          (emir-dump-database))
         ((and forced (not (equal forced tracked)))
          (message "Updating branch of %s (%S (default) => %S (forced))"
                   name default forced)
          (emir--update-branch-1 pkg default forced)
          (emir-dump-database))
         ((equal forced default)
          (message "Updating branch of %s (no longer force default %S)"
                   name default)
          (oset pkg upstream-branch nil)
          (emir-dump-database)))))))

(defun emir--update-branch-1 (pkg old new)
  (magit-git "config" "remote.origin.fetch"
             (format "+refs/heads/%s:refs/remotes/origin/%s" new new))
  (magit-git "fetch" "origin")
  (magit-git "update-ref" "-d" (concat "refs/remotes/origin/" old))
  (magit-git "branch" "master" (concat "--set-upstream-to=origin/" new))
  (magit-git "remote" "set-head" "origin" new)
  (oset pkg branch new))

;;;; Setup

(cl-defmethod emir-setup ((pkg epkg-package))
  (with-emir-repository pkg
    (magit-git "reset" "--hard" "HEAD")
    (magit-git "update-ref" "refs/heads/master" (oref pkg hash))
    (magit-git "checkout" "master")
    (magit-git "remote" "rename" "origin" "mirror")
    (magit-git "config" "remote.pushDefault" "mirror")
    (let ((origin
           (cl-typecase pkg
             (epkg-wiki-package
              (file-relative-name emir-ewiki-repository))
             (epkg-nongnu-elpa-package
              (file-relative-name emir-nongnu-elpa-repository))
             (epkg-gnu-elpa-package
              (file-relative-name emir-gnu-elpa-repository))
             (t (oref pkg url))))
          (branch (oref pkg branch)))
      (cl-typecase pkg
        (epkg-shelved-package
         (magit-git "branch" "--unset-upstream"))
        (epkg-file-package
         (magit-git "branch" "--unset-upstream"))
        (epkg-subtree-package
         (magit-git "branch" "--unset-upstream")
         (magit-git "remote" "add" "-f" "-t" branch "origin" origin "--no-tags"))
        (epkg-wiki-package
         (magit-git "remote" "add" "-f" "-t" branch "origin" origin "--no-tags")
         (magit-git "branch" (concat "--set-upstream-to=origin/" branch)))
        (epkg-gnu-elpa-package
         (magit-git "remote" "add" "-f" "-t" branch "origin" origin "--no-tags")
         (magit-git "branch" (concat "--set-upstream-to=origin/" branch)))
        (t
         (magit-git "remote" "add" "-f" "-t" branch "origin" origin)
         (magit-git "branch" (concat "--set-upstream-to=origin/" branch)))))))

;;; Database
;;;; Add

(cl-defmethod emir-add ((pkg epkg-mirrored-package) &optional replace)
  (emir--set-urls pkg)
  (emir--set-slots pkg)
  (unless replace
    (closql-insert (epkg-db) pkg)
    (emir-gh-init pkg))
  (emir-clone     pkg)
  (emir-push      pkg replace)
  (emir-update    pkg replace)
  (emir-gh-update pkg t))

(defun emir--set-slots (pkg)
  (oset pkg mirror-name
        (string-replace "+" "-plus" (oref pkg name)))
  (when (epkg-orphaned-package-p pkg)
    (oset pkg upstream-user "emacsorphanage"))
  (oset pkg mirror-url (emir--format-url pkg 'mirror-url-format))
  (oset pkg mirrorpage (emir--format-url pkg 'mirrorpage-format))
  (oset pkg repopage   (emir--format-url pkg 'repopage-format))
  (oset pkg homepage   (emir--format-url pkg 'homepage-format)))

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
                      (car (cl-find-if (##string-suffix-p main (car %))
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
            (oset pkg wikipage    (emir--wikipage pkg))
            (when (string-prefix-p "monorepo-" name) ; sad
              (oset pkg summary "A monorepo containing unrelated packages")
              (oset pkg commentary "\
A monorepo containing unrelated packages.  This is only mirrored
because some of these packages are also available from Melpa.")))
        (unless (or (epkg-shelved-package-p pkg)
                    (equal name "emacs"))
          (error "Cannot determine main library"))))
    (pcase-let ((`(,required ,provided)
                 (emir--features pkg)))
      (oset pkg required required)
      (oset pkg provided provided))
    (when-let ((buf (magit-get-mode-buffer 'magit-process-mode)))
      (kill-buffer buf))
    t))

;;; Extract

(cl-defmethod emir--main-library ((pkg epkg-package))
  (or (oref pkg library)
      (let ((name (oref pkg name))
            (load-suffixes '(".el" ".el.in" ".el.tmpl"))
            (load-file-rep-suffixes '("")))
        (or (ignore-errors
              (elx-main-library default-directory name nil t))
            (and (string-prefix-p "monorepo-" name)
                 ;; Roll eyes and pick a random library.
                 (car (elx-libraries default-directory)))
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
  (or (emir--config pkg :homepage)
      (and-let* ((url (lm-website))
                 (url (if (string-suffix-p "/" url)
                          (substring url 0 -1)
                        url)))
        (and (string-match-p "\\`https?://" url) url))
      (emir--format-url pkg 'homepage-format)))

(cl-defmethod emir--wikipage ((pkg epkg-package))
  (and-let*
      ((page (or (and (epkg-wiki-package-p pkg) (elx-wikipage))
                 (emir--config pkg :wikipage)
                 (let* ((norm (lambda (string)
                                (thread-last string
                                  (string-replace "+" "plus")
                                  (string-replace "-" "")
                                  downcase)))
                        (name (funcall norm (oref pkg name)))
                        (alist (with-emir-repository 'epkg-wiki-package
                                 (mapcar (##cons (funcall norm %) %)
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
          provided provided-join provided-full hard soft)
      (dolist (lib (if (epkg-builtin-package-p pkg)
                       (mapcar #'car (oref pkg builtin-libraries))
                     (elx-libraries default-directory)))
        (with-temp-buffer
          (insert-file-contents lib)
          (setq buffer-file-name lib)
          (set-buffer-modified-p nil)
          (with-syntax-table emacs-lisp-mode-syntax-table
            (pcase-let ((`(,h ,s) (elx-required))
                        (p        (elx-provided)))
              (dolist (h h) (cl-pushnew h hard))
              (dolist (s s) (cl-pushnew s soft))
              (dolist (p p) (cl-pushnew p provided))))))
      (setq provided-join
            (mapcan (pcase-lambda (`(,feature ,reason))
                      (and (not (memq feature provided))
                           (list (list feature nil reason))))
                    (epkg-sql [:select [feature join] :from provided
                               :where (and (= package $s1) (notnull join))
                               :order-by [(asc feature)]]
                              name)))
      (setq provided-full (append provided (mapcar #'car provided-join)))
      (list
       (let ((drop (epkg-sql [:select [feature drop] :from required
                              :where (and (= package $s1) (notnull drop))
                              :order-by [(asc feature)]]
                             name)))
         (setq hard (cl-set-difference hard provided-full))
         (setq soft (cl-set-difference soft provided-full))
         (setq soft (cl-set-difference soft hard))
         (nconc (mapcar (##list % t   nil (cadr (assoc % drop))) hard)
                (mapcar (##list % nil nil (cadr (assoc % drop))) soft)))
       (let ((drop (epkg-sql [:select [feature drop] :from provided
                              :where (and (= package $s1) (notnull drop))
                              :order-by [(asc feature)]]
                             name)))
         (nconc (mapcar (##list % (cadr (assoc % drop)) nil) provided)
                provided-join))))))

(defun emir--builtin-packages-alist ()
  (emir-with-emacs-worktree
    (let ((builtins-alist
           (with-temp-buffer
             (insert-file-contents "lisp/finder.el")
             (re-search-forward "^(defvar finder--builtins-alist")
             (goto-char (match-beginning 0))
             (eval (nth 2 (read (current-buffer)))))))
      (mapcar
       (lambda (elt)
         (cons (car elt) (mapcar #'cdr (cdr elt))))
       (seq-group-by
        #'car
        (cl-sort
         (mapcan
          (lambda (file)
            (and (string-suffix-p ".el" file)
                 (if (or (string-match-p finder-no-scan-regexp file)
                         (member file
                                 '(;; Pending [[notmuch-tree:thread:000000000001545a][#62751]]:
                                   ;; - Old versions:
                                   "lisp/obsolete/otodo-mode.el"
                                   ;; - Moved to GNU Elpa:
                                   "lisp/obsolete/crisp.el"
                                   "lisp/obsolete/landmark.el")))
                     (prog1 nil (message "Skipping %s...done" file))
                   (message "Importing %s..." file)
                   (with-temp-buffer
                     (insert-file-contents file)
                     (emacs-lisp-mode)
                     (let ((package
                            (cond
                             ;; Pending [[notmuch-tree:thread:000000000001545a][#62751]]:
                             ((equal file "lisp/use-package/bind-key.el")
                              "bind-key")
                             ;; Properly specified packages:
                             ((and-let* ((elt (assoc (thread-first file
                                                       file-name-directory
                                                       directory-file-name
                                                       file-name-nondirectory)
                                                     builtins-alist)))
                                (symbol-name (cdr elt))))
                             ((lm-header "Package"))
                             ((thread-first file
                                file-name-nondirectory
                                file-name-sans-extension)))))
                       (prog1 (mapcar (##list package file %)
                                      (or (elx-provided)
                                          (list nil)))
                         (message "Importing %s...done" file)))))))
          (magit-git-items "ls-tree" "-z" "-r" "--name-only" "HEAD" "lisp/"))
         #'string< :key #'car))))))

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
  (when (oref pkg mirrored)
    (emir-gh pkg "POST" "/orgs/%o/repos" `((name . ,(oref pkg mirror-name))))
    (emir-gh pkg "WAIT" "/repos/%o/%m")))

(cl-defmethod emir-gh-init ((pkg epkg-github-package))
  (if (condition-case-unless-debug err
          (let-alist (cdr (ghub--graphql-synchronous
                           '(query (repository
                                    [(owner $owner String!)
                                     (name  $name  String!)]
                                    (parent
                                     (forks [(:edges t)
                                             (affiliations ORGANIZATION_MEMBER)]
                                            (owner login)))))
                           `((owner . ,(oref pkg upstream-user))
                             (name  . ,(oref pkg upstream-name)))))
            (seq-some (##equal (let-alist % .owner.login) "emacsmirror")
                      .repository.parent.forks))
        (error (message "Error: %S" err) t))
      (cl-call-next-method)
    (emir-gh pkg "POST" "/repos/%u/%n/forks"
             `((organization . "emacsmirror")
               (name . ,(oref pkg mirror-name))
               (default-branch-only . t)))
    (emir-gh pkg "WAIT" "/repos/%o/%m")))

(cl-defmethod emir-gh-unsubscribe :after ((pkg epkg-package))
  (emir-gh pkg "DELETE" "/repos/%o/%m/subscription"
           :errorback (lambda (err &rest _)
                        (message "Error: Failed to unsubscribe from %s/%s: %S"
                                 (oref pkg upstream-user)
                                 (oref pkg upstream-name) err))))

(cl-defmethod emir-gh-update ((pkg epkg-package) &optional _clone)
  (when (oref pkg mirrored)
    (emir-gh pkg "PATCH" "/repos/%o/%m"
             `((name           . ,(oref pkg mirror-name))
               (description    . ,(oref pkg summary))
               (homepage       . ,(oref pkg homepage))
               (has_issues     . nil)
               (has_wiki       . nil)
               (has_downloads  . nil))
             :errorback
             (lambda (err &rest _)
               (message "Error: Failed to publish metadata for %s: %S"
                        (oref pkg mirror-name) err)))))

(cl-defmethod emir-gh-update :after ((pkg epkg-github-package) &optional clone)
  (when clone
    (with-emir-repository pkg
      (when-let ((branches
                  (delete "master" (magit-list-remote-branches "mirror"))))
        (magit-git "push" "mirror" (mapcar (##concat ":" %) branches))))))

(cl-defmethod emir-gh-delete ((pkg epkg-package))
  (when (oref pkg mirrored)
    (emir-gh pkg "DELETE" "/repos/%o/%m")))

(defun emir-gh-foreach-query (query callback &optional per-page packages)
  (letrec ((result nil)
           (page 0)
           (pages
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
           (len (length pages))
           (run (lambda (&optional data _headers _status _req)
                 (setq result (nconc result (cdar data)))
                 (cond
                  (pages
                   (message "Fetching page...%s/%s" (cl-incf page) len)
                   (ghub-graphql
                    (gsexp-encode
                     (ghub--graphql-prepare-query
                      (cons 'query (pop pages))))
                    nil :callback run :auth 'emir))
                  (t
                   (message "Fetching page...done")
                   (dolist (elt result)
                     (setcar elt (base64-decode-string
                                  (string-replace
                                   "_" "="
                                   (substring (symbol-name (car elt)) 1)))))
                   (funcall callback result))))))
    (funcall run)))

;;; Urls

(cl-defmethod emir--set-urls ((pkg epkg-mirrored-package))
  (when-let ((url (oref pkg url)))
    (let ((conflict (and url (emir--lookup-url url)))
          (name (oref pkg name)))
      (when (and conflict
                 (not (equal conflict name))
                 (not (emir--config name :sharing-repo))
                 (not (cl-typep pkg 'epkg-wiki-package))
                 (not (cl-typep pkg 'epkg-gnu-elpa-package)))
        (user-error "Another package, %s, is already mirrored from %s"
                    conflict url)))
    (when-let ((url-format (oref pkg url-format)))
      (pcase-dolist (`(,slot . ,value) (emir--match-url url-format url))
        (when (and (eq slot 'upstream-name)
                   (string-suffix-p ".git" value))
          (setq value (substring value 0 -4)))
        (eieio-oset pkg slot value))))
  (when-let ((url (emir--format-url pkg 'url-format)))
    (oset pkg url url)))

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
                             '(;; epkg-package
                               (?m . mirror-name)
                               (?n . upstream-name)
                               (?u . upstream-user)
                               ;; epkg-*elpa-recipe
                               (?r . repo)))))
            (push slot slots)
            (replace-match (if (memq char '(?u ?r))
                               "\\(.+\\)"
                             "\\([^/]+?\\)")
                           t t)))
        (and (string-match (concat "\\`" (buffer-string) "\\'") url)
             (let ((i 0))
               (mapcar (##cons % (match-string (cl-incf i) url))
                       (nreverse slots))))))))

(defun emir--url-to-class (url &optional base-class)
  (cl-find-if (lambda (class)
                (ignore-errors
                  (emir--match-url (oref-default class url-format) url)))
              (closql--list-subclasses (or base-class 'epkg-package))))

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

(defun emir--config (pkg keyword)
  (caar (epkg-sql [:select [v] :from $i1 :where (= k $s2)]
                  (intern (concat "var-" (substring (symbol-name keyword) 1)))
                  (if (stringp pkg) pkg (oref pkg name)))))

(defun emir--store-table (table data)
  (epkg-sql [:create-table-if-not-exists $i1 $S2] table [k v])
  (emacsql-with-transaction (epkg-db)
    (let ((old (cl-sort (epkg-sql [:select * :from $i1] table)
                        #'string< :key #'car))
          (new (cl-sort data #'string< :key #'car)))
      (while (or old new)
        (pcase-let ((`(,okey ,oval) (car old))
                    (`(,nkey ,nval) (car new)))
          (cond
           ((and okey (or (not nkey) (string< okey nkey)))
            (epkg-sql [:delete-from $i1 :where (= k $s2)]
                      table okey)
            (pop old))
           ((string= okey nkey)
            (unless (equal oval nval)
              (epkg-sql [:update $i1 :set (= v $s2) :where (= k $s3)]
                        table nval nkey))
            (pop old)
            (pop new))
           (t
            (epkg-sql [:insert-into $i1 :values $v2]
                      table (vector nkey nval))
            (pop new)))))))
  (emir-dump-database))

(defun emir--ignore-tags-p (pkg)
  (or (cl-typep pkg 'epkg-subtree-package)
      (cl-typep pkg 'epkg-wiki-package)
      (cl-typep pkg 'epkg-gnu-elpa-package)))

(defun emir--assert-clean-worktree ()
  (cond ((not (zerop (magit-git-exit-code "diff" "--quiet")))
         (error "unstaged changes"))
        ((not (zerop (magit-git-exit-code "diff" "--quiet" "--cached")))
         (error "uncommitted changes"))
        ;; This may happen due to an earlier failed push.
        ;; ((not (magit-rev-eq "HEAD" "mirror/master"))
        ;;  (error "unpushed changes"))
        ((not (equal (magit-get-current-branch) "master"))
         (error "wrong branch checked out"))))

;;; _
(provide 'emir)
(require 'emir-gelpa)
(require 'emir-melpa)
(require 'emir-utils)
;;; emir.el ends here
