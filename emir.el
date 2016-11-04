;;; emir.el --- maintain the Emacsmirror          -*- lexical-binding: t -*-

;; Copyright (C) 2016  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: https://gitlab.com/tarsius/emir
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
(require 'finder)
(require 'ghub)
(require 'magit)
(require 'org)
(require 'packed)

(defvar finder-no-scan-regexp)
(defvar finder--builtins-alist)

(defconst emir--dummy-package "emir--dummy-package")

(cl-pushnew (expand-file-name
             "bin" (file-name-directory (or load-file-name buffer-file-name)))
            exec-path :test #'equal)

;;; Options

(defgroup emir nil
  "Maintain the Emacsmirror."
  :group 'local)

(defcustom emir-emacs-repository nil
  "The Emacs repository used to extract builtin packages."
  :group 'emir
  :type '(choice directory (const nil)))

(defcustom emir-ignored-dependencies nil
  "List of dependencies that are ignored in all packages."
  :group 'emir
  :type '(repeat (list (symbol :tag "Name")
                       (string :tag "Reason"))))

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
  "List of packages which are temporarily not being updated."
  :group 'emir
  :type '(repeat (list (string :tag "Name")
                       (string :tag "Reason"))))

(defcustom emir-preferred-upstreams nil
  "List of packages with multiple potential upstreams."
  :group 'emir
  :type '(repeat (list (string :tag "Name")
                       (string :tag "Reason"))))

(defcustom emir-renamed-files nil
  "Alist of files that have to be renamed after fetching with curl.
This variable should only be used as a last resort."
  :group 'emir
  :type '(repeat (list (string :tag "Name")
                       (string :tag "Filename"))))

;;; Repository

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
  (expand-file-name "import/gelpa/" epkg-repository))

(cl-defmethod epkg-repository ((_class (subclass epkg-elpa-branch-package)))
  (expand-file-name "import/gelpa/" epkg-repository))

(cl-defmethod epkg-repository ((_class (subclass epkg-wiki-package)))
  (expand-file-name "import/wiki/" epkg-repository))

;;; Add
;;;; Add Package

;;;###autoload
(defun emir-add-package (name url class &rest plist)
  (interactive
   (let* ((url  (emir-read-url "Add package from url"))
          (type (epkg-read-type "Package type: "
                                (--when-let (emir--url-to-class url)
                                  (substring (symbol-name it) 5 -8))))
          (name (magit-read-string
                 "Package name"
                 (->> (or (emir--url-get url 'upstream-name) "")
                      (replace-regexp-in-string "\\`emacs-" "")
                      (replace-regexp-in-string "\\`elisp-" "")
                      (replace-regexp-in-string "[.-]el\\'" "")))))
     (list name url (intern (format "epkg-%s-package" type)))))
  (let ((pkg (apply class :name name :url url plist)))
    (unless (epkg-wiki-package-p pkg)
      (emir--assert-unknown name url))
    (with-epkg-repository t (emir-add pkg))
    (with-epkg-repository t (magit-call-git "add" "epkg.sqlite")))
  (emir--sort-submodule-sections))

(cl-defmethod emir-add ((pkg epkg-mirrored-package))
  (emir-init      pkg)
  (emir-gh-init   pkg)
  (emir-clone     pkg)
  (emir-push      pkg)
  (emir-update    pkg)
  (emir-gh-update pkg)
  (emir--stage    pkg))

(cl-defmethod emir-add :after ((pkg epkg-github-package))
  (emir-gh-prune pkg))

(cl-defmethod emir-add ((pkg epkg-subset-package))
  (emir-init      pkg)
  (emir-gh-init   pkg)
  (emir-import    pkg)
  (emir-push      pkg)
  (emir-clone     pkg)
  (emir-update    pkg)
  (emir-gh-update pkg))

(cl-defmethod emir-add ((pkg epkg-subtree-package))
  (emir-init      pkg)
  (emir-gh-init   pkg)
  (emir-clone     pkg)
  (emir-pull      pkg)
  (emir-push      pkg)
  (emir-update    pkg)
  (emir-gh-update pkg))

(cl-defmethod emir-add ((pkg epkg-builtin-package) &optional recreate)
  (unless recreate
    (closql-insert (epkg-db) pkg))
  (emir-update pkg))

(cl-defmethod emir-init ((pkg epkg-package) &optional recreate)
  (unless recreate
    (closql-insert (epkg-db) pkg))
  (with-slots
      (url mirror-url upstream-user upstream-name mirror-name name mirrorpage)
      pkg
    (when url
      (emir--set-url pkg url))
    (unless url
      (setf url (emir--format-url pkg 'url-format)))
    (when (or (not mirror-name) (string-match-p "\\+" mirror-name))
      (setf mirror-name (replace-regexp-in-string "\\+" "-plus" name)))
    (unless upstream-name
      (setf upstream-name name))
    (when (epkg-orphaned-package-p pkg)
      (setf upstream-user "emacsorphanage"))
    (setf mirror-url (emir--format-url pkg 'mirror-url-format))
    (setf mirrorpage (format "https://github.com/%s/%s"
                             (if (epkg-shelved-package-p pkg)
                                 "emacsattic"
                               "emacsmirror")
                             mirror-name))))

(defun emir--set-url (pkg url)
  (oset pkg url
        (if (oref pkg url-format)
            (let ((i 0))
              (dolist (slot (emir--match-url (oref pkg url-format) url))
                (setf (slot-value pkg slot)
                      (match-string (cl-incf i) url)))
              (emir--format-url pkg 'url-format))
          url)))

(defun emir--format-url (pkg slot)
  (--when-let (eieio-oref-default pkg slot)
    (format-spec it `((?p . ,(oref pkg name))
                      (?m . ,(oref pkg mirror-name))
                      (?n . ,(oref pkg upstream-name))
                      (?u . ,(oref pkg upstream-user))))))

(defun emir--match-url (format url &optional lax)
  (with-temp-buffer
    (insert (regexp-quote format))
    (goto-char (point-min))
    (let (slots)
      (while (re-search-forward "%\\(.\\)" nil t)
        (push (cdr (assq (string-to-char (match-string 1))
                         '((?N . mirror-name)
                           (?n . upstream-name)
                           (?u . upstream-user))))
              slots)
        (replace-match "\\([^/]+\\)" t t))
      ;; The side-effect and the return value.
      (and (string-match (concat "^" (buffer-string) (unless lax "$")) url)
           (nreverse slots)))))

(defun emir--url-to-class (url)
  (--first (ignore-errors (emir--match-url (oref-default it url-format) url))
           (closql--list-subclasses 'epkg-package)))

(defun emir--url-get (url slot)
  (-when-let* ((class (emir--url-to-class url))
               (slots (emir--match-url (oref-default class url-format) url))
               (value (cl-position slot slots)))
    (match-string (1+ value) url)))

(defun emir--assert-unknown (name url)
  (--if-let (epkg name)
      (cl-typecase it
        (epkg-builtin-package (user-error "Package %s is already built-in" name))
        (epkg-shelved-package (user-error "Package %s is already shelved"  name))
        (t                    (user-error "Package %s is already mirrored" name)))
    (when (assoc name emir-pending-packages)
      (user-error "Package %s is pending" name)))
  (--when-let (and url (cadr (assoc url (epkgs [url name]))))
    (user-error "Another package, %s, is already mirrored from %s" it url))
  (let ((repo (expand-file-name (concat ".git/modules/" name) epkg-repository)))
    (when (file-exists-p repo)
      (user-error "Package %s used to be mirrored.  Remove %s first"
                  name repo))))

;;;; Add Packages

;;;###autoload
(defun emir-add-elpa-packages (&optional dry-run)
  (interactive "P")
  (emir-pull 'epkg-elpa-package)
  (dolist (name (emir--list-packages 'epkg-elpa-package))
    (unless (epkg name)
      (--if-let (or (assoc name emir-pending-packages)
                    (and (string-match-p "theme" name) (list nil "theme")))
          (message "Skipping %s (%s)...done" name (cadr it))
        (message "Adding %s..." name)
        (emir--assert-unknown name nil)
        (unless dry-run
          (emir-add (epkg-elpa-package :name name)))
        (message "Adding %s...done" name))))
  (unless dry-run
    (emir--commit "add %n elpa %p")))

;;;###autoload
(defun emir-add-elpa-branch-packages (&optional dry-run)
  (interactive "P")
  (emir-pull 'epkg-elpa-package)
  (dolist (name (emir--list-packages 'epkg-elpa-branch-package))
    (unless (epkg name)
      (--if-let (assoc name emir-pending-packages)
          (message "Skipping %s (%s)...done" name (cadr it))
        (message "Adding %s..." name)
        (emir--assert-unknown name nil)
        (unless dry-run
          (emir-add (epkg-elpa-branch-package :name name)))
        (message "Adding %s...done" name))))
  (unless dry-run
    (emir--commit "add %n elpa-branch %p")))

;;;###autoload
(defun emir-add-builtin-packages (&optional dry-run)
  (interactive "P")
  (dolist (name (epkg-sql [:select :distinct name :from builtin-packages
                           :order-by (asc name)]))
    (setq  name (car name))
    (unless (epkg name)
      (--if-let (assoc name emir-pending-packages)
          (message "Skipping %s (%s)...done" name (cadr it))
        (message "Adding %s..." name)
        (unless dry-run
          (emir-add (epkg-builtin-package :name name :mirror-name name))
          (emir--commit "add %n builtin %p"))
        (message "Adding %s...done" name)))))

;;;###autoload
(defun emir-add-melpa-packages (&optional dry-run)
  (interactive "P")
  (let ((mirrored (epkgs 'url)))
    (pcase-dolist (`(,name ,fetcher ,url ,branch)
                   (epkg-sql [:select [name fetcher url branch]
                              :from melpa-recipes
                              :order-by [(asc name)]]))
        (unless (or (epkg name)
                    ;; (not url)
                    (memq fetcher '(bzr cvs darcs fossil svn))
                    (member url mirrored)
                    (assoc name emir-pending-packages)
                    (assoc name emir-secondary-packages))
          (if branch ; Probably okay, but needs special attention.
              (user-error "Melpa imports %s from branch %s" name branch)
            (message "Adding %s..." name)
            (unless dry-run
              (emir-add-package name url
                                (intern (format "epkg-%s-package" fetcher))))
            (message "Adding %s...done" name))))
    (emir-import-melpa-recipes)
    (emir--commit "add %n %p")))

;;; Update
;;;; Update Package

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
          (emir--stage pkg)
          (when (or force (not (equal (oref pkg hash) tip)))
            (emir-gh-update pkg)
            (emir-push pkg)))
      (magit-git-error
       (push (oref pkg name) emir-failed-updates)
       (message "%s" (error-message-string err)))
      (epg-error
       (push (oref pkg name) emir-failed-updates)
       (signal (car err) (cdr err)))
      (error
       (push (oref pkg name) emir-failed-updates)
       (message "%s" (error-message-string err))))))

(cl-defmethod emir-update ((pkg epkg-package))
  (with-epkg-repository pkg
    (with-slots (name hash libraries library) pkg
      (setf hash (magit-rev-parse "HEAD"))
      (when (epkg-builtin-package-p pkg)
        (setf libraries
              (mapcar #'car (epkg-sql [:select library :from builtin-packages
                                       :where (= name $s1)]
                                      name)))
        (setf library
              (pcase name
                ("emacs" nil)
                ("nxml" "lisp/nxml/nxml-mode.el")
                (_ (let ((main (concat "/" name ".el")))
                     (--first (string-suffix-p main it) libraries))))))
      (--if-let (or library
                    (ignore-errors
                      (packed-main-library default-directory name nil t)))
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
            (oset pkg repopage    (emir--repopage pkg))
            (oset pkg homepage    (emir--homepage pkg))
            (oset pkg wikipage    (emir--wikipage pkg)))
        (unless (or (epkg-shelved-package-p pkg)
                    (equal name "emacs"))
          (error "Cannot determine main library"))))
    (emir--set-features pkg)
    (--when-let (magit-mode-get-buffer 'magit-process-mode)
      (kill-buffer it))))

(define-error 'epg-error "GPG error")

;;;; Update Packages

;;;###autoload
(defun emir-update-packages (&optional predicate from message)
  (interactive (list nil (car (emir-update-read-args)) "update %n %p"))
  (dolist (pkg (epkgs nil (or predicate 'epkg-mirrored-package)))
    (with-slots (name) pkg
      (when (or (not from) (string< from name))
        (if (assoc name emir-suspended-packages)
            (message "Skipping suspended %s" name)
          (condition-case err
              (progn (message "Updating %s..." name)
                     (emir-update-package name)
                     (when message
                       (emir--commit message))
                     (message "Updating %s...done" name))
            (error (message "Updating %s...failed %s" name
                            (error-message-string err)))))))))

;;;###autoload
(defun emir-update-git-packages (&optional from)
  (interactive (emir-update-read-args))
  (emir-update-packages 'epkg-git-package--eieio-childp from)
  (emir--commit "update %n git %p"))

;;;###autoload
(defun emir-update-hg-packages (&optional from)
  (interactive (emir-update-read-args))
  (emir-update-packages 'epkg-hg-package--eieio-childp from)
  (emir--commit "update %n hg %p"))

;;;###autoload
(defun emir-update-file-packages (&optional from)
  (interactive (emir-update-read-args))
  (emir-update-packages 'epkg-file-package from)
  (emir--commit "update %n file %p"))

;;;###autoload
(defun emir-update-wiki-packages (&optional from)
  (interactive (emir-update-read-args))
  (if (y-or-n-p "Did you run `emir-import-wiki-packages' before this? ")
      (progn (emir-update-packages 'epkg-wiki-package from)
             (emir--commit "update %n wiki %p"))
    (user-error "Well, then do that now")))

;;;###autoload
(defun emir-update-elpa-packages (&optional from)
  (interactive (emir-update-read-args))
  (emir-pull   'epkg-elpa-package)
  (emir-import 'epkg-elpa-package)
  (emir-update-packages 'epkg-elpa-package from)
  (emir--commit "update %n elpa %p"))

;;;###autoload
(defun emir-update-elpa-branch-packages (&optional from)
  (interactive (emir-update-read-args))
  (emir-pull   'epkg-elpa-branch-package)
  (emir-update-packages 'epkg-elpa-branch-package from)
  (emir--commit "update %n elpa-branch %p"))

(defun emir-update-read-args ()
  (when current-prefix-arg
    (list (epkg-read-package "Limit to packages after: "))))

;;; Recreate

;;;###autoload
(defun emir-recreate-package (package)
  (interactive (list (epkg-read-package "Recreate package: ")))
  (let ((pkg (epkg package)))
    (emacsql-with-transaction (epkg-db)
      (when (cl-typep pkg 'epkg-mirrored-package)
        (emir-init pkg t))
      (emir-update pkg))))

;;;###autoload
(defun emir-recreate-packages (&optional filter from)
  (interactive (cons nil (emir-update-read-args)))
  (dolist (pkg (epkgs nil filter))
    (with-slots (name) pkg
      (when (or (not from) (string< from name))
        (message "Recreating package %s..." name)
        (emir-recreate-package name)
        (message "Recreating package %s...done" name)))))

;;;###autoload
(defun emir-recalculate-features (&optional filter)
  (interactive)
  (let ((pkgs (epkgs nil filter)))
    (dolist (pkg pkgs)
      (with-slots (name) pkg
        (message "Recalculating features for %s..." name)
        (emir--set-features pkg)
        (message "Recalculating features for %s...done" name)))))

;;;###autoload
(defun emir-recalculate-builtin-features ()
  (interactive)
  (emir-recalculate-features 'epkg-builtin-package))

;;; Remove

;;;###autoload
(defun emir-remove-package (name)
  (interactive (list (epkg-read-package "Remove package: " t)))
  (let ((pkg (epkg name)))
    (unless (epkg-builtin-package-p pkg)
      (with-epkg-repository t
        (magit-git "add" ".gitmodules")
        (let ((module-dir (epkg-repository pkg)))
          (when (file-exists-p module-dir)
            (magit-git "rm" "-f" module-dir))))
      (with-demoted-errors "Error: %S"
        (ghub-delete (format "/repos/%s/%s"
                             (if (epkg-shelved-package-p pkg)
                                 "emacsattic"
                               "emacsmirror")
                             (oref pkg mirror-name)))))
    (closql-delete (epkg-db) pkg)
    (with-epkg-repository t
      (magit-call-git "add" "epkg.sqlite"))
    (when (epkg-wiki-package-p pkg)
      (with-epkg-repository 'epkg-wiki-package
        (magit-call-git "branch" "-D" name)))
    (with-demoted-errors "Error: %S"
      (delete-directory
       (expand-file-name (concat ".git/modules/" name) epkg-repository) t))))

;;; Shelve

;;;###autoload
(defun emir-shelve-package (name)
  (interactive (list (epkg-read-package "Shelve package: " t)))
  (let ((pkg (epkg name)))
    (with-demoted-errors "Error: %S"
      (ghub-delete (format "/repos/emacsmirror/%s" (oref pkg mirror-name))))
    (with-epkg-repository pkg
      (magit-git "reset" "--hard" "HEAD"))
    (with-epkg-repository t
      (magit-git "mv"
                 (concat "mirror/" name)
                 (concat "attic/" name)))
    (closql--set-object-class (epkg-db) pkg 'epkg-shelved-package)
    (emir-init pkg t)
    (with-slots (mirror-url) pkg
      (with-epkg-repository t
        (magit-git "config" "-f" ".gitmodules"
                   (concat "submodule." name ".url") mirror-url)
        (magit-git "add" ".gitmodules" "epkg.sqlite"))
      (with-epkg-repository pkg
        (magit-git "remote" "rm" "mirror")
        (magit-git "remote" "add" "attic" mirror-url)))
    (emir-update  pkg)
    (emir-gh-init pkg)
    (emir-push    pkg)))

(cl-defmethod closql--set-object-class ((db epkg-database) obj class)
  (let* ((primary-table (closql--oref-default db 'primary-table))
         (primary-key   (closql--oref-default db 'primary-key))
         (object-id (closql--oref obj primary-key)))
    (aset obj 0 (closql--public-to-class db class))
    (emacsql db [:update $i1 :set (= class $s2) :where (= $i3 $s4)]
             primary-table
             (closql--class-to-sql db class)
             primary-key object-id)))

(cl-defmethod closql--public-to-class ((_db epkg-database) value)
  (intern (format "eieio-class-tag--%s" value)))

;;; Migrate

;;;###autoload
(defun emir-migrate-github-package (name url)
  (interactive (list (epkg-read-package "Migrate: ")
                     (emir-read-url "Url")))
  (let ((pkg (epkg name)))
    (oset pkg url url)
    (emir-init      pkg t)
    (emir-update    pkg)
    (emir-gh-update pkg)))

;;; Clone

(cl-defmethod emir-clone ((pkg epkg-mirrored-package))
  (with-slots (name url upstream-branch mirror-url) pkg
    (with-epkg-repository t
      (magit-git "submodule" "add" "--name" name
                 "-b" (or upstream-branch "master")
                 url (concat "mirror/" name))
      (magit-git "config" "-f" ".gitmodules"
                 (concat "submodule." name ".url") mirror-url))
    (with-epkg-repository pkg
      (when upstream-branch
        (magit-git "branch" "-M" upstream-branch "master"))
      (magit-git "remote" "add" "mirror" mirror-url)
      (magit-call-git "fetch" "mirror"))))

(cl-defmethod emir-clone :before ((pkg epkg-hg-package))
  ;; `git submodule' doesn't want to use `git remote-hg'
  (with-slots (name url upstream-branch mirror-url) pkg
    (with-epkg-repository t
      (magit-git "clone" "-b" (or upstream-branch "master")
                 url (concat "mirror/" name)))))

(cl-defmethod emir-clone ((pkg epkg-file-package))
  (with-slots (name mirror-url) pkg
    (with-epkg-repository t
      (magit-git "init" (concat "mirror/" name)))
    (emir-pull pkg t)
    (with-epkg-repository t
      (magit-git "submodule" "add" "--name" name
                 mirror-url (concat "mirror/" name)))
    (with-epkg-repository pkg
      (magit-git "remote" "add" "mirror" mirror-url)
      (magit-git "config" "branch.master.remote" "mirror")
      (magit-git "config" "branch.master.merge" "refs/heads/master"))))

(cl-defmethod emir-clone :after ((pkg epkg-subtree-package))
  (with-epkg-repository pkg
    (magit-git "checkout" "--detach" "HEAD")
    (magit-git "branch" "-D" "master")))

(cl-defmethod emir-clone ((pkg epkg-subset-package))
  (with-slots (name mirror-url) pkg
    (with-epkg-repository t
      (magit-git "submodule" "add" "--name" name "-b" "master"
                 mirror-url (concat "mirror/" name)))
    (with-epkg-repository pkg
      (magit-git "remote" "rename" "origin" "mirror")
      (magit-git "remote" "add" "import"
                 (concat "../../import/"
                         (if (epkg-wiki-package-p pkg) "wiki" "gelpa")))
      (magit-git "config" "remote.import.fetch"
                 (format "refs/heads/%s%s:refs/remotes/import/master"
                         (pcase (eieio-object-class pkg)
                           ('epkg-wiki-package "")
                           ('epkg-elpa-package "directory/")
                           ('epkg-elpa-branch-package "externals/"))
                         name))
      (magit-git "fetch"  "import")
      (magit-git "config" "--remove-section" "branch.master")
      (magit-git "branch" "--set-upstream-to=import/master"))))

(cl-defmethod emir-clone ((pkg epkg-mocking-package))
  (with-slots (name mirror-url) pkg
    (let ((archive (if (epkg-shelved-package-p pkg) "attic" "mirror")))
      (with-epkg-repository t
        (magit-git "submodule" "add" "--name" name "-b" "master"
                   mirror-url (concat archive "/" name)))
      (with-epkg-repository pkg
        (magit-git "remote" "rename" "origin" archive)))))

;;; Pull

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
    (magit-git "subtree"  "-P" (oref pkg upstream-tree) "push" "." "master")
    (magit-git "checkout" "master")))

(cl-defmethod emir-pull ((pkg epkg-subset-package))
  (with-epkg-repository pkg
    (magit-git "pull" "--ff-only" "import")))

(cl-defmethod emir-pull ((pkg epkg-elpa-branch-package))
  (with-epkg-repository pkg
    (magit-git "pull" "--ff-only" "import"
               (concat "externals/" (oref pkg name)))))

(cl-defmethod emir-pull ((class (subclass epkg-subset-package)))
  (with-epkg-repository class
    (magit-git "checkout" "master")
    (magit-git "pull" "--ff-only" "origin")))

;;; Push

(cl-defmethod emir-push ((pkg epkg-mirrored-package))
  (with-epkg-repository pkg
    (magit-git "push" "mirror")))

(cl-defmethod emir-push ((pkg epkg-subset-package))
  (with-slots (name mirror-name mirror-url) pkg
    (let ((class (eieio-object-class pkg)))
      (with-epkg-repository class
        (magit-git "push" mirror-url
                   (format (pcase class
                             ('epkg-wiki-package                  "%s:master")
                             ('epkg-elpa-package        "directory/%s:master")
                             ('epkg-elpa-branch-package "externals/%s:master"))
                           name))))))

(cl-defmethod emir-push ((pkg epkg-shelved-package))
  (with-epkg-repository pkg
    (magit-git "push" "attic")))

;;; Extract

(cl-defmethod emir--updated ((_pkg epkg-package))
  (replace-regexp-in-string
   "-" "" (substring (magit-rev-format "%ci" "HEAD") 0 10)))

(cl-defmethod emir--updated ((_pkg epkg-file-package))
  (or (elx-updated)
      (cl-call-next-method)))

(cl-defmethod emir--updated ((_pkg epkg-builtin-package)))

(cl-defmethod emir--repopage ((pkg epkg-package))
  (emir--format-url pkg 'repopage-format))

(cl-defmethod emir--homepage ((pkg epkg-package))
  (or (caar (epkg-sql [:select [page] :from pkg-homepages
                       :where [(= package $s1)]]
                      (oref pkg name)))
      (--when-let (lm-homepage)
        (if (string-match-p "/$" it)
            (substring it 0 -1)
          it))
      (emir--format-url pkg 'homepage-format)))

(cl-defmethod emir--wikipage ((pkg epkg-package))
  (--when-let (or (and (epkg-wiki-package-p pkg) (elx-wikipage))
                  (caar (epkg-sql [:select [page] :from pkg-wikipages
                                   :where [(= package $s1)]]
                                  (oref pkg name)))
                  (let ((name (emir--normalize-wikipage (oref pkg name))))
                    (or (caar (epkg-sql [:select [page] :from raw-wikipages
                                         :where (= normalized $s1)] name))
                        (caar (epkg-sql [:select [page] :from raw-wikipages
                                         :where (= normalized $s1)]
                                (if (string-match-p "mode$" name)
                                    (substring name 0 -4)
                                  (concat name "mode")))))))
    (concat "http://emacswiki.org/" it)))

(defun emir--authors ()
  (cl-delete-duplicates (--map (list (car it) (cdr it)) (elx-authors))
                        :test #'equal :key #'car))

(defun emir--maintainers ()
  (cl-delete-duplicates (--map (list (car it) (cdr it)) (elx-maintainers))
                        :test #'equal :key #'car))

;;; Features

(cl-defmethod emir--set-features ((pkg epkg-package))
  (with-epkg-repository pkg
    (let ((enable-local-variables nil)
          (name (oref pkg name))
          provided hard soft)
      (dolist (lib (if (epkg-builtin-package-p pkg)
                       (oref pkg libraries)
                     (packed-libraries default-directory name)))
        (with-temp-buffer
          (insert-file-contents lib)
          (setq buffer-file-name lib)
          (set-buffer-modified-p nil)
          (with-syntax-table emacs-lisp-mode-syntax-table
            (-let [(h s) (packed-required)]
              (--each h (cl-pushnew it hard))
              (--each s (cl-pushnew it soft))
              (--each (packed-provided)
                (cl-pushnew it provided))))))
      (emacsql-with-transaction (epkg-db)
        (let ((drop (emir--lookup-feature 'provided 'drop t name))
              (join (emir--lookup-feature 'provided 'join t name)))
          (oset pkg provided
                (nconc
                 (--map (list it (and (memq it drop) t) nil)
                        provided)
                 (--map (progn (push it provided)
                               (list it nil t))
                        (-difference join provided)))))
        (let ((drop (emir--lookup-feature 'required 'drop t name))
              (ease (emir--lookup-feature 'required 'ease t name)))
          (setq hard (-difference   hard provided))
          (setq soft (-difference   soft provided))
          (setq soft (-difference   soft hard))
          (setq ease (-intersection ease hard))
          (setq soft (-union        soft ease))
          (setq hard (-difference   hard ease))
          (oset pkg required
                (nconc
                 (--map (list it t nil
                              (or (and (assq it emir-ignored-dependencies) 'global)
                                  (and (memq it drop) t)))
                        hard)
                 (--map (list it nil
                              (and (memq it ease) t)
                              (or (and (assq it emir-ignored-dependencies) 'global)
                                  (and (memq it drop) t)))
                        soft))))
        ))))

(defun emir--lookup-feature (table column value name)
  (sort (mapcar #'car (epkg-sql [:select [feature] :from $i1
                                 :where (and (= $i2 $s3)
                                             (= package $s4))]
                        table column value name))
        #'string<))

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
        (name (oref pkg name)))
    (with-demoted-errors
        (format "Failed to unsubscribe from %s/%s: %%S" org name)
      (ghub-delete (format "/repos/%s/%s/subscription" org name)))))

(cl-defmethod emir-gh-update ((pkg epkg-package))
  (let ((org (if (epkg-shelved-package-p pkg) "emacsattic" "emacsmirror")))
    (with-demoted-errors
        (format "Failed to update metadata for %s/%s: %%S" org
                (oref pkg mirror-name))
      (ghub-patch (format "/repos/%s/%s" org (oref pkg mirror-name))
                  nil `((name           . ,(oref pkg mirror-name))
                        (description    . ,(oref pkg summary))
                        (homepage       . ,(oref pkg homepage))
                        (has_issues     . nil)
                        (has_wiki       . nil)
                        (has_downloads  . nil)
                        (default_branch . "master"))))))

(cl-defmethod emir-gh-prune ((pkg epkg-github-package))
  (with-epkg-repository pkg
    (--when-let (delete "master" (magit-list-remote-branches "mirror"))
      (magit-git "push" "mirror" (--map (concat ":" it) it)))))

;;; Import
;;;; Wiki

;;;###autoload
(defun emir-import-wiki-packages ()
  (interactive)
  (emir-pull   'epkg-wiki-package)
  (emir-import 'epkg-wiki-package))

;;;; Emacs

;;;###autoload
(defun emir-import-builtin-libraries ()
  (interactive)
  (message "Importing builtin libraries...")
  (let ((recipes (make-hash-table :test #'equal :size 500)))
    (dolist (file (emir--list-builtin-libraries))
      (message "Importing %s..." file)
      (pcase-dolist (`(,epkg-name . ,recipe)
                     (emir-emacs--libraries file))
        (push recipe (gethash epkg-name recipes)))
      (message "Importing %s...done" file))
    (message "Importing builtin libraries...")
    (emir--insert-recipes 'builtin-libraries recipes)
    (message "Importing builtin libraries...done")))

(defun emir-emacs--libraries (file)
  (with-temp-buffer
    (insert-file-contents (expand-file-name file emir-emacs-repository))
    (let* ((features (packed-provided))
           (name (cond ((not features) "emacs")
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
                            file-name-sans-extension))))
           (epkg (epkg name)))
      (mapcar (lambda (feature)
                (list (if epkg name emir--dummy-package)
                      file feature name))
             (or features (list nil))))))

(defun emir--list-builtin-libraries ()
  (let ((default-directory emir-emacs-repository))
    (-filter (lambda (file)
               (and (string-match-p "\\.el$" file)
                    (not (string-match-p finder-no-scan-regexp file))
                    (not (member file
                                 '("lisp/gnus/.dir-locals.el"
                                   ;; Old versions:
                                   "lisp/obsolete/old-emacs-lock.el"
                                   "lisp/obsolete/old-whitespace.el"
                                   "lisp/obsolete/otodo-mode.el"
                                   ;; Moved to GNU Elpa:
                                   "lisp/obsolete/crisp.el"
                                   "lisp/obsolete/landmark.el")))))
             (magit-git-items "ls-tree" "-z" "-r" "--name-only"
                              "HEAD" "lisp/"))))

;;;; Melpa

;;;###autoload
(defun emir-import-melpa-recipes (&optional fetch)
  (interactive (list (not current-prefix-arg)))
  (when fetch
    (message "Fetching Melpa recipes...")
    (magit-git "checkout" "master")
    (magit-git "clean" "-fdx" "recipes")
    (magit-git "pull" "--ff-only" "origin")
    (message "Fetching Melpa recipes...done"))
  (message "Importing Melpa recipes...")
  (let ((default-directory (expand-file-name "import/melpa/" epkg-repository))
        (recipes (make-hash-table :test #'equal :size 6000)))
    (dolist (file (directory-files
                   (expand-file-name "recipes/" default-directory)
                   t "^[^.]"))
      (-let [(epkg-name . recipe)
             (emir-melpa--recipe file)]
        (message "Importing %s..." epkg-name)
        (push recipe (gethash epkg-name recipes))
        (message "Importing %s...done" epkg-name)))
    (message "Importing Melpa recipes...")
    (emir--insert-recipes 'melpa-recipes recipes)
    (message "Importing Melpa recipes...done")))

(defun emir-melpa--recipe (file)
  (-let* (((name . plist)
           (with-temp-buffer
             (insert-file-contents file)
             (read (current-buffer))))
          (name (symbol-name name))
          (repo (plist-get plist :repo)))
    (pcase (plist-get plist :fetcher)
      ('github
       (plist-put plist :url      (format "git@github.com:%s.git" repo))
       (plist-put plist :repopage (format "https://github.com/%s" repo)))
      ('gitlab
       (plist-put plist :url      (format "git@gitlab.com:%s.git" repo))
       (plist-put plist :repopage (format "https://gitlab.com/%s" repo)))
      ('bitbucket
       (plist-put plist :url      (format "hg::ssh://hg@bitbucket.org/%s" repo))
       (plist-put plist :repopage (format "https://bitbucket.org/%s" repo)))
      ('wiki
       (plist-put plist :repopage
                  (format "https://www.emacswiki.org/emacs/download/%s.el"
                          name))))
    (plist-put plist :name name)
    (plist-put plist :closql-id emir--dummy-package)
    (cond ((epkg name)
           (plist-put plist :closql-id name))
          ((assoc name emir-pending-packages)
           (plist-put plist :status 'pending))
          (t (--if-let (or (emir--lookup-url (plist-get plist :url))
                           (cadr (assoc name emir-secondary-packages)))
                 (progn (plist-put plist :closql-id it)
                        (plist-put plist :status 'partial))
               (plist-put plist :status 'new))))
    (mapcar (lambda (row)
              (plist-get plist (intern (format ":%s" row))))
            (cl-coerce (closql--slot-get 'epkg-package 'melpa-recipes :columns)
                       'list))))

;;;; Gelpa

;;;###autoload
(defun emir-import-gelpa-recipes ()
  (interactive)
  (message "Fetching Gelpa recipes...")
  (emir-pull 'epkg-elpa-package)
  (message "Fetching Felpa recipes...done")
  (message "Importing Gelpa recipes...")
  (let ((default-directory (epkg-repository 'epkg-elpa-package))
	(recipes (make-hash-table :test #'equal :size 300)))
    (push (list "org" :core nil nil t "git://orgmode.org/org-mode.git")
          (gethash "org" recipes))
    (pcase-dolist (`(,name ,type ,url) (emir-gelpa--externals))
      (message "Importing %s..." name)
      (push (emir-gelpa--recipe name type url)
            (gethash (and (epkg name) name) recipes))
      (message "Importing %s...done" name))
    (dolist (dir (directory-files "packages/" t "^[^.]"))
      (when (file-directory-p dir)
	(let* ((name (file-name-nondirectory dir))
               (epkg (epkg name)))
	  (unless (gethash name recipes)
            (message "Importing %s..." name)
            (push (emir-gelpa--recipe name)
                  (gethash (and epkg name) recipes))
                    (message "Importing %s...done" name)))))
    (message "Importing Gelpa recipes...")
    (emir--insert-recipes 'gelpa-recipes recipes)
    (message "Importing Gelpa recipes...done")))

(defun emir-gelpa--recipe (name &optional type url)
  (list name
        (cond (type)
              ((cadr (assoc name (emir-gelpa--externals))))
              ((member name (emir--list-packages 'epkg-elpa-branch-package))
               :external!)
              ((member name (emir--list-packages 'epkg-elpa-package))
               :subtree!))
        (cond ((epkg name) nil)
              ((assoc name emir-pending-packages) 'pending)
              (t 'new))
        (cond ((member name (emir--list-packages 'epkg-elpa-branch-package))
               nil)
              ((with-epkg-repository 'epkg-elpa-package
                 (--any-p (string-match-p "^[^ ]+ Merge commit" it)
                          (magit-git-lines "log" "--oneline" "--"
                                           (concat "packages/" name))))
               :squash)
              ((member name '("loccur" "undo-tree"))
               :squash!)
              (t
               :merge))
        (let ((file (expand-file-name (format "packages/%s/%s.el" name name)
                                      (epkg-repository 'epkg-elpa-package))))
          (if (not (file-exists-p file))
              t ; assume all externals are released
            (not (equal (with-temp-buffer
                          (insert-file-contents file)
                          (goto-char (point-min))
                          (or (lm-header "package-version")
                              (lm-header "version")))
                        "0"))))
        (or url
            (--when-let (epkg name)
              (oref it url)))))

(defun emir-gelpa--externals ()
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "externals-list"
                       (epkg-repository 'epkg-elpa-package)))
    (read (current-buffer))))

;;;; Utilities

(defun emir--insert-recipes (slot recipes)
  (let ((dummy-epkg (epkg-orphaned-package :name emir--dummy-package))
        (db (epkg-db)))
    (emacsql-with-transaction db
      (closql-insert db dummy-epkg)
      (epkg-sql [:update melpa-recipes
                 :set    (= closql-id $s1)
                 :where  (isnull closql-id)]
                emir--dummy-package nil)
      (maphash (lambda (key val)
                 (eieio-oset (epkg key) slot val))
               recipes)
      (epkg-sql [:update melpa-recipes
                 :set    (= closql-id $s1)
                 :where  (= closql-id $s2)]
                nil emir--dummy-package)
      (closql-delete db dummy-epkg))))

(defun emir--lookup-url (url)
  (caar (epkg-sql [:select [name] :from packages :where (= url $s1)] url)))

(cl-defmethod emir-import ((class (subclass epkg-wiki-package)))
  (message "Importing wiki packages...")
  (with-epkg-repository class
    (magit-process-buffer)
    (magit-run-git-async "filter-emacswiki" "--tag" "--notes"))
  (message "Importing wiki packages...done"))

(cl-defmethod emir-import ((pkg epkg-wiki-package))
  (with-epkg-repository 'epkg-wiki-package
    (with-slots (name) pkg
      (magit-git "filter-emacswiki" "--tag" "--notes" name))))

(cl-defmethod emir-import ((class (subclass epkg-elpa-package)))
  (--each (emir--list-packages class)
    (message "Importing %s..." it)
    (emir-import (epkg-elpa-package :name it))
    (message "Importing %s...done" it)))

(cl-defmethod emir-import ((pkg epkg-elpa-package))
  (with-epkg-repository 'epkg-elpa-package
    (with-slots (name) pkg
      (magit-git "branch" "-f" (concat "directory/" name) "master")
      (magit-git "filter-elpa" name))))

(cl-defmethod emir-import ((_pkg epkg-elpa-branch-package))) ; noop

(cl-defmethod emir--list-packages ((class (subclass epkg-wiki-package)))
  (with-epkg-repository class
    (delete "master" (magit-list-local-branch-names))))

(cl-defmethod emir--list-packages ((class (subclass epkg-elpa-package)))
  (-mapcat (lambda (line)
             (setq line (cdr (split-string line)))
             (and (equal (nth 0 line) "tree")
                  (list  (nth 2 line))))
           (with-epkg-repository class
             (magit-git-lines "ls-tree" "master:packages"))))

(cl-defmethod emir--list-packages ((class (subclass epkg-elpa-branch-package)))
  (--map (substring it 10)
         (with-epkg-repository class
           (magit-list-refnames "refs/heads/externals"))))

;;; Patch

;;;###autoload
(defun emir-join-provided (package feature)
  (interactive
   (let  ((package (epkg-read-package "Package: ")))
     (list package (intern (read-string "Join provide: ")))))
  (let* ((pkg (epkg package))
         (val (oref pkg provided))
         (elt (assq feature val)))
    (if elt
        (user-error "%s is already provided" feature)
      (oset pkg provided
            (cons (list feature nil t) val)))))

;;;###autoload
(defun emir-drop-provided (package feature)
  (interactive
   (let  ((package (epkg-read-package "Package: ")))
     (list package (intern (completing-read "Drop provide: "
                                            (oref (epkg package) provided)
                                            nil t)))))
  (let* ((pkg (epkg package))
         (val (oref pkg provided))
         (elt (assq feature val)))
    (setf (nth 1 elt) t)
    (oset pkg provided val)))

;;;###autoload
(defun emir-drop-required (package feature)
  (interactive
   (let  ((package (epkg-read-package "Package: ")))
     (list package (intern (completing-read "Drop require: "
                                            (oref (epkg package) required)
                                            nil t)))))
  (let* ((pkg (epkg package))
         (val (oref pkg required))
         (elt (assq feature val)))
    (setf (nth 3 elt) t)
    (oset pkg required val)))

;;;###autoload
(defun emir-soften-required (package feature)
  (interactive
   (let  ((package (epkg-read-package "Package: ")))
     (list package (intern (completing-read "Soften require: "
                                            (oref (epkg package) required)
                                            nil t)))))
  (let* ((pkg (epkg package))
         (val (oref pkg required))
         (elt (assq feature val)))
    (setf (nth 1 elt) nil)
    (setf (nth 2 elt) t)
    (oset pkg required val)))

;;; Find

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
  (interactive (list (completing-read "Find org file: "
                                      '("emir" "config" "compare" "issues"))))
  (find-file-other-frame
   (if (equal name "emir")
       "~/.emacs.d/lib/emir/emir.org"
     (format "~/Repos/pages/emacsmirror.net/stats/%s.org" name))))

;;; Utilities

;;;###autoload
(defun emir-describe-package (package)
  "Display the full documentation of PACKAGE.
Show all slots instead of honoring `epkg-describe-package-slots'."
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


(declare-function org-publish 'ox-publish)
(defvar emir-report-src "~/Repos/pages/emacsmirror.net/stats/")
(defvar emir-report-dst "~/Repos/pages/emacsmirror.net/_site/stats/")

(defun emir-generate-reports ()
  (interactive)
  (let ((org-confirm-babel-evaluate nil))
    (org-publish
     `("emir"
       :base-extension       "org"
       :base-directory       ,emir-report-src
       :publishing-directory ,emir-report-dst
       :publishing-function  org-html-publish-to-html)
     t)))

(defvar emir-url-history nil)

(defun emir-read-url (prompt)
  (magit-read-string prompt nil 'emir-url-history nil nil t))

(defun emir--stage (pkg)
  (unless (epkg-builtin-package-p pkg)
    (with-epkg-repository t
      (magit-call-git "add" (epkg-repository pkg)))))

(defun emir--commit (message &rest include)
  (with-epkg-repository t
    (let ((count (length (magit-staged-files nil "mirror"))))
      (when (> count 0)
        (emir--sort-submodule-sections)
        (magit-git "commit"
                   "-m" (format-spec
                         message
                         `((?n . ,(number-to-string count))
                           (?p . ,(if (> count 1) "packages" "package"))))
                   "-i" ".gitmodules" "epkg.sqlite" include)))))

(defun emir--sort-submodule-sections ()
  (with-epkg-repository t
    (borg--sort-submodule-sections ".git/config")
    (borg--sort-submodule-sections ".gitmodules")
    (magit-call-git "add" ".gitmodules")))

(defun emir--normalize-wikipage (string)
  (->> string
    (replace-regexp-in-string "\\+" "plus")
    (replace-regexp-in-string "-" "")
    downcase))

(defmacro emir--with-org-header (header &rest body)
  (declare (indent defun))
  `(--when-let (progn ,@body)
     (append '((,@header) hline) it)))

(defun emir--melpa-get (name select)
  (let ((val (car (epkg-sql [:select $i1 :from melpa-recipes
                             :where (= name $s2)]
                            select name))))
    (if (vectorp select) val (car val))))

;;; emir.el ends soon
(provide 'emir)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; emir.el ends here
