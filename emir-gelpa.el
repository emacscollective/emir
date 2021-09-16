;;; emir-gelpa.el --- import GNU Elpa recipes     -*- lexical-binding: t -*-

;; Copyright (C) 2016-2021  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>

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

;;; Code:

(require 'emir)
(require 'f)

;;;###autoload
(defun emir-import-gelpa-recipes (args)
  (interactive (list (transient-args 'emir-import-recipes)))
  (when (transient-arg-value "--fetch" args)
    (message "Fetching Gelpa recipes...")
    (emir-pull 'epkg-gnu-elpa-package)
    (message "Fetching Felpa recipes...done"))
  (message "Importing Gelpa recipes...")
  (emacsql-with-transaction (epkg-db)
    (let ((alist (emir-gelpa--package-alist)))
      (emir-gelpa--recipe-asserts alist)
      (pcase-dolist (`(,name . ,spec) alist)
        (message "Updating %s recipe..." name)
        (emir-import-gelpa-recipe name spec)
        (message "Updating %s recipe...done" name))
      (message "Importing Gelpa recipes...")
      (dolist (name (gelpa-recipes 'name))
        (unless (assoc name alist)
          (message "Removing %s..." name)
          (closql-delete (gelpa-get name))
          (message "Removing %s...done" name)))))
  (emir-commit "Update Gelpa recipes" nil :dump)
  (message "Importing Gelpa recipes...done"))

(defun emir-import-gelpa-recipe (name &optional spec)
  (unless spec
    (setq spec (alist-get name (emir-gelpa--package-alist))))
  (let ((rcp (gelpa-get name))
        (class (cond ((plist-member spec :core) 'gelpa-core-recipe)
                     ((plist-member spec :url)  'gelpa-external-recipe)
                     (t (error "Invalid recipe %S" (cons name spec))))))
    (when (and rcp (not (eq (type-of rcp) class)))
      (closql-delete rcp)
      (setq rcp nil))
    (unless rcp
      (setq rcp (funcall class :name name))
      (closql-insert (epkg-db) rcp))
    (oset rcp url (or (plist-get spec :core)
                      (plist-get spec :url)))
    (when (eq class 'gelpa-external-recipe)
      (oset rcp released (emir-gelpa--released-p name)))
    (oset rcp epkg-package (and (epkg name) name))))

(defun emir-gelpa--package-alist ()
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "elpa-packages" emir-gelpa-repository))
    (read (current-buffer))))

(defun emir-gelpa--recipe-asserts (alist)
  (let ((default-directory emir-gelpa-repository))
    (dolist (line (magit-list-refnames "refs/heads/externals"))
      (let ((name (substring line 10)))
        (when-let ((spec (alist-get name alist)))
          (when (plist-member spec :core)
            (error "`%s's type is `:core' but branch `externals/%s' also exists"
                   name name)))))
    (pcase-dolist (`(,name . ,spec) alist)
      (when (and (plist-member spec :url)
                 (not (magit-branch-p (concat "origin/externals/" name))))
        (error "`%s's type is `:url' but branch `origin/externals/%s' is missing"
               name name)))))

(defun emir-gelpa--released-p (name)
  ;; See section "Public incubation" in "<gelpa>/README".
  (let ((default-directory emir-gelpa-repository))
    (not (equal (with-temp-buffer
                  (magit-git-insert
                   "cat-file" "-p"
                   (format "origin/externals/%s:%s.el" name name))
                  (goto-char (point-min))
                  (or (lm-header "package-version")
                      (lm-header "version")))
                "0"))))

(defun emir-gelpa--core-filter-args (name)
  (let* ((files  (oref (gelpa-get name) url))
         (files  (if (listp files) files (list files))))
    (nconc (mapcan (##list "--path" %) files)
           (list "--path-rename" (concat (f-common-parent files) ":")))))

;;; _
(provide 'emir-gelpa)
;;; emir-gelpa.el ends here
