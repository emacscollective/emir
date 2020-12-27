;;; emir-gelpa.el --- import GNU Elpa recipes     -*- lexical-binding: t -*-

;; Copyright (C) 2016-2020  Jonas Bernoulli

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
(defun emir-import-gelpa-recipes (&optional fetch)
  (interactive (list (not current-prefix-arg)))
  (when fetch
    (message "Fetching Gelpa recipes...")
    (emir-pull 'epkg-gnu-elpa-package)
    (message "Fetching Felpa recipes...done"))
  (message "Importing Gelpa recipes...")
  (emacsql-with-transaction (epkg-db)
    (let ((alist (emir-gelpa--package-alist)))
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

(defun emir-import-gelpa-recipe (name spec)
  (pcase-let* ((default-directory emir-gelpa-repository)
               (rcp (gelpa-get name))
               (`(,url ,type ,released) spec)
               (class (pcase type
                        ('core 'gelpa-core-recipe)
                        ('url  'gelpa-external-recipe))))
    (when (and rcp (not (eq (type-of rcp) class)))
      (closql-delete rcp)
      (setq rcp nil))
    (unless rcp
      (setq rcp (funcall class :name name))
      (closql-insert (epkg-db) rcp))
    (oset rcp url url)
    (oset rcp released released)
    (oset rcp epkg-package (and (epkg name) name))))

(defun emir-gelpa--package-alist ()
  ;; Be defensive.
  (let ((default-directory emir-gelpa-repository)
        (alist (with-temp-buffer
                 (insert-file-contents
                  (expand-file-name "elpa-packages" emir-gelpa-repository))
                 (read (current-buffer)))))
    (dolist (line (magit-list-refnames "refs/heads/externals"))
      (let ((name (substring line 10)))
        (when-let ((elt (assoc name alist)))
          (pcase-let ((`(,_ ,type ,_) elt))
            (unless (eq type :url)
              (error "`%s's type is `%s' but `externals/%s' also exists"
                     name type name))))))
    (mapcar
     (pcase-lambda (`(,name ,type ,url))
       (let (released)
         (when (eq type :url)
           (unless (magit-branch-p (concat "externals/" name))
             (error "`%s's type is `%s' but `externals/%s' is missing"
                    name type name))
           (setq released (emir-gelpa--released-p name)))
         (list name url
               (intern (substring (symbol-name type) 1))
               released)))
     (cl-sort alist #'string< :key #'car))))

(defun emir-gelpa--released-p (name)
  ;; See section "Public incubation" in "<gelpa>/README".
  (not (equal (with-temp-buffer
                (magit-git-insert
                 "cat-file" "-p" (format "externals/%s:%s.el" name name))
                (goto-char (point-min))
                (or (lm-header "package-version")
                    (lm-header "version")))
              "0")))

(defun emir-gelpa--core-filter-args (name)
  (let* ((files  (oref (gelpa-get name) url))
         (files  (if (listp files) files (list files))))
    (nconc (mapcan (##list "--path" %) files)
           (list "--path-rename" (concat (f-common-parent files) ":")))))

;;; _
(provide 'emir-gelpa)
;;; emir-gelpa.el ends here
