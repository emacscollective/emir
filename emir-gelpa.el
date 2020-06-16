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

;;;###autoload
(defun emir-import-gelpa-recipes ()
  (interactive)
  (message "Fetching Gelpa recipes...")
  (emir-pull 'epkg-elpa-package)
  (message "Fetching Felpa recipes...done")
  (emacsql-with-transaction (epkg-db)
    (message "Importing Gelpa recipes...")
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
          (message "Removing %s...done" name))))
    (message "Importing Gelpa recipes...done")))

(defun emir-import-gelpa-recipe (name spec)
  (pcase-let* ((default-directory emir-gelpa-repository)
               (rcp   (gelpa-get name))
               (`(,url ,type ,method ,released) spec)
               (class (pcase type
                        ('core     'gelpa-core-recipe)
                        ('subtree  'gelpa-subtree-recipe)
                        ('external 'gelpa-external-recipe))))
    (when (and rcp (not (eq (type-of rcp) class)))
      (closql-delete rcp)
      (setq rcp nil))
    (unless rcp
      (setq rcp (funcall class :name name))
      (closql-insert (epkg-db) rcp))
    (oset rcp url url)
    (oset rcp method method)
    (oset rcp released released)
    (oset rcp epkg-package (and (epkg name) name))))

(defun emir-gelpa--package-alist ()
  ;; Be defensive.
  (let ((default-directory emir-gelpa-repository)
        (alist (with-temp-buffer
                 (insert-file-contents
                  (expand-file-name "externals-list" emir-gelpa-repository))
                 (read (current-buffer)))))
    (unless (assoc "org" alist)
      (push (list "org" :core nil) alist))
    (dolist (line (magit-git-lines "ls-tree" "master:packages"))
      (pcase-let ((`(,_ ,object-type ,_ ,name) (split-string line)))
        (when (equal object-type "tree")
          (--if-let (assoc name alist)
              (pcase-let ((`(,_ ,type ,_) it))
                (unless (eq type :subtree)
                  (error "`%s's type is `%s' but `packages/%s' also exists"
                         name type name)))
            (push (list name :subtree nil) alist)))))
    (dolist (line (magit-list-refnames "refs/heads/externals"))
      (let ((name (substring line 10)))
        (--when-let (assoc name alist)
          (pcase-let ((`(,_ ,type ,_) it))
            (unless (eq type :external)
              (error "`%s's type is `%s' but `externals/%s' also exists"
                     name type name))))))
    (mapcar
     (pcase-lambda (`(,name ,type ,url))
       (let ((method nil)
             (released t))
         (cl-ecase type
           (:core)
           (:subtree
            (unless (file-exists-p
                     (expand-file-name (concat "packages/" name)))
              (error "`%s's type is `%s' but `packages/%s' is missing"
                     name type name))
            (setq released (emir-gelpa--subtree-released-p name)))
           (:external
            (unless (magit-branch-p (concat "externals/" name))
              (error "`%s's type is `%s' but `externals/%s' is missing"
                     name type name))))
         (list name url
               (intern (substring (symbol-name type) 1))
               method released)))
     (cl-sort alist #'string< :key #'car))))

(defun emir-gelpa--subtree-released-p (name)
  ;; See section "Public incubation" in "<gelpa>/README".
  (not (equal (with-temp-buffer
                (insert-file-contents
                 (expand-file-name
                  (format "packages/%s/%s.el" name name)))
                (goto-char (point-min))
                (or (lm-header "package-version")
                    (lm-header "version")))
              "0")))

;;; _
(provide 'emir-gelpa)
;;; emir-gelpa.el ends here
