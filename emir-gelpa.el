;;; emir-gelpa.el --- Import [Non]GNU Elpa recipes  -*- lexical-binding:t -*-

;; Copyright (C) 2016-2023 Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: https://github.com/emacscollective/emir
;; Keywords: local

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

;;; Code:

(require 'emir)

;;;###autoload
(defun emir-import-all-elpa-recipes (args)
  (interactive (list (transient-args 'emir-import-recipes)))
  (emir-import-melpa-recipes args)
  (emir-import-nongnu-elpa-recipes args)
  (emir-import-gnu-elpa-recipes args))

;;;###autoload
(defun emir-import-nongnu-elpa-recipes (args)
  (interactive (list (transient-args 'emir-import-recipes)))
  (let ((default-directory emir-nongnu-elpa-repository ))
    (emir-gelpa--import-recipes 'nongnu "NonGNU Elpa" args)))

;;;###autoload
(defun emir-import-gnu-elpa-recipes (args)
  (interactive (list (transient-args 'emir-import-recipes)))
  (let ((default-directory emir-gnu-elpa-repository))
    (emir-gelpa--import-recipes 'gnu "GNU Elpa" args)))

(defun emir-gelpa--import-recipes (elpa desc args)
  (when (transient-arg-value "--fetch" args)
    (message "Fetching %s recipes..." desc)
    (magit-git "checkout" "mirror")
    (magit-git "pull" "--ff-only" "origin")
    (message "Fetching %s recipes...done" desc))
  (message "Importing %s recipes..." desc)
  (emacsql-with-transaction (epkg-db)
    (let ((alist (emir-gelpa--recipes-alist)))
      (emir-gelpa--validate-recipes elpa alist)
      (pcase-dolist (`(,name . ,spec) alist)
        (message "Updating %s recipe..." name)
        (emir-gelpa--import-recipe elpa name spec)
        (message "Updating %s recipe...done" name))
      (message "Importing %s recipes..." desc)
      (dolist (name (epkg-list-recipes elpa 'name))
        (unless (assoc name alist)
          (message "Removing %s..." name)
          (closql-delete (epkg-get-recipe elpa name))
          (message "Removing %s...done" name)))))
  (magit-git "tag" "-f" "mirror-imported")
  (emir-commit (format "Update %s recipes" desc) nil :dump)
  (message "Importing %s recipes...done" desc))

(defun emir-gelpa--import-recipe (elpa name plist)
  (let* ((rcp (epkg-get-recipe elpa name))
         (kind  (car  plist))
         (value (cadr plist))
         (plist (cddr plist))
         (class
          (pcase-exhaustive (list elpa kind value)
            (`(gnu :core  ,_) 'epkg-gnu-elpa-core-recipe)
            (`(gnu  :url nil) 'epkg-gnu-elpa-internal-recipe)
            (`(gnu  :url  ,_)
             (cond
              ;; We don't support bzr; treat elpa as the upstream.
              ((string-prefix-p "bzr::" value) 'epkg-gnu-elpa-internal-recipe)
              ((string-prefix-p "hg::"  value) 'epkg-gnu-elpa-hg-recipe)
              ((emir--url-to-class value 'epkg-gnu-elpa-external-recipe))
              ;; emir--url-to-class doesn't support unsafe git:// urls
              ;; but [Non]GNU Elpa uses those.  Additionally they mix
              ;; "savannah.[non]gnu.org" and the alias "sv.[non]gnu.org".
              ((string-match-p "git\\.\\(sv\\|savannah\\)\\.gnu\\.org" value)
               'epkg-gnu-elpa-gnu-recipe)
              ((string-match-p "git\\.\\(sv\\|savannah\\)\\.nongnu\\.org" value)
               'epkg-gnu-elpa-nongnu-recipe)
              (t 'epkg-gnu-elpa-git-recipe)))
            (`(nongnu :url nil) 'epkg-nongnu-elpa-internal-recipe)
            (`(nongnu :url  ,_)
             (cond
              ((string-prefix-p "bzr::" value) 'epkg-nongnu-elpa-internal-recipe)
              ((string-prefix-p "hg::"  value) 'epkg-nongnu-elpa-hg-recipe)
              ((emir--url-to-class value 'epkg-nongnu-elpa-external-recipe))
              ((string-match-p "git\\.\\(sv\\|savannah\\)\\.gnu\\.org" value)
               'epkg-nongnu-elpa-gnu-recipe)
              ((string-match-p "git\\.\\(sv\\|savannah\\)\\.nongnu\\.org" value)
               'epkg-nongnu-elpa-nongnu-recipe)
              (t 'epkg-nongnu-elpa-git-recipe))))))
    (when (and rcp (not (eq (type-of rcp) class)))
      (closql-delete rcp)
      (setq rcp nil))
    (unless rcp
      (setq rcp (funcall class :name name))
      (closql-insert (epkg-db) rcp))
    (oset rcp released
          (or (eq class 'epkg-gnu-elpa-core-recipe)
              (emir-gelpa--released-p elpa name)))
    (pcase kind
      (:url  (oset rcp url value))
      (:core (oset rcp core value)))
    (while plist
      (let ((slot (intern (substring (symbol-name (pop plist)) 1))))
        (unless (memq slot epkg--elpa-recipe-slots)
          (error "Unknown %s recipe slot: %s" elpa slot))
        (eieio-oset rcp slot (pop plist))))
    (cond ((epkg name)
           (oset rcp epkg-package name))
          ((not (emir--config name :delayed))
           (when-let ((name (or (emir--lookup-url (oref rcp url))
                                (emir--config name :secondary))))
             (oset rcp epkg-package name))))))

(defun emir-gelpa--recipes-alist (&optional dir)
  (with-temp-buffer
    (insert-file-contents (expand-file-name "elpa-packages" dir))
    (mapcar (pcase-lambda (`(,sym . ,rest))
              (cons (symbol-name sym) rest))
            (read (current-buffer)))))

(defun emir-gelpa--validate-recipes (elpa alist)
  (when (eq elpa 'gnu)
    (dolist (line (magit-list-refnames "refs/heads/externals"))
      (let ((name (substring line 10)))
        (when-let ((spec (alist-get name alist)))
          (when (plist-member spec :core)
            (error "`%s's type is `:core' but branch `externals/%s' also exists"
                   name name))))))
  (pcase-dolist (`(,name . ,spec) alist)
    (let ((branch (format "%s/%s" (if (eq elpa 'gnu) "externals" "elpa") name)))
      (when (and (plist-member spec :url)
                 (not (magit-branch-p (concat "refs/heads/" branch))))
        (error "`%s's type is `:url' but branch `%s' is missing" name branch)))))

(defun emir-gelpa--released-p (elpa name)
  ;; See section "Public incubation" in "<gnu>/README".
  (not (equal (with-temp-buffer
                (magit-git-insert
                 "cat-file" "-p"
                 (format "refs/heads/%s/%s:%s.el"
                         (if (eq elpa 'gnu) "externals" "elpa")
                         name name))
                (goto-char (point-min))
                (or (lm-header "package-version")
                    (lm-header "version")))
              "0")))

;;; _
(provide 'emir-gelpa)
;;; emir-gelpa.el ends here
