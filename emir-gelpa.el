;;; emir-gelpa.el --- Import [Non]GNU Elpa recipes  -*- lexical-binding:t -*-

;; Copyright (C) 2016-2026 Jonas Bernoulli

;; Author: Jonas Bernoulli <emacs.emir@jonas.bernoulli.dev>
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

;;; Mirror recipes

;;;###autoload
(defun emir-import-all-elpa-recipes (args)
  "Import GNU Elpa, NonGNU Elpa and Melpa package recipes."
  (interactive (list (transient-args 'emir-import-recipes)))
  (emir-import-melpa-recipes args)
  (emir-import-nongnu-elpa-recipes args)
  (emir-import-gnu-elpa-recipes args))

;;;###autoload
(defun emir-import-nongnu-elpa-recipes (args)
  "Import NonGNU Elpa package recipes."
  (interactive (list (transient-args 'emir-import-recipes)))
  (let ((default-directory emir-nongnu-elpa-repository ))
    (emir-gelpa--import-recipes 'nongnu "NonGNU Elpa" args)))

;;;###autoload
(defun emir-import-gnu-elpa-recipes (args)
  "Import GNU Elpa package recipes."
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
        (let ((url (plist-get spec :url)))
          (when (and url (symbolp url))
            (plist-put spec :url
                       (plist-get
                        (alist-get (symbol-name url) alist nil nil #'equal)
                        :url))))
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
      (let ((slot (intern (substring (symbol-name (pop plist)) 1)))
            (value (pop plist)))
        (unless (memq slot epkg--elpa-recipe-slots)
          (error "Unknown %s recipe slot: %s" elpa slot))
        ;; `doc-files' is not relevant for us.
        ;; `maintainer' is used for a single package.
        ;; `manual-sync' is not relevant for us.
        (unless (memq slot '(doc-files maintainer manual-sync))
          (eieio-oset rcp slot value))))
    (cond ((epkg name)
           (oset rcp epkg-package name))
          ((emir--config name :delayed)
           (oset rcp epkg-package nil))
          ((oset rcp epkg-package
                 (or (emir--lookup-url (oref rcp url))
                     (emir--config name :secondary)))))))

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
    (when (plist-member spec :url)
      (let* ((url (plist-get spec :url))
             (branch (format "%s/%s"
                             (if (eq elpa 'gnu) "externals" "elpa")
                             (if (and url (symbolp url))
                                 ;; `url' is a symbol identifying another
                                 ;; package, whose branch is to be used.
                                 url
                               name))))
        (unless (magit-branch-p (concat "refs/heads/" branch))
          (error "`%s's type is `:url' but branch `%s' is missing"
                 name branch))))))

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

;;; Queries

(defun emir-gnu-elpa--migrated-packages (&optional include-builtin)
  (emir--migrated-packages 'gnu-elpa include-builtin))

(defun emir-nongnu-elpa--migrated-packages (&optional include-builtin)
  (emir--migrated-packages 'nongnu-elpa include-builtin))

(defun emir--migrated-packages (elpa &optional include-builtin)
  (seq-filter
   (pcase-lambda (`(,_name ,type ,url ,fetcher ,eurl))
     (not (or (eq type elpa)
              (and (eq fetcher 'hg)
                   (string-prefix-p "hg::" url)
                   (equal eurl (substring url 4)))
              (string-prefix-p "https://github.com/emacsmirror/" eurl)
              (string-prefix-p "https://github.com/emacsattic/"  eurl)
              (equal
               (if (string-suffix-p ".git"  url) (substring  url 0 -4)  url)
               (if (string-suffix-p ".git" eurl) (substring eurl 0 -4) eurl))
              (equal
               (if (string-suffix-p "/"  url) (substring  url 0 -1)  url)
               (if (string-suffix-p "/" eurl) (substring eurl 0 -1) eurl)))))
   (epkg-sql [:select :distinct [packages:name packages:class packages:url
                                 recipes:class recipes:url]
              :from [packages (as $i1 recipes)]
              :where (and (= recipes:epkg-package packages:name)
                          (= recipes:epkg-package recipes:name)
                          (not (= recipes:url packages:url))
                          (not (in packages:class $v2)))]
             (intern (format "%s-recipes" elpa))
             (vconcat (closql-where-class-in
                       (if include-builtin [subtree] [subtree builtin])
                       (epkg-db))))))

;;; _
(provide 'emir-gelpa)
;; Local Variables:
;; read-symbol-shorthands: (
;;   ("and$"      . "cond-let--and$")
;;   ("and-let"   . "cond-let--and-let")
;;   ("if-let"    . "cond-let--if-let")
;;   ("when$"     . "cond-let--when$")
;;   ("when-let"  . "cond-let--when-let")
;;   ("while-let" . "cond-let--while-let"))
;; End:
;;; emir-gelpa.el ends here
