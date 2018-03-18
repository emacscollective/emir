;;; emir-melpa.el --- import Melpa recipes        -*- lexical-binding: t -*-

;; Copyright (C) 2016-2018  Jonas Bernoulli

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
(require 'url)

(defvar url-http-end-of-headers)

;;;###autoload
(defun emir-import-melpa-recipes (&optional fetch)
  (interactive (list (not current-prefix-arg)))
  (when fetch
    (let ((default-directory emir-melpa-repository))
      (message "Fetching Melpa recipes...")
      (magit-git "checkout" "master")
      (magit-git "clean" "-fdx" "recipes")
      (magit-git "pull" "--ff-only" "origin")
      (message "Fetching Melpa recipes...done")))
  (emacsql-with-transaction (epkg-db)
    (message "Importing Melpa recipes...")
    (dolist (name (directory-files (emir-melpa--recipe-file nil) nil "^[^.]"))
      (message "Updating %s recipe..." name)
      (emir-import-melpa-recipe name)
      (message "Updating %s recipe...done" name))
    (message "Importing Melpa recipes...")
    (dolist (name (melpa-recipes 'name))
      (unless (file-exists-p (emir-melpa--recipe-file name))
        (message "Removing %s..." name)
        (closql-delete (melpa-get name))
        (message "Removing %s...done" name)))
    (message "Importing Melpa recipes...done"))
  (when fetch
    (emir-import-melpa-downloads)))

(defun emir-import-melpa-recipe (name)
  (let* ((rcp   (melpa-get name))
         (plist (emir-melpa--recipe-plist name))
         (class (intern (format "melpa-%s-recipe"
                                (plist-get plist :fetcher)))))
    (when (and rcp (not (eq (type-of rcp) class)))
      (closql-delete rcp)
      (setq rcp nil))
    (unless rcp
      (setq rcp (funcall class :name name))
      (closql-insert (epkg-db) rcp))
    (dolist (slot '(url repo files branch commit module version-regexp old-names))
      (eieio-oset rcp slot (plist-get plist (intern (format ":%s" slot)))))
    (unless (oref rcp url)
      (oset rcp url    (emir--format-url rcp 'url-format)))
    (oset rcp repopage (emir--format-url rcp 'repopage-format))
    (cond ((epkg name)
           (oset rcp epkg-package name))
          ((not (assoc name emir-pending-packages))
           (--when-let (or (emir--lookup-url (oref rcp url))
                           (cadr (assoc name emir-secondary-packages)))
             (oset rcp epkg-package it))))))

(defun emir-melpa--recipe-plist (name)
  (with-temp-buffer
    (insert-file-contents (emir-melpa--recipe-file name))
    (cdr (read (current-buffer)))))

(defun emir-melpa--recipe-file (name)
  (expand-file-name (concat "recipes/" name) emir-melpa-repository))

(cl-defmethod emir--format-url ((rcp melpa-recipe) slot)
  (ignore-errors
    (--when-let (eieio-oref-default rcp slot)
      (format-spec it `((?r . ,(oref rcp repo)))))))

(defun emir-import-melpa-downloads ()
  (message "Importing Melpa downloads...")
  (with-current-buffer
      (url-retrieve-synchronously "https://melpa.org/download_counts.json")
    (goto-char (1+ url-http-end-of-headers))
    (emacsql-with-transaction (epkg-db)
      (pcase-dolist (`(,name . ,count)
                     (cl-sort
                      (json-read-from-string
                       (decode-coding-string
                        (buffer-substring-no-properties (point) (point-max))
                        'utf-8))
                      #'string< :key #'car))
        (-when-let (pkg (epkg (symbol-name name)))
          (oset pkg downloads count)))))
  (message "Importing Melpa downloads..."))

;;; _
(provide 'emir-melpa)
;;; emir-melpa.el ends here
