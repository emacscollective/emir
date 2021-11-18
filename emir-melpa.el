;;; emir-melpa.el --- import Melpa recipes        -*- lexical-binding: t -*-

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
(require 'url)

(defvar url-http-end-of-headers)

;;;###autoload
(defun emir-import-melpa-recipes (args)
  (interactive (list (transient-args 'emir-import-recipes)))
  (let* ((default-directory emir-melpa-repository)
         (old-rev (or (magit-get "emir.melpa-imported")
                      (magit-rev-parse "HEAD")))
         (all (transient-arg-value "--all" args)))
    (when (transient-arg-value "--fetch" args)
      (message "Fetching Melpa recipes...")
      (magit-git "checkout" "master")
      (magit-git "clean" "-fdx" "recipes")
      (magit-git "pull" "--ff-only" "origin")
      (message "Fetching Melpa recipes...done"))
    (message "Importing Melpa recipes...")
    (let* ((recipes (cl-remove-if
                     (lambda (n) (string-prefix-p "." n))
                     (magit-git-items "ls-tree" "-z" "--name-only"
                                      "HEAD:recipes")))
           (imports (cl-remove-if
                     (lambda (n) (string-prefix-p "." n))
                     (if all
                         recipes
                       (magit-git-items "diff-tree" "-z" "--name-only"
                                        "--diff-filter=AM"
                                        (concat old-rev ":recipes")
                                        "HEAD:recipes")))))
      (if (not imports)
          (message "No recipes modified since %s" old-rev)
        (emacsql-with-transaction (epkg-db)
          (dolist-with-progress-reporter (name imports)
              "Importing Melpa recipes..."
            (emir-import-melpa-recipe name)))
        (message "Importing Melpa recipes..."))
      (dolist (name (cl-set-difference (melpa-recipes 'name)
                                       recipes :test #'equal))
        (message "Removing %s recipe..." name)
        (closql-delete (melpa-get name))
        (message "Removing %s recipe...done" name)))
    (magit-set (magit-rev-parse "HEAD") "emir.melpa-imported")
    (emir-commit "Update Melpa recipes" nil :dump)
    (message "Importing Melpa recipes...done")))

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
    (dolist (slot '(url repo files branch commit version-regexp old-names))
      (eieio-oset rcp slot (plist-get plist (intern (format ":%s" slot)))))
    (unless (oref rcp url)
      (oset rcp url    (emir--format-url rcp 'url-format)))
    (oset rcp repopage (emir--format-url rcp 'repopage-format))
    (cond ((epkg name)
           (oset rcp epkg-package name))
          ((not (assoc name emir-pending-packages))
           (when-let ((name (or (emir--lookup-url (oref rcp url))
                                (cadr (assoc name emir-secondary-packages)))))
             (oset rcp epkg-package name))))))

(defun emir-melpa--recipe-plist (name)
  (with-temp-buffer
    (insert-file-contents (emir-melpa--recipe-file name))
    (cdr (read (current-buffer)))))

(defun emir-melpa--recipe-file (name)
  (expand-file-name (concat "recipes/" name) emir-melpa-repository))

(cl-defmethod emir--format-url ((rcp melpa-recipe) slot)
  (ignore-errors
    (and-let* ((format (eieio-oref-default rcp slot)))
      (format-spec format `((?r . ,(oref rcp repo)))))))

;;;###autoload
(defun emir-import-melpa-downloads ()
  (interactive)
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
        (when-let ((pkg (epkg (symbol-name name))))
          (oset pkg downloads count)))))
  (emir-commit "Update Melpa download counts" nil :dump)
  (message "Importing Melpa downloads...done"))

(defun emir-melpa-list-archived-packages ()
  (interactive)
  (when-let ((archived
              (cl-sort
               (mapcan (lambda (name)
                         (and (member name emir--archived-packages)
                              (melpa-get name)
                              (let ((pkg (epkg name)))
                                (list (list name
                                            (oref pkg repopage)
                                            (oref pkg upstream-user))))))
                       (epkgs 'name 'epkg-mirrored-package--eieio-childp))
               #'string< :key #'car)))
    (let ((buf (get-buffer-create "*melpa archive*")))
      (pop-to-buffer buf)
      (with-current-buffer buf
        (erase-buffer)
        (pcase-dolist (`(,name ,page ,user) archived)
          (insert
           (format "- [ ] [%s](%s) by @%s%s\n" name page user
                   (if-let ((dependents (epkg-reverse-dependencies name)))
                       (format " (required by %s)"
                               (mapconcat (pcase-lambda (`(,name . ,_features)) name)
                                          dependents " "))
                     ""))))))))

;;; _
(provide 'emir-melpa)
;;; emir-melpa.el ends here
