;;; emir-melpa.el --- import Melpa recipes        -*- lexical-binding: t -*-

;; Copyright (C) 2016-2017  Jonas Bernoulli

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
(defun emir-import-melpa-recipes (&optional fetch)
  (interactive (list (not current-prefix-arg)))
  (let ((default-directory emir-melpa-repository)
        (recipes (make-hash-table :test #'equal :size 6000)))
    (when fetch
      (message "Fetching Melpa recipes...")
      (magit-git "checkout" "master")
      (magit-git "clean" "-fdx" "recipes")
      (magit-git "pull" "--ff-only" "origin")
      (message "Fetching Melpa recipes...done"))
    (message "Importing Melpa recipes...")
    (dolist (file (directory-files
                   (expand-file-name "recipes/" default-directory)
                   t "^[^.]"))
      (-let [(epkg-name . recipe)
             (emir-melpa--recipe file)]
        (message "Importing %s..." epkg-name)
        (push recipe (gethash epkg-name recipes))
        (message "Importing %s...done" epkg-name)))
    (message "Importing Melpa recipes...")
    (emacsql-with-transaction (epkg-db)
      (emir--insert-recipes 'melpa-recipes recipes)
      ;; FIXME this should not have to be done explicitly
      (dolist (elt (epkg-sql [:select name :from melpa-recipes]))
        (unless (file-exists-p (expand-file-name (concat "recipes/" (car elt))))
          (epkg-sql [:delete-from melpa-recipes :where (= name $s1)]
                    (car elt)))))
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
            (cl-coerce (closql--slot-get 'epkg-package 'melpa-recipes
                                         :closql-columns)
                       'list))))

(defun emir--melpa-get (name select)
  (let ((val (car (epkg-sql [:select $i1 :from melpa-recipes
                             :where (= name $s2)]
                            select name))))
    (if (vectorp select) val (car val))))

(provide 'emir-melpa)
;;; emir-melpa.el ends here
