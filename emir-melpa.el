;;; emir-melpa.el --- Import Melpa recipes  -*- lexical-binding:t -*-

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
(require 'package-build)
(require 'url)

(defvar url-http-end-of-headers)

;;; Mirror Melpa recipes

;;;###autoload
(defun emir-import-melpa-recipes (args)
  (interactive (list (transient-args 'emir-import-recipes)))
  (let* ((default-directory emir-melpa-repository)
         (imported (magit-rev-verify "mirror-imported"))
         (all (or (not imported)
                  (transient-arg-value "--all" args))))
    (when (transient-arg-value "--fetch" args)
      (message "Fetching Melpa recipes...")
      (magit-git "checkout" "master")
      (magit-git "clean" "-fdx" "recipes")
      (magit-git "pull" "--ff-only" "origin" "master")
      (message "Fetching Melpa recipes...done"))
    (message "Importing Melpa recipes...")
    (let* ((recipes (cl-remove-if
                     (##string-prefix-p "." %)
                     (magit-git-items "ls-tree" "-z" "--name-only"
                                      "HEAD:recipes")))
           (imports (cl-remove-if
                     (##string-prefix-p "." %)
                     (if all
                         recipes
                       (magit-git-items "diff-tree" "-z" "--name-only"
                                        "--diff-filter=AM"
                                        (concat imported ":recipes")
                                        "HEAD:recipes")))))
      (if (not imports)
          (message "No recipes modified since %s" imported)
        (emacsql-with-transaction (epkg-db)
          (dolist-with-progress-reporter (name imports)
              "Importing Melpa recipes..."
            (emir-melpa--import-recipe name)))
        (message "Importing Melpa recipes..."))
      (dolist (name (cl-set-difference (epkg-list-recipes 'melpa 'name)
                                       recipes :test #'equal))
        (message "Removing %s recipe..." name)
        (closql-delete (epkg-get-recipe 'melpa name))
        (message "Removing %s recipe...done" name)))
    (magit-git "tag" "-f" "mirror-imported")
    (emir-commit "Update Melpa recipes" nil :dump)
    (message "Importing Melpa recipes...done")))

(defun emir-melpa--import-recipe (name)
  (let* ((rcp   (epkg-get-recipe 'melpa name))
         (plist (emir-melpa--recipe-plist name))
         (class (intern (format "epkg-melpa-%s-recipe"
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
          ((emir--config name :delayed)
           (oset rcp epkg-package nil))
          (t
           (when-let ((name (or (emir--lookup-url (oref rcp url))
                                (emir--config name :secondary))))
             (oset rcp epkg-package name))))))

(defun emir-melpa--recipe-plist (name)
  (with-temp-buffer
    (insert-file-contents (emir-melpa--recipe-file name))
    (cdr (read (current-buffer)))))

(defun emir-melpa--recipe-file (name)
  (expand-file-name (concat "recipes/" name) emir-melpa-repository))

(cl-defmethod emir--format-url ((rcp epkg-melpa-recipe) slot)
  (ignore-errors
    (and-let* ((format (eieio-oref-default rcp slot)))
      (save-match-data
        (format-spec format `((?r . ,(oref rcp repo))))))))

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
  (and-let* ((archived
              (cl-sort
               (mapcan (lambda (name)
                         (let ((pkg (epkg name)))
                           (and (eq (oref pkg upstream-state) 'archived)
                                (epkg-get-recipe 'melpa name)
                                (list (list name
                                            (oref pkg repopage)
                                            (oref pkg upstream-user))))))
                       (epkgs 'name #'epkg-github-package--eieio-childp))
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

(defun emir-melpa-migrate-recipe (name msg &optional redirected)
  (let ((default-directory emir-melpa-repository)
        (file (concat "recipes/" name))
        (pkg (epkg name))
        (rcp (epkg-get-recipe 'melpa name)))
    (when (file-exists-p file)
      (with-temp-file file
        (insert-file-contents file)
        (save-excursion
          (if (re-search-forward (format ":fetcher ?\\([a-z]+\\)") nil t)
              (let ((fetcher (substring (symbol-name (eieio-object-class pkg))
                                        5 -8)))
                (pcase fetcher
                  ("orphaned"
                   (setq fetcher "github"))
                  ((or "gnu" "nongnu" "subtree")
                   (setq fetcher "git"))
                  ("bitbucket"
                   (setq fetcher "hg")))
                (unless (fboundp (intern (format "epkg-melpa-%s-recipe" fetcher)))
                  (error "%s isn't a valid Melpa fetcher" fetcher))
                (replace-match fetcher t t nil 1))
            (message "WARNING: Cannot find `:fetcher'")))
        (if (re-search-forward (format ":\\(repo\\|url\\) ?\"[^\"]+\"") nil t)
            (replace-match
             (if (cl-typep rcp 'epkg--platform-recipe)
                 (format ":repo \"%s\"" (emir--format-url pkg "%u/%n"))
               (format ":url \"%s\"" (oref pkg url)))
             t t)
          (message "WARNING: Cannot find `:repo' or `:url'")))
      (if (magit-anything-unstaged-p nil file)
          (magit-git "commit" "--gpg-sign" "-m"
                     (concat (format msg name)
                             (and redirected
                                  "\n\nThe old url redirects to the new url."))
                     "--" file)
        (message "WARNING: Recipe is unmodified")))))

;;; Queries

(defun emir-melpa--migrated-packages ()
  (seq-filter
   (pcase-lambda (`(,_name ,_type ,url ,fetcher ,murl))
     (not (or (and (eq fetcher 'hg)
                   (string-prefix-p "hg::" url)
                   (equal murl (substring url 4)))
              (string-prefix-p "https://github.com/emacsmirror/" murl)
              (string-prefix-p "https://github.com/emacsattic/"  murl)
              (equal
               (if (string-suffix-p ".git"  url) (substring  url 0 -4)  url)
               (if (string-suffix-p ".git" murl) (substring murl 0 -4) murl)))))
   (epkg-sql [:select :distinct [packages:name packages:class packages:url
                                 melpa-recipes:class melpa-recipes:url]
              :from [packages melpa-recipes]
              :where (and (= melpa-recipes:epkg-package packages:name)
                          (= melpa-recipes:epkg-package melpa-recipes:name)
                          (not (= melpa-recipes:url packages:url))
                          (in packages:class $v1))]
             (vconcat (closql-where-class-in
                       [file hg git
                        github gitlab codeberg sourcehut gnu nongnu
                        shelved]
                       (epkg-db))))))

(defun emir-melpa--diverging-branches ()
  (epkg-sql [:select :distinct [packages:name
                                packages:branch
                                packages:upstream-branch
                                melpa-recipes:branch
                                packages:class
                                melpa-recipes:class]
             :from [packages melpa-recipes]
             :where (and (= packages:name melpa-recipes:name)
                         (not (= packages:class 'wiki))
                         (or (and (isnull  packages:upstream-branch)
                                  (notnull melpa-recipes:branch))
                             (and (notnull packages:upstream-branch)
                                  (isnull  melpa-recipes:branch))
                             (!= packages:branch melpa-recipes:branch)))
             :order-by [(asc packages:name)]]))

;;; _
(provide 'emir-melpa)
;;; emir-melpa.el ends here
