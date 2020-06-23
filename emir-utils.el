;;; emir-utils.el --- non-essential utilities     -*- lexical-binding: t -*-

;; Copyright (C) 2016-2020  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: https://github.com/emacscollective/emir
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

;;; Code:

(require 'emir)

;;;###autoload
(defun emir-report ()
  (interactive)
  (find-file-other-frame
   (expand-file-name "compare.org" emir-stats-repository)))

(defun emir-remove-obsolete-wiki-branches ()
  (interactive)
  (with-epkg-repository 'epkg-wiki-package
    (dolist (branch (magit-list-local-branch-names))
      (let ((pkg (epkg branch)))
        (when (and (not (equal branch "master"))
                   (or (not pkg)
                       (cl-typep pkg 'epkg-shelved-package)))
          (message "Removing %s..." branch)
          (magit-call-git "branch" "-D" branch)
          (message "Removing %s...done" branch))))))

(defun emir--list-tables (db)
  "Return a list of all tables in DB."
  (emacsql db [:select name :from sqlite_master
               :where (= type table)
               :order-by [(asc name)]]))

(defun emir--table-schemata (db)
  "Return the schemata of all tables in DB."
  (let ((str (emacsql-compile
              db [:select * :from sqlite_master
                  :where (= type table)])))
    (emacsql-clear db)
    (emacsql-send-message db str)
    (emacsql-wait db)
    (with-current-buffer (emacsql-buffer db)
      (goto-char (point-min))
      (let* ((alist '(("\"" . " ")
                      ("'"  . "\"")
                      (","  . " | ")
                      ("DEFAULT"     . ":default")
                      ("PRIMARY KEY" . ":primary-key")
                      ("FOREIGN KEY" . ":foreign-key")
                      ("ON DELETE"   . ":on-delete")
                      ("CASCADE"     . ":cascade")
                      ("REFERENCES"  . ":references")
                      ("NOT NULL"    . ":not-null")
                      ("SET NULL"    . ":set-null")
                      ("NULL"        . ":null")))
             (regex (concat "\\(" (mapconcat #'car alist "\\|") "\\)"))
             (case-fold-search nil))
        (save-excursion
          (while (re-search-forward regex nil t)
            (let ((key (match-string 0)))
              (replace-match (cdr (assoc (match-string 0) alist)) t t)
              (when (and (member key '("PRIMARY KEY" "FOREIGN KEY"))
                         (looking-at " ([^)]+)"))
                (replace-match
                 (save-match-data
                   (format " [%s]"
                           (replace-regexp-in-string
                            "," " " (substring (match-string 0) 2 -1))))
                 nil t))))))
      (mapcar (lambda (tbl)
                (list (cadr tbl)
                      (let* ((seq (car (nthcdr 7 tbl)))
                             (pos (seq-position seq '| #'eq)))
                        (vector (seq-take seq pos)
                                (seq-drop seq pos)))))
              (read (current-buffer))))))

(defun emir--set-db-version (db version)
  (closql--db-set-version db version)
  (emir-commit (format "Bump database version to %s" version) nil :dump))

(defun emir--recreate-db ()
  (require 'epkg-schemata)
  (when epkg--db-connection
    (emacsql-close epkg--db-connection))
  (message "Recreating Epkg database...")
  (let* ((old-file (expand-file-name "epkg.sqlite" epkg-repository))
         (new-file (expand-file-name "new.sqlite" epkg-repository))
         (old (closql-db 'epkg-database nil old-file))
         (new (closql-db 'epkg-database nil new-file)))
    (emacsql new (format "PRAGMA user_version = %s" epkg-db-version))
    (emacsql-with-transaction new
      (dolist (obj (closql-entries old))
        (let ((name (oref obj name)))
          (message "Re-inserting %s..." name)
          (closql--resolve-slots obj)
          (closql--oset obj 'gelpa-recipes eieio-unbound)
          (closql--oset obj 'melpa-recipes eieio-unbound)
          (closql-insert new obj)
          (message "Re-inserting %s...done" name))))
    (emacsql-close old)
    (emacsql-close new)
    (rename-file new-file old-file t)
    (epkg-db)
    (emir-import-gelpa-recipes)
    (emir-import-melpa-recipes))
  (message "Recreating Epkg database...done"))

;;; _
(provide 'emir-utils)
;;; emir-utils.el ends here
