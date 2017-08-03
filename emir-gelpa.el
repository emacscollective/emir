;;; emir-gelpa.el --- import GNU Elpa recipes     -*- lexical-binding: t -*-

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
(defun emir-import-gelpa-recipes ()
  (interactive)
  (message "Fetching Gelpa recipes...")
  (emir-pull 'epkg-elpa-package)
  (message "Fetching Felpa recipes...done")
  (message "Importing Gelpa recipes...")
  (let ((default-directory emir-gelpa-repository)
	(recipes (make-hash-table :test #'equal :size 300)))
    (push (list "org" :core nil nil t "git://orgmode.org/org-mode.git")
          (gethash "org" recipes))
    (pcase-dolist (`(,name ,type ,url) (emir-gelpa--externals))
      (message "Importing %s..." name)
      (push (emir-gelpa--recipe name type url)
            (gethash (if (epkg name) name emir--dummy-package) recipes))
      (message "Importing %s...done" name))
    (dolist (dir (directory-files "packages/" t "^[^.]"))
      (when (file-directory-p dir)
	(let ((name (file-name-nondirectory dir)))
	  (unless (gethash name recipes)
            (message "Importing %s..." name)
            (push (emir-gelpa--recipe name)
                  (gethash (if (epkg name) name emir--dummy-package) recipes))
            (message "Importing %s...done" name)))))
    (message "Importing Gelpa recipes...")
    (emir--insert-recipes 'gelpa-recipes recipes)
    (message "Importing Gelpa recipes...done")))

(defun emir-gelpa--recipe (name &optional type url)
  (list name
        (cond (type)
              ((cadr (assoc name (emir-gelpa--externals))))
              ((member name (emir--list-packages 'epkg-elpa-branch-package))
               :external!)
              ((member name (emir--list-packages 'epkg-elpa-package))
               :subtree!))
        (cond ((epkg name) nil)
              ((assoc name emir-pending-packages) 'pending)
              (t 'new))
        (cond ((member name (emir--list-packages 'epkg-elpa-branch-package))
               nil)
              ((with-epkg-repository 'epkg-elpa-package
                 (--any-p (string-match-p "^[^ ]+ Merge commit" it)
                          (magit-git-lines "log" "--oneline" "--"
                                           (concat "packages/" name))))
               :squash)
              ((member name '("loccur" "undo-tree"))
               :squash!)
              (t
               :merge))
        (let ((file (expand-file-name (format "packages/%s/%s.el" name name)
                                      emir-gelpa-repository)))
          (if (not (file-exists-p file))
              t ; assume all externals are released
            (not (equal (with-temp-buffer
                          (insert-file-contents file)
                          (goto-char (point-min))
                          (or (lm-header "package-version")
                              (lm-header "version")))
                        "0"))))
        (or url
            (--when-let (epkg name)
              (oref it url)))))

(defun emir-gelpa--externals ()
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "externals-list" emir-gelpa-repository))
    (read (current-buffer))))

(cl-defmethod emir--list-packages ((class (subclass epkg-elpa-package)))
  (-mapcat (lambda (line)
             (setq line (cdr (split-string line)))
             (and (equal (nth 0 line) "tree")
                  (list  (nth 2 line))))
           (with-epkg-repository class
             (magit-git-lines "ls-tree" "master:packages"))))

(cl-defmethod emir--list-packages ((class (subclass epkg-elpa-branch-package)))
  (--map (substring it 10)
         (with-epkg-repository class
           (magit-list-refnames "refs/heads/externals"))))

(provide 'emir-gelpa)
;;; emir-gelpa.el ends here
