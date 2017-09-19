;;; emir-utils.el --- non-essential utilities     -*- lexical-binding: t -*-

;; Copyright (C) 2016-2017  Jonas Bernoulli

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
(defun emir-find-file (filename &optional wildcards)
  (interactive (emir-find-file-read-args "Find file: "))
  (emir-find-file-noselect filename #'switch-to-buffer wildcards))

;;;###autoload
(defun emir-find-file-other-window (filename &optional wildcards)
  (interactive (emir-find-file-read-args "Find file in other window: "))
  (emir-find-file-noselect filename #'switch-to-buffer-other-window wildcards))

;;;###autoload
(defun emir-find-file-other-frame (filename &optional wildcards)
  (interactive (emir-find-file-read-args "Find file in other frame: "))
  (emir-find-file-noselect filename #'switch-to-buffer-other-frame wildcards))

(defun emir-find-file-read-args (prompt)
  (with-epkg-repository (epkg (epkg-read-package "Find file of package: "))
    (find-file-read-args prompt (confirm-nonexistent-file-or-buffer))))

(defun emir-find-file-noselect (filename:s switch &optional wildcards)
  (let ((value (find-file-noselect filename:s nil nil wildcards)))
    (if (listp value)
	(let ((buffers (nreverse value)))
	  (funcall switch (car buffers))
	  (mapc #'switch-to-buffer (cdr buffers))
	  buffers)
      (funcall switch value))))

;;;###autoload
(defun emir-find-org-file (name)
  (interactive
   (list (completing-read "Find org file: "
                          (append '("maint" "config")
                                  (--map (substring it 0 -4)
                                         (directory-files emir-stats-repository
                                                          nil "\\.org\\'"))))))
  (find-file-other-frame
   (cond ((equal name "maint")
          "~/.emacs.d/lib/emir/emir.org")
         ((equal name "config")
          (expand-file-name "emir.org" epkg-repository))
         (t
          (expand-file-name (concat name ".org") emir-stats-repository)))))

;;;###autoload
(defun emir-generate-reports ()
  (interactive)
  (let ((org-confirm-babel-evaluate nil))
    (org-publish
     `("emir"
       :base-extension      "org"
       :base-directory      ,emir-stats-repository
       :publishing-function org-html-publish-to-html)
     t)))

;;;###autoload
(defun emir-describe-package (package)
  (interactive
   (list (epkg-read-package "Describe package: "
                            (or (tabulated-list-get-id)
                                (--when-let (symbol-at-point)
                                  (symbol-name it))))))
  (help-setup-xref (list #'emir-describe-package package)
                   (called-interactively-p 'interactive))
  (with-help-window (help-buffer)
    (with-current-buffer standard-output
      (let ((epkg-describe-package-slots-width 14))
        (epkg-describe-package-1
         (epkg package)
         '(type
           hash
           url
           mirror-url
           mirror-name
           upstream-user
           upstream-name
           upstream-branch
           upstream-tree
           library
           epkg-insert-repopage
           epkg-insert-homepage
           epkg-insert-mirrorpage
           epkg-insert-wikipage
           license
           created
           updated
           summary
           libraries
           provided
           required
           epkg-insert-keywords
           epkg-insert-authors
           epkg-insert-maintainers
           epkg-insert-commentary
           epkg-insert-dependencies
           epkg-insert-reverse-dependencies))))))

(provide 'emir-utils)
;;; emir-utils.el ends here
