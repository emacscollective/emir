;;; emir-report.el --- update Emacsmirror reports -*- lexical-binding: t -*-

;; Copyright (C) 2016  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: https://gitlab.com/tarsius/emir
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

;;; Commentary:

;; The functions defined here are used to generate the statistics
;; that are published to https://emacsmirror.net/stats and tracked
;; at https://gitlab.com/tarsius/emacsmirror.net/tree/master/stats.

;;; Code:

(require 'dash)
(require 'emir)
(require 'org)

(declare-function org-publish 'ox-publish)

;;; Variables

(defvar emir-report-src "~/Repos/pages/emacsmirror.net/stats/")
(defvar emir-report-dst "~/Repos/pages/emacsmirror.net/_site/stats/")

;;; Commands

(defun emir-generate-reports ()
  (interactive)
  (let ((org-confirm-babel-evaluate nil))
    (org-publish
     `("emir"
       :base-extension       "org"
       :base-directory       ,emir-report-src
       :publishing-directory ,emir-report-dst
       :publishing-function  org-html-publish-to-html)
     t)))

;;; Utilities

(defun emir-melpa-packages ()
  (mapcar #'car (epkg-sql [:select name :from melpa-recipes])))

(defun emir-melpa-get (name select)
  (let ((val (car (epkg-sql [:select $i1 :from melpa-recipes
                             :where (= name $s2)]
                            select name))))
    (if (vectorp select) val (car val))))

(defun emir-org-melpa-fetcher (name)
  (or (emir-melpa-get name 'fetcher) ""))

(cl-defmethod emir-org-link ((pkg epkg-package))
  (if (oref pkg repopage-format)
      (format "[[%s][%s/%s]]"
              (oref pkg repopage)
              (oref pkg upstream-user)
              (oref pkg upstream-name))
    (--if-let (or (oref pkg repopage)
                  (oref pkg homepage))
        (format "[[%s]]" it)
      "")))

(cl-defmethod emir-org-link ((name string))
  (-let [(fetcher repo repopage url)
         (car (epkg-sql [:select [fetcher repo repopage url]
                         :from melpa-recipes
                         :where (= name $s1)]
                        name))]
    (if (memq fetcher '(github gitlab bitbucket))
        (format "[[%s][%s]]" repopage repo)
      (--if-let (or repopage
                    (and url
                         (string-match-p "\\`https?://" url)
                         (replace-regexp-in-string "\\.git\\'" "" url)))
          (format "[[%s]]" it)
        ""))))

(defmacro emir-with-org-header (header &rest body)
  (declare (indent defun))
  `(--when-let (progn ,@body)
     (append '((,@header) hline) it)))

;;; Compare
;;;; Summary

(defun emir-archives-summary ()
  (require 'finder-inf)
  (let* ((included-builtin (length (epkgs nil 'epkg-builtin-package-p)))
         (ret `((builtin     ,included-builtin
                             0
                             ,(length package--builtins)
                             ,(length (epkg-sql [:select * :from gelpa-packages
                                                         :where (= type ':core)]))
                             0)
                (elpa        0 0 0
                             ,(length (epkg-sql [:select * :from gelpa-packages
                                                         :where type :in $v1
                                                         :and (isnull unreleased)]
                                                [:dir :subtree]))
                             0)
                (elpa-branch 0 0 0
                             ,(length (epkg-sql [:select * :from gelpa-packages
                                                         :where (= type ':external)]))
                             0)
                (shelved     0
                             ,(length (epkgs 'class 'epkg-shelved-package-p))
                             0 0 0))))
    (dolist (class (epkgs 'class 'epkg-mirrored-package--eieio-childp))
      (--if-let (assq class ret)
          (cl-incf (nth 1 it))
        (push (list class 1 0 0 0 0) ret)))
    (dolist (fetcher (epkg-sql [:select fetcher :from melpa-recipes]))
      (setq  fetcher (car fetcher))
      (--if-let (assq fetcher ret)
          (cl-incf (nth 5 it))
        (push (list fetcher 0 0 0 0 1) ret)))
    (emir-with-org-header ("Type" "Mirror" "Attic" "Emacs" "Gelpa" "Melpa")
      (append (cl-sort (copy-sequence ret) #'> :key #'cadr)
              (list 'hline
                    (list "- builtin" (- included-builtin) "" "" "" "")
                    (append
                     (--reduce (list "= total"
                                     (+ (nth 1 acc) (nth 1 it))
                                     (+ (nth 2 acc) (nth 2 it))
                                     (+ (nth 3 acc) (nth 3 it))
                                     (+ (nth 4 acc) (nth 4 it))
                                     (+ (nth 5 acc) (nth 5 it)))
                               (cons (list nil (- included-builtin) 0 0 0 0)
                                     ret))))))))

(defun emir-archives-compare (symbol type nocache get-diff get-type)
  (declare (indent 3))
  (cond ((not type)
         (or (and (not nocache)
                  (symbol-value symbol))
             (set symbol
                  (let (alist)
                    (dolist (package (funcall get-diff))
                      (-when-let (type (funcall get-type package))
                        (-if-let (elt (assq type alist))
                            (push package (cdr elt))
                          (push (list type package) alist))))
                    (cl-sort (mapcar (-lambda ((type . packages))
                                       (list type
                                             (length packages)
                                             (sort packages #'string<)))
                                     alist)
                             #'> :key #'cadr)))))
        ((eq type 'summary)
         (emir-with-org-header ("Reason" "N")
           (--map (butlast it) (funcall symbol nil nocache))))
        (t
         (car (cddr (assq type (funcall symbol nil nocache)))))))

;;;; Emacsmirror vs. Gelpa

(defun  emir-gelpa-shadowed ()
  (emir-with-org-header ("Package" "Type" "Gelpa" "Squash" "Reason" "Link")
    (let ((branches (emir--list-packages 'epkg-elpa-branch-package))
          (externals-list
           (with-temp-buffer
             (insert-file-contents
              (expand-file-name "externals-list"
                                (epkg-repository 'epkg-elpa-package)))
             (read (current-buffer)))))
      (mapcar
       (lambda (name)
         (-let ((pkg (epkg name))
                (dir (expand-file-name
                      (concat "packages/" name)
                      (epkg-repository 'epkg-elpa-package))))
           (list name
                 (and pkg (epkg-type pkg))
                 (cond ((cadr (assoc name externals-list)))
                       ((member name branches) "?external")
                       ((file-exists-p dir)    "?subtree")
                       (t                      ""))
                 (if (file-exists-p dir)
                     (if (with-epkg-repository 'epkg-elpa-branch-package
                           (--any-p (string-match-p "^[^ ]+ Merge commit" it)
                                    (magit-git-lines "log" "--oneline" "--"
                                                     (concat "packages/" name))))
                         "yes"
                       (if (member name '("loccur" "undo-tree"))
                           "YES"
                         "no"))
                   "")
                 (or (car (cddr (assoc name emir-preferred-upstreams))) "")
                 (emir-org-link pkg))))
       (emir-gelpa-only 'upstream)))))

(defvar emir-gelpa-only nil)
(defun  emir-gelpa-only (&optional type nocache)
  (emir-archives-compare 'emir-gelpa-only type nocache
    (lambda ()
      (nconc (emir--list-packages 'epkg-elpa-package)
             (emir--list-packages 'epkg-elpa-branch-package)))
    (lambda (package)
      (let ((pkg (epkg package)))
        (cond ((epkg-builtin-package-p pkg)          'builtin)
              ((assoc package emir-pending-packages) 'pending)
              ((assoc package emir-ignored-packages) 'ignored)
              ((not pkg)                             'new)
              ((not (memq (epkg-type pkg) '(elpa elpa-branch))) 'upstream))))))

;;;; Emacsmirror vs. Melpa

(defvar emir-melpa-only nil)
(defun  emir-melpa-only (&optional type nocache)
  (let ((urls (epkgs 'url)))
    (emir-archives-compare 'emir-melpa-only type nocache
      (lambda ()
        (cl-set-difference (emir-melpa-packages)
                           (epkgs 'name)
                           :test #'equal))
      (lambda (package)
        (-let [(fetcher url)
               (car (epkg-sql [:select [fetcher url]
                               :from melpa-recipes
                               :where (= name $s1)]
                              package))]
          (cond ((assoc package emir-pending-packages)      'pending)
                ((assoc package emir-ignored-packages)      'ignored)
                ((eq fetcher 'wiki)                         'wiki)
                ((memq fetcher '(bzr cvs darcs fossil svn)) 'old-vc)
                ((member url urls)                          'partial)
                (t                                          'new)))))))

(defvar emir-melpa-missing nil)
(defun  emir-melpa-missing (&optional type nocache)
  (let ((elpa (nconc (emir--list-packages 'epkg-elpa-package)
                     (emir--list-packages 'epkg-elpa-package))))
    (emir-archives-compare 'emir-melpa-missing type nocache
      (lambda ()
        (cl-set-difference (epkgs 'name)
                           (emir-melpa-packages)
                           :test #'equal))
      (lambda (package)
        (let ((type (epkg-type (epkg package))))
          (if (and (not (memq type '(builtin elpa elpa-branch)))
                   (member package elpa))
              'elpa/upstream
            type))))))

;;; Issues

(defun emir-feature-conflicts (&optional shelved)
  (let (alist body)
    (dolist (name (if shelved
                      (epkgs 'name)
                    (epkgs 'name '(epkg-mirrored-package--eieio-childp
                                   epkg-builtin-package-p))))
      (dolist (feature (epkg-sql [:select feature :from provided
                                  :where (and (= package $s1)
                                              (isnull drop))]
                                 name))
        (setq feature (car feature))
        (-if-let (elt (assq feature alist))
            (push name (cdr elt))
          (cl-pushnew (list feature name) alist))))
    (setq body
          (-mapcat (-lambda ((feature . providers))
                     (and (> (length providers) 1)
                          (list (list feature providers))
                          (--map (list feature it)
                                 (sort providers #'string<))))
                   (cl-sort alist #'string< :key #'car)))
    (if shelved
        (emir-with-org-header ("Feature" "Package" "Shelved")
          (mapcar (-lambda ((feature package))
                    (list feature package
                          (or (epkg-shelved-package-p (epkg package)) "")))
                  body))
      (emir-with-org-header ("Feature" "Package")
        body))))

(defun emir-unsatisfied-hard-dependencies ()
  (emir-with-org-header ("Package" "Type" "Melpa" "Feature")
    (-mapcat (-lambda ((name class))
               (let (unsatisfied)
                 (-each (epkg-sql [:select feature :from required
                                   :where (and (= package $s1)
                                               (= hard 't)
                                               (isnull drop))
                                   :order-by [(asc feature)]]
                                  name)
                   (-lambda ((feature))
                     (unless (epkg--required name feature)
                       (push feature unsatisfied))))
                 (when unsatisfied
                   (let ((fetcher (emir-org-melpa-fetcher name)))
                     (--map (list name class fetcher it)
                            (sort unsatisfied #'string<))))))
             (epkgs [name class] 'epkg-mirrored-package--eieio-childp))))

(defun emir-unsatisfied-soft-dependencies ()
  (emir-with-org-header ("Package" "Type" "Melpa" "Feature")
    (-mapcat (-lambda ((name class))
               (let (unsatisfied)
                 (-each (epkg-sql [:select feature :from required
                                   :where (and (= package $s1)
                                               (isnull hard)
                                               (isnull drop))
                                   :order-by [(asc feature)]]
                                  name)
                   (-lambda ((feature))
                     (unless (epkg--required name feature)
                       (push feature unsatisfied))))
                 (when unsatisfied
                   (let ((fetcher (emir-org-melpa-fetcher name)))
                     (--map (list name class fetcher it)
                            (sort unsatisfied #'string<))))))
             (epkgs [name class] 'epkg-mirrored-package--eieio-childp))))

(defun emir-hard-required-shelved ()
  (emir-with-org-header ("Package" "Type" "Melpa" "Feature")
    (-mapcat (-lambda ((name class))
               (let (sight)
                 (-each (epkg-sql [:select feature :from required
                                   :where (and (= package $s1)
                                               (= hard 't)
                                               (isnull drop))
                                   :order-by [(asc feature)]]
                                  name)
                   (-lambda ((feature))
                     (--when-let (epkg--required name feature)
                       (and (epkg-shelved-package-p (epkg it))
                            (push feature sight)))))
                 (when sight
                   (let ((fetcher (emir-org-melpa-fetcher name)))
                     (--map (list name class fetcher it)
                            (sort sight #'string<))))))
             (epkgs [name class] 'epkg-mirrored-package--eieio-childp))))

(defun emir-soft-required-shelved ()
  (emir-with-org-header ("Package" "Type" "Melpa" "Feature")
    (-mapcat (-lambda ((name class))
               (let (sight)
                 (-each (epkg-sql [:select feature :from required
                                   :where (and (= package $s1)
                                               (isnull hard)
                                               (isnull drop))
                                   :order-by [(asc feature)]]
                                  name)
                   (-lambda ((feature))
                     (--when-let (epkg--required name feature)
                       (and (epkg-shelved-package-p (epkg it))
                            (push feature sight)))))
                 (when sight
                   (let ((fetcher (emir-org-melpa-fetcher name)))
                     (--map (list name class fetcher it)
                            (sort sight #'string<))))))
             (epkgs [name class] 'epkg-mirrored-package--eieio-childp))))

(defun emir-orphans ()
  (emir-with-org-header ("Package" "Type" "Melpa")
    (--map (list it
                 (epkg-type (epkg it))
                 (if (string-prefix-p "emacsorphanage/"
                                      (emir-melpa-get it 'repo))
                     "(orphaned)"
                   (emir-org-melpa-fetcher it)))
           (sort (--map (cdr (assq 'name it))
                        (ghub-get "/orgs/emacsorphanage/repos"))
                 #'string<))))

;;; emir-report.el ends soon
(provide 'emir-report)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; emir-report.el ends here
