;;; org-make-toc.el --- Automatic tables of contents for Org files  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; URL: http://github.com/alphapapa/org-make-toc
;; Version: 0.1-pre
;; Package-Requires: ((emacs "25.1") (dash "2.12") (s "1.10.0") (org "9.0"))
;; Keywords: Org, convenience

;;; Commentary:

;; This package makes it easy to have a customizable table of contents in Org files that can be
;; updated manually, or automatically when the file is saved.  Links to headings are created
;; compatible with GitHub's Org renderer.

;;;; Installation

;; Install the packages `dash' and `s'.  Then put this file in your `load-path', and put this in
;; your init file:

;; (require 'org-make-toc)

;;;; Usage

;; 1.  Make a heading in the Org file where you want the table of contents, and give it the Org
;; property "TOC" with the value "this".

;; 2.  Run the command `org-make-toc'.

;;;;; Customization

;; The table of contents can be customized by setting the "TOC" property of headings:

;; +  Set to "ignore" to omit a heading from the TOC.
;; +  Set to "ignore-children" or "0" to omit a heading's child headings from the TOC.
;; +  Set to a number N to include child headings no more than N levels deep in the TOC.

;;;;; Automatically update on save

;; To automatically update a file's TOC when the file is saved, use the command
;; `add-file-local-variable' to add `org-make-toc' to the Org file's `before-save-hook'.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'subr-x)

(require 'dash)
(require 's)

;;;; Variables

(defgroup org-make-toc nil
  "Settings for `org-make-toc'."
  :group 'org
  :link '(url-link "http://github.com/alphapapa/org-make-toc"))

;;;; Commands

;;;###autoload
(defun org-make-toc ()
  "Make or update table of contents in current buffer."
  (interactive)
  (when-let ((toc-position (or (org-find-property "TOC" "this")
                               (let ((message "No TOC node found.  A node must have the \"TOC\" property set to \"this\""))
                                 (if (called-interactively-p 'interactive)
                                     (message message)
                                   (user-error message)))))
             (list (or (->> (cddr (org-element-parse-buffer 'headline))
                            (org-make-toc--remove-ignored-entries)
                            (org-make-toc--remove-higher-level-than-toc)
                            (org-make-toc--tree-to-list))
                       (error "Failed to build table of contents"))))
    (org-make-toc--replace-entry-contents toc-position list)))

;;;; Functions

(defun org-make-toc--filter-tree (tree pred)
  "Return TREE with elements for which PRED returns non-nil."
  (cl-loop with properties
           with children
           for element in tree
           when (eql 'headline (car element))
           do (setq children (caddr element))
           if (funcall pred element)
           do (setq properties (second element))
           else do (setq properties nil)
           collect (list 'headline
                         properties
                         (org-make-toc--filter-tree children pred))))

(defun org-make-toc--first-in-tree (tree test-fn value-fn)
  "Return the value of VALUE-FN for the first heading in TREE that TEST-FN matches."
  ;; In a way this is ugly, but in another way it's not, and it works.
  (cl-loop for element in tree
           for type = (car element)
           do (org-element-property :title element)
           if (eq 'headline type)
           when (funcall test-fn element)
           return (funcall value-fn element)
           else
           for children = (caddr element)
           when children
           for result = (org-make-toc--first-in-tree children test-fn value-fn)
           when result
           return result))

(cl-defun org-make-toc--remove-ignored-entries (tree &key depth)
  "Return TREE without ignored entries, up to DEPTH."
  (cl-loop when (and depth
                     (< depth 0))
           return nil

           for element in tree
           for type = (car element)
           for properties = (second element)
           for children = (cddr element)
           when (eql 'headline type)
           for result = (pcase (org-element-property :TOC element)
                          ;; Ignore this entry and its children
                          ("ignore"
                           nil)
                          ;; Keep this entry but ignore its children
                          ((or "ignore-children" "0")
                           (list type properties))
                          ;; Normal entry: descend into tree
                          ((or (pred not) "")
                           (list type
                                 properties
                                 (org-make-toc--remove-ignored-entries children
                                                                       :depth (when depth
                                                                                (1- depth)))))
                          ;; TOC entry; descend into but leave this entry blank so it won't be in the TOC
                          ("this"
                           (list type
                                 (plist-put properties :title nil)
                                 (org-make-toc--remove-ignored-entries children
                                                                       :depth (when depth
                                                                                (1- depth)))))
                          ;; Depth setting
                          ((and number (guard (string-to-number number)))
                           (list type
                                 properties
                                 (org-make-toc--remove-ignored-entries children
                                                                       :depth (or (when depth
                                                                                    (1- depth))
                                                                                  (1- (string-to-number number))))))
                          ;; Invalid setting
                          (other
                           (goto-char (org-element-property :begin element))
                           (user-error "Invalid value for TOC property at entry \"%s\": %s"
                                       (org-element-property :title element) other)))
           when result
           collect result))

;;;;; Filters

(defun org-make-toc--remove-higher-level-than-toc (tree)
  "Return TREE without headings that have a higher level than the TOC."
  (let ((toc-level (org-make-toc--first-in-tree tree
                                                #'org-make-toc--toc-entry-p
                                                #'org-make-toc--element-level)))
    (org-make-toc--filter-tree tree (lambda (element)
                                      (>= (org-element-property :level element)
                                          toc-level)))))

;;;;; Predicates

(defun org-make-toc--toc-entry-p (element)
  "Return non-nil if ELEMENT is the table of contents."
  (string= "this" (org-element-property :TOC element)))

;;;;; Transformer

(defun org-make-toc--tree-to-list (tree)
  "Return TREE converted to a table of contents as a plain list."
  (let* ((contents (s-join "\n"
                           (cl-loop for element in tree
                                    for level = (or (org-element-property :level element) 0)
                                    for indent = (s-repeat (* 2 level) " ")
                                    for children = (org-make-toc--tree-to-list (caddr element))
                                    for link = (org-make-toc--link-entry-github element)
                                    collect (concat indent "-" "  " link "\n" children)))))
    (with-temp-buffer
      (insert contents)
      (goto-char (point-min))
      ;; Remove blank lines and blank list items (ignored items with
      ;; non-ignored children are left as parent nodes, so we must
      ;; delete them; this is probably the easiest way)
      (flush-lines (rx bol (optional
                            (optional (1+ space))
                            "-"
                            (optional (1+ space)))
                       eol))
      ;; Insert blank line after list
      (goto-char (point-max))
      (insert "\n")
      (buffer-string))))

(defun org-make-toc--link-entry-github (entry)
  "Return text for ENTRY converted to GitHub style link."
  (-when-let* ((title (org-element-property :title entry))
               (title (org-link-display-format title))
               (target (replace-regexp-in-string " " "-" (downcase title)))
               (target (replace-regexp-in-string "[^[:alnum:]_-]" "" target)))
    (format "[[#%s][%s]]" target title)))

;;;;; Misc

(defun org-make-toc--toc-level (tree)
  "Return the outline level of the table of contents in TREE."
  (org-make-toc--first-in-tree tree
                               #'org-make-toc--toc-entry-p
                               #'org-make-toc--element-level))

(defun org-make-toc--element-level (element)
  "Return the outline level of Org element ELEMENT."
  (org-element-property :level element))

(defun org-make-toc--replace-entry-contents (pos contents)
  "Replace the contents of entry at POS with CONTENTS."
  (save-excursion
    (goto-char pos)
    (forward-line 1)
    (let ((end (org-entry-end-position)))
      (org-end-of-meta-data)
      (beginning-of-line)
      (setf (buffer-substring (point) end) contents))))

(provide 'org-make-toc)

;;; org-make-toc.el ends here
