;;; org-make-toc.el --- Automatic tables of contents for Org files  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; URL: http://github.com/alphapapa/org-make-toc
;; Version: 0.4-pre
;; Package-Requires: ((emacs "25.1") (dash "2.12") (s "1.10.0") (org "9.0"))
;; Keywords: Org, convenience

;;; Commentary:

;; This package makes it easy to have one or more customizable tables of contents in Org files.
;; They can be updated manually, or automatically when the file is saved.  Links to headings are
;; created compatible with GitHub's Org renderer.

;;;; Installation

;; Install the packages `dash' and `s'.  Then put this file in your `load-path', and put this in
;; your init file:

;;   (require 'org-make-toc)

;;;; Usage

;; 1.  Make a heading in the Org file where you want the table of contents, and give it the Org
;; property "TOC" with the value "this".

;; 2.  Run the command `org-make-toc'.

;;;;; Advanced

;; A document may contain multiple tables of contents.  Tables of contents can be customized by
;; setting the "TOC" property of headings to these values:

;; +  "all": Include all headings in the file, except ignored headings.
;; +  "children": Include only child headings of this ToC.
;; +  "siblings": Include only sibling headings of this ToC.
;; +  "ignore": Omit a heading from the TOC.
;; +  "ignore-children" or "0": Omit a heading's child headings from the TOC.
;; +  a number "N": Include child headings no more than "N" levels deep in the TOC.

;;;;; Automatically update on save

;; To automatically update a file's TOC when the file is saved, use the command
;; "add-file-local-variable" to add "org-make-toc" to the Org file's "before-save-hook".

;; Or, you may activate it in all Org buffers like this:

;;   (add-hook 'org-mode-hook #'org-make-toc-mode)

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

(defcustom org-make-toc-filename-prefix nil
  "Prefix links with filename before anchor tag."
  :type 'boolean
  :safe #'booleanp)

(defcustom org-make-toc-link-type-fn #'org-make-toc--link-entry-github
  "Type of links to make.
`org-element' entries are passed to this function, which returns
an Org link as a string, the target of which should be compatible
with the destination of the published file."
  :type '(choice (const :tag "GitHub-compatible" org-make-toc--link-entry-github)
                 (const :tag "Org-compatible" org-make-toc--link-entry-org)
                 (function :tag "Custom function")))

;;;; Commands

;;;###autoload
(defun org-make-toc ()
  "Make or update table of contents in current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (cl-loop with made-toc
             for position = (org-make-toc--find-next-property "TOC")
             while position
             do (progn
                  (goto-char position)
                  (when (org-make-toc--update-toc-at-point)
                    (setq made-toc t))
                  (or (outline-next-heading)
                      (goto-char (point-max))))
             finally do (unless made-toc
                          (message "org-make-toc: No TOC node found.")))))

(defun org-make-toc-at-point ()
  "Make or update table of contents at current entry."
  (interactive)
  (unless (org-make-toc--update-toc-at-point)
    (user-error "No TOC node found")))

;;;; Functions

(defun org-make-toc--update-toc-at-point ()
  "Make or update table of contents at current entry."
  (when-let* ((toc-string (org-make-toc--toc-at (point))))
    (setq toc-string (org-make-toc--remove-excess-indentation toc-string))
    (org-make-toc--replace-entry-contents (point) toc-string)
    t))

(defun org-make-toc--toc-at (position)
  "Return table of contents as string for entry at POSITION."
  (save-excursion
    (save-restriction
      (let ((type (org-entry-get position "TOC")))
        (when (member type '("this" "all" "siblings" "children"))
          (goto-char position)
          (pcase type
            ;; Widen or narrow as necessary
            ((or "this" "all") (widen))
            ("siblings" (progn
                          (ignore-errors
                            (outline-up-heading 1))
                          (narrow-to-region (save-excursion
                                              (forward-line 1)
                                              (point))
                                            (save-excursion
                                              (org-end-of-subtree)
                                              (point)))))
            ("children" (org-narrow-to-subtree)))
          (or (--> (cddr (org-element-parse-buffer 'headline))
                   (org-make-toc--remove-ignored-entries it :keep-all (string= type "all"))
                   (org-make-toc--tree-to-list it))
              (error "Failed to build table of contents at position: %s" position)))))))

(defun org-make-toc--find-next-property (property &optional value)
  "Return position of next entry in buffer that has PROPERTY, or nil if none is found.
When VALUE is non-nil, find entries for which PROPERTY has VALUE.
Like `org-find-property', but searches forward from point instead
of from the beginning of the buffer."
  (save-excursion
    (let ((case-fold-search t)
          (re (org-re-property property nil (not value) value)))
      (cl-loop while (re-search-forward re nil t)
               when (if value
                        (org-at-property-p)
                      (org-entry-get (point) property nil t))
               return (progn
                        (org-back-to-heading t)
                        (point))))))

(defun org-make-toc--filter-tree (tree pred)
  "Return TREE with elements for which PRED returns non-nil."
  (cl-loop with properties
           with children
           for element in tree
           when (eql 'headline (car element))
           do (setq children (caddr element))
           if (funcall pred element)
           do (setq properties (cadr element))
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

(cl-defun org-make-toc--remove-ignored-entries (tree &key depth keep-all)
  "Return TREE without ignored entries, up to DEPTH.
When KEEP-ALL is non-nil, return all entries."
  (cl-loop when (and depth
                     (< depth 0))
           return nil

           for element in tree
           for type = (car element)
           for properties = (cadr element)
           for children = (cddr element)
           when (eql 'headline type)
           for result = (if keep-all
                            (list type
                                  properties
                                  (org-make-toc--remove-ignored-entries children
                                                                        :depth (when depth
                                                                                 (1- depth))
                                                                        :keep-all keep-all))
                          (pcase (org-element-property :TOC element)
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
                                                                                  (1- depth))
                                                                         :keep-all keep-all)))
                            ;; TOC entry; descend into but leave this entry blank so it won't be in the TOC
                            ((or "this" "children" "siblings")
                             (list type
                                   (plist-put properties :title nil)
                                   (org-make-toc--remove-ignored-entries children
                                                                         :depth (when depth
                                                                                  (1- depth))
                                                                         :keep-all keep-all)))
                            ;; Depth setting
                            ((and number (guard (string-to-number number)))
                             (list type
                                   properties
                                   (org-make-toc--remove-ignored-entries children
                                                                         :depth (or (when depth
                                                                                      (1- depth))
                                                                                    (1- (string-to-number number)))
                                                                         :keep-all keep-all)))
                            ;; Invalid setting
                            (other
                             (goto-char (org-element-property :begin element))
                             (user-error "Invalid value for TOC property at entry \"%s\": %s"
                                         (org-element-property :title element) other))))
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
                                    for link = (funcall org-make-toc-link-type-fn element)
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
      (if (= (buffer-size) 0)
          ;; Empty buffer.
          ""
        ;; Insert blank line after list.
        (goto-char (point-max))
        (insert "\n")
        ;; Remove excess indentation.
        (buffer-string)))))

(defun org-make-toc--remove-excess-indentation (s)
  "Remove excess indentation in string S.
Preserves indentation of each line relative to the others."
  (with-temp-buffer
    (insert s)
    (let* ((excess (progn
                     (goto-char (point-min))
                     (cl-loop while (re-search-forward (rx bol (1+ blank)) nil t)
                              minimize (length (match-string 0))
                              do (forward-line 1)
                              until (eobp)))))
      (when (and excess (> excess 0))
        (goto-char (point-min))
        (while (re-search-forward (rx-to-string `(seq bol (repeat 1 ,excess blank)) t)
                                  nil t)
          (replace-match "")
          (forward-line 1))))
    (buffer-string)))

(defun org-make-toc--link-entry-github (entry)
  "Return text for ENTRY converted to GitHub style link."
  (-when-let* ((title (org-element-property :title entry))
               (target (--> title
                            (downcase it)
                            (replace-regexp-in-string " " "-" it)
                            (replace-regexp-in-string "[^[:alnum:]_-]" "" it)))
               (filename (if org-make-toc-filename-prefix
                             (file-name-nondirectory (buffer-file-name))
                           "")))
    (org-make-link-string (concat "#" filename target)
                          (org-make-toc--visible-text title))))

(defun org-make-toc--link-entry-org (entry)
  "Return text for ENTRY converted to regular Org link."
  ;; FIXME: There must be a built-in function to do this, although it might be in `org-export'.
  (-when-let* ((title (org-element-property :title entry))
               (filename (if org-make-toc-filename-prefix
                             (concat "file:" (file-name-nondirectory (buffer-file-name)) "::")
                           "")))
    (org-make-link-string (concat filename title)
                          (org-make-toc--visible-text title))))

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

(defun org-make-toc--visible-text (string)
  "Return only visible text in STRING after fontifying it like in Org-mode.

`org-fontify-like-in-org-mode' is a very, very slow function
because it creates a new temporary buffer and runs `org-mode' for
every string it fontifies.  This function reuses a single
invisible buffer and only runs `org-mode' when the buffer is
created."
  ;; MAYBE: Use `org-sort-remove-invisible' instead?  Not sure if it does exactly the same thing.
  (let ((buffer (get-buffer " *org-make-toc-fontification*")))
    (unless buffer
      (setq buffer (get-buffer-create " *org-make-toc-fontification*"))
      (with-current-buffer buffer
        (buffer-disable-undo)
        (org-mode)
        (setq-local org-hide-emphasis-markers t)))
    (with-current-buffer buffer
      (insert string)
      ;; FIXME: "Warning: ‘font-lock-fontify-buffer’ is for interactive use only; use
      ;; ‘font-lock-ensure’ or ‘font-lock-flush’ instead."
      (font-lock-fontify-buffer)
      ;; This is more complicated than I would like, but the `org-find-invisible' and
      ;; `org-find-visible' functions don't seem to be appropriate to this task, so this works.
      (prog1
          (cl-flet ((visible-p () (not (get-char-property (point) 'invisible)))
                    (invisible-p () (get-char-property (point) 'invisible))
                    (forward-until (until)
                                   (cl-loop until (or (eobp) (funcall until))
                                            for pos = (next-single-property-change (point) 'invisible nil (point-max))
                                            while pos
                                            do (goto-char pos))
                                   (point))
                    (backward-until (until)
                                    (cl-loop until (or (eobp) (funcall until))
                                             for pos = (previous-single-property-change (point) 'invisible nil (point-max))
                                             while pos
                                             do (goto-char pos))
                                    (point)))
            (goto-char (point-min))
            (unless (visible-p)
              (forward-until #'visible-p))
            (setq string (cl-loop concat (buffer-substring (point) (forward-until #'invisible-p))
                                  until (eobp)
                                  do (forward-until #'visible-p))))
        (erase-buffer)))))

;;;; Mode

(define-minor-mode org-make-toc-mode
  "Add the `org-make-toc' command to the `before-save-hook' in the current Org buffer.
With prefix argument ARG, turn on if positive, otherwise off."
  :init-value nil
  (unless (derived-mode-p 'org-mode)
    (user-error "Not an Org buffer"))
  (funcall (if org-make-toc-mode #'add-hook #'remove-hook)
           'before-save-hook #'org-make-toc)
  (message (format "org-make-toc-mode %s."
                   (if org-make-toc-mode
                       "enabled"
                     "disabled"))))

;;;; Footer

(provide 'org-make-toc)

;;; org-make-toc.el ends here
