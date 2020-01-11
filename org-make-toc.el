;;; org-make-toc.el --- Automatic tables of contents for Org files  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; URL: http://github.com/alphapapa/org-make-toc
;; Version: 0.4
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
             for pos = (org-make-toc--next-toc-position)
             while pos
             do (progn
                  (goto-char pos)
                  (when (org-make-toc--update-toc-at-point)
                    (setq made-toc t)))
             finally do (unless made-toc
                          (message "org-make-toc: No TOC node found.")))))

;;;###autoload
(defun org-make-toc-at-point ()
  "Make or update table of contents at current entry."
  (interactive)
  (unless (org-make-toc--update-toc-at-point)
    (user-error "No TOC node found")))

;;;###autoload
(defun org-make-toc-insert ()
  "Insert \":CONTENTS:\" drawer at point."
  (interactive)
  (org-insert-drawer nil "CONTENTS"))

;;;; Functions

(defun org-make-toc--next-toc-position ()
  "Return position of next TOC, or nil."
  (save-excursion
    (when (and (re-search-forward (rx bol ":CONTENTS:" (0+ blank) eol) nil t)
               (org-at-drawer-p))
      (point))))

(defun org-make-toc--update-toc-at-point ()
  "Make or update table of contents at current entry."
  (when-let* ((toc-string (org-make-toc--toc-at-point)))
    (org-make-toc--replace-entry-contents toc-string)
    t))

(defun org-make-toc--toc-at-point ()
  "Return TOC tree for entry at point."
  (cl-labels ((children-p ()
                          (let ((level (org-current-level)))
                            (save-excursion
                              (when (outline-next-heading)
                                (> (org-current-level) level)))))
              (next-sibling ()
                            (let ((pos (point)))
                              (org-forward-heading-same-level 1 'invisible-ok)
                              (/= pos (point))))
              (descendants (&key depth force-p)
                           (when (and (or (null depth) (> depth 0))
                                      (children-p))
                             (save-excursion
                               (save-restriction
                                 (org-narrow-to-subtree)
                                 (outline-next-heading)
                                 (cl-loop collect (append (cons (entry :force-p force-p)
                                                                (unless (entry-match :ignore 'descendants)
                                                                  (descendants :depth (when depth
                                                                                        (1- depth))
                                                                               :force-p force-p))))
                                          while (next-sibling))))))
              (siblings (&key depth force-p)
                        (save-excursion
                          (save-restriction
                            (when (org-up-heading-safe)
                              (org-narrow-to-subtree)
                              (outline-next-heading))
                            (cl-loop collect (cons (entry :force-p force-p)
                                                   (unless (entry-match :ignore 'descendants)
                                                     (descendants :depth (when depth
                                                                           (1- depth))
                                                                  :force-p force-p)))
                                     while (next-sibling)))))
              (entry (&key force-p)
                     (unless (and (not force-p) (entry-match :ignore 'this))
                       (funcall org-make-toc-link-type-fn)))
              (entry-match (property value)
                           (when-let* ((found-value (entry-property property)))
                             (or (equal value found-value)
                                 (and (listp found-value) (member value found-value)))))
              (entry-property (property)
                              (plist-get (read (concat "(" (org-entry-get (point) "TOC") ")"))
                                         property)))
    (save-excursion
      (save-restriction
        (-let* (((&plist :include :depth :force force-p)
                 (read (concat "(" (org-entry-get (point) "TOC") ")")))
                (tree (pcase include
                        ;; Set bounds.
                        ('all (org-with-wide-buffer
                               (goto-char (point-min))
                               (when (org-before-first-heading-p)
                                 (outline-next-heading))
                               (siblings :depth depth :force-p force-p)))
                        ('descendants (descendants :depth depth :force-p force-p))
                        ('siblings (siblings :depth depth :force-p force-p)))))
          (org-make-toc--tree-to-list tree))))))

(defun org-make-toc--tree-to-list (tree)
  "Return list string for TOC TREE."
  (cl-labels ((tree (tree depth)
                    (when (> (length tree) 0)
                      (when-let* ((entries (->> (append (when (car tree)
                                                          (list (concat (s-repeat depth "  ")
                                                                        "- " (car tree))))
                                                        (--map (tree it (1+ depth))
                                                               (cdr tree)))
                                                -non-nil -flatten)))
                        (s-join "\n" entries)))))
    (->> tree
         (--map (tree it 0))
         -flatten (s-join "\n"))))

(defun org-make-toc--link-entry-github ()
  "Return text for ENTRY converted to GitHub style link."
  ;; FIXME: org-get-heading takes more arguments in newer Orgs.
  (-when-let* ((title (org-get-heading t t))
               (target (--> title
                            (downcase it)
                            (replace-regexp-in-string " " "-" it)
                            (replace-regexp-in-string "[^[:alnum:]_-]" "" it)))
               (filename (if org-make-toc-filename-prefix
                             (file-name-nondirectory (buffer-file-name))
                           "")))
    (org-make-link-string (concat filename "#" target)
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

(defun org-make-toc--replace-entry-contents (contents)
  "Replace the contents of TOC in entry at point with CONTENTS.
Replaces contents of :CONTENTS: drawer."
  (save-excursion
    (org-back-to-heading)
    (let* ((end (org-entry-end-position))
           contents-beg contents-end)
      (when (and (re-search-forward (rx bol ":CONTENTS:" (0+ blank) eol) end t)
                 (org-at-drawer-p))
        ;; Set the end first, then search back and skip any ":TOC:" property line in the drawer.
        (setf contents-end (save-excursion
                             (when (re-search-forward (rx bol ":END:" (0+ blank) eol) end)
                               (match-beginning 0)))
              contents-beg (progn
                             (when (save-excursion
                                     (forward-line 1)
                                     (looking-at-p (rx bol ":TOC:" (0+ blank) (group (1+ nonl)))))
                               (forward-line 1))
                             (point-at-eol))
              contents (concat "\n" (string-trim contents) "\n")))
      (setf (buffer-substring contents-beg contents-end) contents))))

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
      (font-lock-ensure)
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

;;;###autoload
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
