
;;; Code:

(require 'cl-lib)
(require 'org)
(require 'dash)
(require 's)

;;;; Functions

;; FIXME: figure out for CERTAIN whether cdddr or cddr is the way to get children

(defun org-walk-tree (tree element-pred)
  (cl-loop for element in tree
           when (eql 'headline (car element))
           when (funcall element-pred element)
           collect (list 'headline
                         :name (org-element-property :title element)
                         :children (org-walk-tree (cdddr element) element-pred))))

(defun org-make-toc--filter-tree (tree pred)
  "Return tree with elements for which PRED returns non-nil."
  (cl-loop with properties
           for element in tree
           when (eql 'headline (car element))
           do (org-element-property :title element)
           if (funcall pred element)
           do (setq properties (second element))
           else do (setq properties nil)
           collect (list 'headline
                         properties
                         (org-make-toc--filter-tree (caddr element) pred))))

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

(defun org-make-toc--remove-ignored-entries (tree)
  (cl-loop for element in tree
           for type = (car element)
           for properties = (second element) ; (list :title (org-element-property :title element));; (second element)  ; (org-element-property :title element)
           for children = (cddr element)
           when (eql 'headline type)
           for result = (pcase (org-element-property :TOC element)
                          ;; Ignore this entry and its children
                          ("ignore"
                           nil)
                          ;; Keep this entry but ignore its children
                          ("ignore-children"
                           (list type properties))
                          ;; Normal entry: descend into tree
                          ((or (pred not) "")
                           (list type properties (org-make-toc--remove-ignored-entries children)))
                          ;; TOC entry; descend into but leave this entry blank so it won't be in the TOC
                          ("this"
                           (list type (plist-put properties :title nil) (org-make-toc--remove-ignored-entries children)))
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
                                                #'org-make-toc--is-toc-entry
                                                #'org-make-toc--element-level)))
    (org-make-toc--filter-tree tree (lambda (element)
                                      (>= (org-element-property :level element)
                                          toc-level)))))

(defun org-make-toc--include-heading-p (element)
  "Return non-nil if heading should be included in ToC."
  (pcase (org-element-property :TOC element)
    ("ignore" nil)
    ('nil t)
    (other (user-error "Invalid value for TOC property at position %s: "
                       (point) other))))

;;;;; Predicates

(defun org-make-toc--is-toc-entry (element)
  (org-element-property :title element)
  (string= "this" (org-element-property :TOC element)))

;;;;; Transformer

(defun org-make-toc--tree-to-list (tree)
  (let* ((contents (s-join "\n" (cl-loop with toc-level = (org-make-toc--toc-level tree)
                                         for element in tree
                                         for level = (or (org-element-property :level element) 0)
                                         for indent = (s-repeat (1+ level) " ")
                                         for title = (org-element-property :title element)
                                         for children = (org-make-toc--tree-to-list (caddr element))
                                         for link = (org-make-toc--link-entry-github element)
                                         collect (concat indent "-" " " link "\n" children))))
         (contents (with-temp-buffer
                     (insert contents)
                     (goto-char (point-min))
                     (flush-lines (rx bol
                                      (optional (1+ space) "-" (1+ space))
                                      eol))
                     (buffer-string))))
    contents))

(defun org-make-toc--link-entry-github (entry)
  "Return text for ENTRY converted to GitHub style link."
  (-when-let* ((title (org-element-property :title entry))
               (target (s-replace-all '((" " . "-")) (downcase title))))
    (concat "[[" "#" target "][" title "]]")))

;;;;; Misc

(defun org-make-toc--element-level (element)
  (org-element-property :level element))

(defun org-make-toc--toc-level (tree)
  (org-make-toc--first-in-tree tree
                               #'org-make-toc--is-toc-entry
                               #'org-make-toc--element-level))

(defun org-make-toc--replace-entry-contents (pos contents)
  "Replace the contents of entry at POS with CONTENTS."
  (save-excursion
    (goto-char pos)
    (let ((end (org-entry-end-position))
          last-match)
      ;; Skip past property drawer
      (while (or (org-at-drawer-p)
                 (not (equal last-match "END")))
        (re-search-forward org-drawer-regexp end)
        (setq last-match (match-string 1))
        (forward-line 1))
      (setf (buffer-substring (point) end) contents))))

(defun org-make-toc--replace-entry-contents (pos contents)
  "Replace the contents of entry at POS with CONTENTS."
  (save-excursion
    (goto-char pos)
    (forward-line 1)
    (let ((end (org-entry-end-position)))
      ;; Skip past property drawer
      (while (or (org-at-drawer-p)
                 (org-at-property-p))
        (re-search-forward org-drawer-regexp end)
        (forward-line 1))
      (forward-line 1)
      (setf (buffer-substring (1- (point)) end) contents))))
