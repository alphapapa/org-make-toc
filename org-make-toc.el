
;;; Code:

(require 'cl-lib)
(require 'org)
(require 'dash)
(require 's)

;;;; Variables

(defgroup org-make-toc nil
  "Settings for `org-make-toc'."
  :group 'org
  :link '(url-link "http://github.com/alphapapa/org-make-toc"))

;;;; Commands

(defun org-make-toc-make-toc ()
  "Make or update table of contents in current buffer."
  (interactive)
  (when-let ((toc-position (or (org-find-property "TOC" "this")
                               (when (called-interactively-p)
                                 ;; Don't error unless the user called this function manually,
                                 ;; e.g. if it's added to the before-save-hook in all Org mode
                                 ;; files, don't error just because a file doesn't have a TOC.
                                 (user-error "No TOC node found.  A node must have the \"TOC\" property set to \"this\""))))
             (list (or (->> (cddr (org-element-parse-buffer 'headline))
                            (org-make-toc--remove-ignored-entries)
                            (org-make-toc--remove-higher-level-than-toc)
                            (org-make-toc--tree-to-list))
                       (error "Failed to build table of contents"))))
    (org-make-toc--replace-entry-contents toc-position list)))

;;;; Functions

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
  (cl-loop when (and depth
                     (< depth 0))
           return nil

           for element in tree
           for type = (car element)
           for properties = (second element) ; (list :title (org-element-property :title element));; (second element)  ; (org-element-property :title element)

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
                                                #'org-make-toc--is-toc-entry
                                                #'org-make-toc--element-level)))
    (org-make-toc--filter-tree tree (lambda (element)
                                      (>= (org-element-property :level element)
                                         toc-level)))))

;;;;; Predicates

(defun org-make-toc--is-toc-entry (element)
  (org-element-property :title element)
  (string= "this" (org-element-property :TOC element)))

(defun org-make-toc--include-heading-p (element)
  "Return non-nil if heading should be included in ToC."
  (pcase (org-element-property :TOC element)
    ("ignore" nil)
    ('nil t)
    (other (user-error "Invalid value for TOC property at position %s: "
                       (point) other))))

;;;;; Transformer

(defun org-make-toc--tree-to-list (tree)
  (let* ((contents (s-join "\n"
                           (cl-loop with toc-level = (org-make-toc--toc-level tree)
                                    for element in tree
                                    for level = (or (org-element-property :level element) 0)
                                    for indent = (s-repeat (* 2 level) " ")
                                    for title = (org-element-property :title element)
                                    for children = (org-make-toc--tree-to-list (caddr element))
                                    for link = (org-make-toc--link-entry-github element)
                                    collect (concat indent "-" "  " link "\n" children)))))
    (with-temp-buffer
      (insert contents)
      (goto-char (point-min))
      (flush-lines (rx bol (optional (1+ space) "-" (1+ space)) eol))
      (buffer-string))))

(defun org-make-toc--link-entry-github (entry)
  "Return text for ENTRY converted to GitHub style link."
  (-when-let* ((title (org-element-property :title entry))
               (title (org-link-display-format title))
               (target (replace-regexp-in-string " " "-" (downcase title)))
               (target (replace-regexp-in-string "[^[:alnum:]_-]" "" target)))
    (format "[[#%s][%s]]" target title)))

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
    (forward-line 1)
    (let ((end (org-entry-end-position)))
      (org-end-of-meta-data)
      (beginning-of-line)
      (setf (buffer-substring (point) end) contents))))

(provide 'org-make-toc)
