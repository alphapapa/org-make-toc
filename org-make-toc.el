
;;; Code:

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
                          ("ignore"   ; Ignore this heading
                           nil)
                          ("ignore-children"  ; Ignore this heading's children
                           (list type properties))
                          ((or (pred not) "this" "")  ; Descend into tree
                           (list type properties (org-make-toc--remove-ignored-entries children)))
                          (other  ; Invalid setting
                           (progn
                             (goto-char (org-element-property :begin element))
                             (user-error "Invalid value for TOC property at entry \"%s\": %s"
                                         (org-element-property :title element) other))))
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

;;;;; Misc

(defun org-make-toc--element-level (element)
  (org-element-property :level element))
