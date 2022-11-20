;; -*- lexical-binding: t; -*-


(defun org-jira-markup-bold
    (_bold contents _info) "Transcode BOLD object into jira markup format.
CONTENTS is the text within bold markup.  INFO is a plist used as
a communication channel."
    (format "*%s*" contents))

(defun org-jira-markup--headline-title (level title)
  ""
  (format "h.%s %s" (number-to-string level) title))

(defun org-jira-markup-headline (headline contents info)
  "Transcode HEADLINE element into Markdown format.
CONTENTS is the headline contents.  INFO is a plist used as
a communication channel."
  (unless (org-element-property :footnote-section-p headline)
    (let* ((level (+ (org-export-get-relative-level headline info)
                     (1- (plist-get info :jira-markdown-toplevel-hlevel))))
	   (title (org-export-data (org-element-property :title headline) info))
	   (todo (and (plist-get info :with-todo-keywords)
		      (let ((todo (org-element-property :todo-keyword
							headline)))
			(and todo (concat (org-export-data todo info) " ")))))
	   (tags (and (plist-get info :with-tags)
		      (let ((tag-list (org-export-get-tags headline info)))
			(and tag-list
			     (concat "     " (org-make-tag-string tag-list))))))
	   (priority
	    (and (plist-get info :with-priority)
		 (let ((char (org-element-property :priority headline)))
		   (and char (format "[#%c] " char)))))
	   ;; Headline text without tags.
	   (heading (concat todo priority title)))
      (concat (org-jira-markdown--headline-title level heading)
	      contents))))

(org-export-define-derived-backend
    'jira-markdown 'md
  :filters-alist '((:filter-parse-tree . org-md-separate-elements))
  :menu-entry
  '(?m "Export to Jira Markup"
       ((?M "To temporary buffer"
	    (lambda (a s v b) (org-jira-markup-export-as-jira-markup a s v)))
	(?m "To file" (lambda (a s v b) (org-jira-markup-export-as-jira-markup a s v)))
	(?o "To file and open"
	    (lambda (a s v b)
	      (if a (org-jira-markup-export-as-jira-markup t s v)
		(org-open-file (org-jira-markdown-export-to-markdown nil s v)))))))
  :translate-alist

  '((bold . org-md-bold)
    (center-block . org-md--convert-to-html)
    (code . org-md-verbatim)
    (drawer . org-md--identity)
    (dynamic-block . org-md--identity)
    (example-block . org-md-example-block)
    (export-block . org-md-export-block)
    (fixed-width . org-md-example-block)
    (headline . org-md-headline)
    (horizontal-rule . org-md-horizontal-rule)
    (inline-src-block . org-md-verbatim)
    (inlinetask . org-md--convert-to-html)
    (inner-template . org-md-inner-template)
    (italic . org-md-italic)
    (item . org-md-item)
    (keyword . org-md-keyword)
    (latex-environment . org-md-latex-environment)
    (latex-fragment . org-md-latex-fragment)
    (line-break . org-md-line-break)
    (link . org-md-link)
    (node-property . org-md-node-property)
    (paragraph . org-md-paragraph)
    (plain-list . org-md-plain-list)
    (plain-text . org-md-plain-text)
    (property-drawer . org-md-property-drawer)
    (quote-block . org-md-quote-block)
    (section . org-md-section)
    (special-block . org-md--convert-to-html)
    (src-block . org-md-example-block)
    (table . org-md--convert-to-html)
    (template . org-md-template)
    (verbatim . org-md-verbatim))

  ;; '((bold . org-jira-markup-bold)
  ;;   (code . org-md-verbatim)
  ;;   (drawer . org-md--identity)
  ;;   (dynamic-block . org-md--identity)
  ;;   (example-block . org-md-example-block)
  ;;   (export-block . org-md-export-block)
  ;;   (fixed-width . org-md-example-block)
  ;;   (headline . org-jira-markup-headline)
  ;;   (horizontal-rule . org-md-horizontal-rule)
  ;;   (inline-src-block . org-md-verbatim)
  ;;   (inlinetask . org-md--convert-to-html)
  ;;   (inner-template . org-md-inner-template)
  ;;   (italic . org-md-italic)
  ;;   (item . org-md-item)
  ;;   (keyword . org-md-keyword)
  ;;   (latex-environment . org-md-latex-environment)
  ;;   (latex-fragment . org-md-latex-fragment)
  ;;   (line-break . org-md-line-break)
  ;;   (link . org-md-link)
  ;;   (node-property . org-md-node-property)
  ;;   (paragraph . org-md-paragraph)
  ;;   (plain-list . org-md-plain-list)
  ;;   (plain-text . org-md-plain-text)
  ;;   (property-drawer . org-md-property-drawer)
  ;;   (quote-block . org-md-quote-block)
  ;;   (section . org-md-section)
  ;;   (special-block . org-md--convert-to-html)
  ;;   (src-block . org-md-example-block)
  ;;   (table . org-md--convert-to-html)
  ;;   (template . org-md-template)
  ;;   (verbatim . org-md-verbatim))
  )

(defun org-jira-markup-export-as-jira-markup (&optional async subtreep visible-only)
  ""
  (interactive)
  (org-export-to-buffer 'jira-markdown "*Org MD Export*"
    async subtreep visible-only nil nil (lambda () (text-mode))))

(org-jira-markup-export-as-jira-markup)
