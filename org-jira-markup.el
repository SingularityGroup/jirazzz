;; -*- lexical-binding: t; -*-

(require 'ox-md)

(defun org-jira-markup-bold
    (_bold contents _info) "Transcode BOLD object into jira markup format.
CONTENTS is the text within bold markup.  INFO is a plist used as
a communication channel."
    (format "*%s*" contents))

(defun org-jira-markup--headline-title (level title)
  ""
  (format "h%s. %s\n" (number-to-string level) title))

(defun org-jira-markup--link (link desc info)
  (let* ((type (org-element-property :type link))
	(raw-path (org-element-property :path link))
	(path (cond
	       ((member type '("http" "https" "ftp" "mailto"))
		(concat type ":" raw-path))
	       ((string-equal  type "file")
		(org-export-file-uri (funcall link-org-files-as-md raw-path)))
	       (t raw-path))))
    (if desc
	(format "[%s|%s]" desc path)
      (format "[%s]" path))))

(defun org-jira-markup-headline (headline contents info)
  "Transcode HEADLINE element into Markdown format.
CONTENTS is the headline contents.  INFO is a plist used as
a communication channel."
  (unless (org-element-property :footnote-section-p headline)
    (let* ((level (org-export-get-relative-level headline info))
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
      (concat (org-jira-markup--headline-title level heading)
	      contents))))

(defun org-jira-markup-verbatim (block &rest _)
  (format "{noformat} %s {noformat}"
	  (org-element-property
	   :value block)))

(defun org-jira-markup-quote (block &rest _)
  (format "{quote} %s {quote}"
	  (org-element-property
	   :value block)))

(defun org-jira-markup-src-block (block &rest args)
  (format
   "{code:%s}\n%s\n{code}"
   (org-element-property
    :language block)
   (org-element-property
    :value block)))

(defun org-jira-markup-hoizontal-rule (&rest _) "----")

(org-export-define-derived-backend
    'jira-markup 'md
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
  ;; I just started implementing some and got bored
  ;; there is not really a reason for keeping the org-md ones except for reference
  ;; See ox-md.el for reference, too.
  ;; If you want something don't hesitate to implement it.
  '((bold . org-jira-markup-bold)
    ;; (center-block . org-md--convert-to-html)
    (code . org-md-verbatim)
    (example-block . org-jira-markup-quote)
    (headline . org-jira-markup-headline)
    (horizontal-rule . org-jira-markup-hoizontal-rule)
    ;; (inline-src-block . org-jira-markup-src-block)
    (inlinetask . org-md--convert-to-html)
    (inner-template . org-md-inner-template)
    (italic . org-md-italic)
    (item . org-md-item)
    (keyword . org-md-keyword)
    (latex-environment . org-md-latex-environment)
    (latex-fragment . org-md-latex-fragment)
    (line-break . org-md-line-break)
    (link . org-jira-markup--link)
    (node-property . org-md-node-property)
    (paragraph . org-md-paragraph)
    (plain-list . org-md-plain-list)
    (plain-text . org-md-plain-text)
    (property-drawer . org-md-property-drawer)
    (quote-block . org-md-quote-block)
    (section . org-md-section)
    (special-block . org-md--convert-to-html)
    (src-block . org-jira-markup-src-block)
    (table . org-md--convert-to-html)
    (template . org-md-template)
    (verbatim . org-jira-markup-verbatim)))

(defun org-jira-markup-export-as-jira-markup (&optional async subtreep visible-only)
  ""
  (interactive)
  (let ((org-export-with-toc nil))
    (org-export-to-buffer 'jira-markup "*Org MD Export*"
      async subtreep visible-only nil nil (lambda () (text-mode)))))
