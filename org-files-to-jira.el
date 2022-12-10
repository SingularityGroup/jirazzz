;; -*- lexical-binding: t; -*-

(defvar org-files-to-jira-bb-program (executable-find "bb"))

(defvar org-files-to-jira-jiraz-dir nil)

(setq org-files-to-jira-jiraz-dir
      (if load-file-name
          (file-name-directory load-file-name)
        (error "[org-files-to-jira] fatal: impossible to determine jiraz-path")))

(defvar org-files-to-jira-jiraz-config-file "my-jira-config.edn")

(defmacro org-files-to-jira-jiraz-env (&rest body)
  `(let
       ((default-directory org-files-to-jira-jiraz-dir)
	(process-environment
	 (cons
	  (concat
	   "JIRAZZZ_CONFIG_FILE=" org-files-to-jira-jiraz-config-file)
	  process-environment)))
     ,@body))

(defun org-files-to-jira-output-line (&rest args)
  (car (org-files-to-jira-jiraz-env
	(apply
	 #'process-lines
	 `(,org-files-to-jira-bb-program
	   "--init"
	   "./jirazzz"
	   "-x"
	   ,@args)))))

(defun org-files-to-jira-output-1 (&rest args)
  (car (read-from-string
	(apply #'org-files-to-jira-output-line args))))

(defun org-files-to-jira-memoize (op)
  (let ((map (make-hash-table :test 'equal)))
    (lambda (&rest args)
      (or
       (gethash args map)
       (puthash args (apply op args) map)))))

(defvar org-files-to-jira-components
  (org-files-to-jira-memoize
   (lambda ()
     (org-files-to-jira-output-1
      "jirazzz/components!"))))

(defvar org-files-to-jira-labels
  (org-files-to-jira-memoize
   (lambda ()
     (org-files-to-jira-output-1
      "jirazzz/labels!"))))

(defvar org-files-to-jira-users
  (org-files-to-jira-memoize
   (lambda ()
     (org-files-to-jira-output-1
      "jirazzz/users!"))))

(defun org-files-to-jira--create-issue (args)
  "Make a jira issue, return the issue key on success.
ARGS is a plist."
  (org-files-to-jira-jiraz-env
   (process-lines
    org-files-to-jira-bb-program
    "--init"
    "./jirazzz"
    "-x"
    "jirazzz/create-issue!")))

(defun ticket-fiels ()
  (let ((ticket-fields))
    (org-babel-map-src-blocks
	(buffer-file-name)
      (unless ticket-fields
	(setf
	 ticket-fields
	 (parseedn-read-str body))))
    ticket-fields))


(defun org-files-to-jira-org-export-to-jira-markup ()
  "Return the jira markup string of the current org buffer."
  (save-restriction
    (let ((b (current-buffer)))
      (with-temp-buffer
	(let ((out-buff (current-buffer))
	      (org-export-show-temporary-export-buffer nil)
	      (org-export-with-toc nil))
	  (with-current-buffer
	      b
	    (widen)
	    (narrow-to-region
	     (progn
	       (goto-char
		(catch 'block-end
		  (org-babel-map-src-blocks
		      (buffer-file-name)
		    (throw 'block-end end-block))))
	       (org-skip-whitespace)
	       (point))
	     (point-max))
	    (org-export-to-buffer
		'jira-markup
		out-buff
	      nil
	      nil
	      t)))
	(buffer-string)))))

(defun org-files-to-jira--user (user-string)
  "Returns (NAME . ID) or USER-STRING."
  (when (string-match
	 "\\(.+?\\) - \\(.+?\\)$"
	 user-string)
    (cons
     (match-string 1 user-string)
     (match-string 2 user-string))))

(defun org-files-to-jira-user-name (user-string)
  (car (org-files-to-jira--user user-string)))

(defun org-files-to-jira-user-id (user-string)
  (cdr (org-files-to-jira--user user-string)))

(defun org-files-to-jira-transform-fields-block
    (op)
  "OP update and returns the ticket-fields (a hashmap)."
  ;; insert one if none
  (let ((ticket-fields))
    (org-babel-map-src-blocks
	(buffer-file-name)
      (unless ticket-fields
	(setf
	 ticket-fields
	 (funcall op (parseedn-read-str body)))
	(delete-region beg-body end-body)
	(goto-char beg-body)
	(insert (parseedn-print-str ticket-fields))
	(insert "\n")
	(let ((end (point-marker)))
	  (goto-char beg-body)
	  (while (re-search-forward "," nil t)
	    (replace-match "\n")))))
    ticket-fields))

(defun org-files-to-jira-set-assigne ()
  (org-files-to-jira-transform-fields-block
   (lambda (fields)
     (puthash
      :assignee
      `(:accountId
	,(org-files-to-jira-user-id
	  (completing-read
	   "Assignee: "
	   (funcall org-files-to-jira-users))))
      fields)
     fields)))

(defun org-files-to-jira-make-ticket ()
  "Make a ticket with current org-files-to-jira buffer."
  (interactive)
  (-let* ((temp-file (make-temp-file "org-files-to-jira-ticket" nil ".edn"))
	  (tkt (ticket-fiels))
	  ((&hash :description description) tkt))
    (pcase description
      (:below
       (puthash :description
		(org-files-to-jira-org-export-to-jira-markup)
		tkt))
      (_ nil))
    (with-current-buffer
	(find-file-noselect temp-file)
      (parseedn-print
       `(:fields ,tkt))
      (save-buffer))
    (let ((ticket (org-files-to-jira-output-line
		   "jirazzz/create-issue!"
		   ":in-file"
		   temp-file)))
      (unless ticket
	(user-error "Did not work: %s" temp-file))
      (message "jira: %s" ticket))))

(defun org-files-to-jira-delete-what-you-just-said (str)
  (delete-region
   (save-excursion
     (forward-char
      (* -1 (length str)))
     (point))
   (point)))

(defun org-files-to-jira-insert-into-vec-or-make-one (str)
  (let ((v (or
	    (save-excursion
	      (beginning-of-line)
	      (when
		  (re-search-forward "\\]" (point-at-eol) t)
		(save-restriction
		  (narrow-to-region
		   (save-excursion
		     (backward-list)
		     (skip-chars-backward "[:blank:]")
		     (forward-char)
		     (point))
		   (point-at-eol))
		  (goto-char (point-min))
		  (prog1
		      (car (read-from-string
			    (buffer-string)))
		    (delete-region
		     (point-min)
		     (point-max))))))
	    (vector))))
    (insert
     (with-output-to-string
       (prin1
	(apply
	 #'vector
	 (append
	  (cl-coerce v 'list)
	  (list str))))))))

(defun org-files-to-jira-fields--completion (symbol)
  (pcase
      symbol
    (:accountId (list
		 (completion-table-dynamic
		  (lambda (_) (funcall org-files-to-jira-users)))
		 :exit-function (lambda (str status)
				  (org-files-to-jira-delete-what-you-just-said
				   str)
				  (insert
				   (format
				    "\"%s\""
				    (org-files-to-jira-user-id str))))))
    (:description (list
		   (completion-table-dynamic
		    (lambda (_) '(":below")))))
    (:summary (list
	       (completion-table-dynamic
		(lambda (_)
		  (list
		   (format
		    "\"%s\""
		    (file-name-base (buffer-name))))))))
    (:assignee (list
		(completion-table-dynamic
		  (lambda (_) (funcall org-files-to-jira-users)))
		:exit-function (lambda (str status)
				 (org-files-to-jira-delete-what-you-just-said
				  str)
				 (insert
				  (format
				   "{:name \"%s\" :accountId \"%s\"}"
				   (org-files-to-jira-user-name str)
				   (org-files-to-jira-user-id str))))))
    (:labels (list
	      (completion-table-dynamic org-files-to-jira-labels)
	      :exit-function (lambda (str status)
			       (org-files-to-jira-delete-what-you-just-said
				str)
			       (org-files-to-jira-insert-into-vec-or-make-one str))))
    (:components
     (list
      (completion-table-dynamic org-files-to-jira-components)
      :exit-function
      (lambda (str status)
	(org-files-to-jira-delete-what-you-just-said str)
	(org-files-to-jira-insert-into-vec-or-make-one str))))
    (_ nil)))

(defun org-files-to-jira-symbol-before ()
  (save-excursion
    (forward-symbol -1)
    (symbol-at-point)))

(defun org-files-to-jira-symbol-bol ()
  (save-excursion
    (beginning-of-line)
    (forward-char
     (current-indentation))
    (symbol-at-point)))

(defun org-files-to-jira-ticket-fields-capf ()
  (when-let*
      ((p (point))
       (completion
	(or
	 (org-files-to-jira-fields--completion
	  (org-files-to-jira-symbol-before))
	 (org-files-to-jira-fields--completion
	  (org-files-to-jira-symbol-bol))
	 (list
	  (completion-table-dynamic
	   (lambda (_)
	     '(":components"
	       ":assignee"
	       ":accountId"
	       ":project"
	       ":summary"
	       ":description"
	       ":issuetype"
	       ":name"
	       ":key"
	       ":labels")))))))
    `(,p ,p ,@completion)))

(define-derived-mode org-files-to-jira-ticket-fields-mode
  clojure-mode
  "Jinote (org-files-to-jira) ticket"
  "Mode for modifying org-files-to-jira ticket fields."
  (add-hook
   'completion-at-point-functions
   #'org-files-to-jira-ticket-fields-capf nil t))
