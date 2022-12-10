;; -*- lexical-binding: t; -*-

(defvar jinote-bb-program (executable-find "bb"))

(defvar jinote-jiraz-dir nil)

(setq jinote-jiraz-dir
      (if load-file-name
          (file-name-directory load-file-name)
        (error "[jinote] fatal: impossible to determine jiraz-path")))

(defvar jinote-jiraz-config-file "my-jira-config.edn")

(defmacro jinote-jiraz-env (&rest body)
  `(let
       ((default-directory jinote-jiraz-dir)
	(process-environment
	 (cons
	  (concat
	   "JIRAZZZ_CONFIG_FILE=" jinote-jiraz-config-file)
	  process-environment)))
     ,@body))

(defun jinote-output-line (&rest args)
  (car (jinote-jiraz-env
	(apply
	 #'process-lines
	 `(,jinote-bb-program
	   "--init"
	   "./jirazzz"
	   "-x"
	   ,@args)))))

(defun jinote-output-1 (&rest args)
  (car (read-from-string
	(apply #'jinote-output-line args))))

(defun jinote-memoize (op)
  (let ((map (make-hash-table :test 'equal)))
    (lambda (&rest args)
      (or
       (gethash args map)
       (puthash args (apply op args) map)))))

(defvar jinote-components
  (jinote-memoize
   (lambda ()
     (jinote-output-1
      "jirazzz/components!"))))

(defvar jinote-labels
  (jinote-memoize
   (lambda ()
     (jinote-output-1
      "jirazzz/labels!"))))

(defvar jinote-users
  (jinote-memoize
   (lambda ()
     (jinote-output-1
      "jirazzz/users!"))))

(defun jinote--create-issue (args)
  "Make a jira issue, return the issue key on success.
ARGS is a plist."
  (jinote-jiraz-env
   (process-lines
    jinote-bb-program
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


(defun jinote-org-export-to-jira-markup ()
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

(defun jinote--user (user-string)
  "Returns (NAME . ID) or USER-STRING."
  (when (string-match
	 "\\(.+?\\) - \\(.+?\\)$"
	 user-string)
    (cons
     (match-string 1 user-string)
     (match-string 2 user-string))))

(defun jinote-user-name (user-string)
  (car (jinote--user user-string)))

(defun jinote-user-id (user-string)
  (cdr (jinote--user user-string)))

(defun jinote-transform-fields-block
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

(defun jinote-set-assigne ()
  (jinote-transform-fields-block
   (lambda (fields)
     (puthash
      :assignee
      `(:accountId
	,(jinote-user-id
	  (completing-read
	   "Assignee: "
	   (funcall jinote-users))))
      fields)
     fields)))

(defun jinote-make-ticket ()
  "Make a ticket with current jinote buffer."
  (interactive)
  (-let* ((temp-file (make-temp-file "jinote-ticket" nil ".edn"))
	  (tkt (ticket-fiels))
	  ((&hash :description description) tkt))
    (pcase description
      (:below
       (puthash :description
		(jinote-org-export-to-jira-markup)
		tkt))
      (_ nil))
    (with-current-buffer
	(find-file-noselect temp-file)
      (parseedn-print
       `(:fields ,tkt))
      (save-buffer))
    (let ((ticket (jinote-output-line
		   "jirazzz/create-issue!"
		   ":in-file"
		   temp-file)))
      (unless ticket
	(user-error "Did not work: %s" temp-file))
      (message "jira: %s" ticket))))

(defun jinote-delete-what-you-just-said (str)
  (delete-region
   (save-excursion
     (forward-char
      (* -1 (length str)))
     (point))
   (point)))

(defun jinote-insert-into-vec-or-make-one (str)
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

(defun jinote-fields--completion (symbol)
  (pcase
      symbol
    (:accountId (list
		 (completion-table-dynamic
		  (lambda (_) (funcall jinote-users)))
		 :exit-function (lambda (str status)
				  (jinote-delete-what-you-just-said
				   str)
				  (insert
				   (format
				    "\"%s\""
				    (jinote-user-id str))))))
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
		  (lambda (_) (funcall jinote-users)))
		:exit-function (lambda (str status)
				 (jinote-delete-what-you-just-said
				  str)
				 (insert
				  (format
				   "{:name \"%s\" :accountId \"%s\"}"
				   (jinote-user-name str)
				   (jinote-user-id str))))))
    (:labels (list
	      (completion-table-dynamic jinote-labels)
	      :exit-function (lambda (str status)
			       (jinote-delete-what-you-just-said
				str)
			       (jinote-insert-into-vec-or-make-one str))))
    (:components
     (list
      (completion-table-dynamic jinote-components)
      :exit-function
      (lambda (str status)
	(jinote-delete-what-you-just-said str)
	(jinote-insert-into-vec-or-make-one str))))
    (_ nil)))

(defun jinote-symbol-before ()
  (save-excursion
    (forward-symbol -1)
    (symbol-at-point)))

(defun jinote-symbol-bol ()
  (save-excursion
    (beginning-of-line)
    (forward-char
     (current-indentation))
    (symbol-at-point)))

(defun jinote-ticket-fields-capf ()
  (when-let*
      ((p (point))
       (completion
	(or
	 (jinote-fields--completion
	  (jinote-symbol-before))
	 (jinote-fields--completion
	  (jinote-symbol-bol))
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

(define-derived-mode jinote-ticket-fields-mode
  clojure-mode
  "Jinote ticket"
  "Mode for modifying jinote ticket fields."
  (add-hook
   'completion-at-point-functions
   #'jinote-ticket-fields-capf nil t))
