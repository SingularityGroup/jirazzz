;;; org-files-to-jira.el --- Make jira cards from org files.  -*- lexical-binding: t; -*-

;;; Commentary:
;; Define jira cards in an org file, make jira cards using jirazzz, which is included in this software.


;;; Code:

(require 'org-jira-markup)
(require 'parseedn)
(require 'clojure-mode)

(defvar org-files-to-jira-bb-program (executable-find "bb"))

(defvar org-files-to-jira-browse-on-create t
  "Wheter to browse a ticket when you create it.")

(defvar org-files-to-jira-jiraz-dir nil)

(setq org-files-to-jira-jiraz-dir
      (if load-file-name
          (file-name-directory load-file-name)
        (error "[org-files-to-jira] fatal: impossible to determine jiraz-path")))

(defvar org-files-to-jira-jiraz-config-file (expand-file-name
					     "my-jira-config.edn"
					     org-files-to-jira-jiraz-dir))

(defmacro org-files-to-jira-jiraz-env (&rest body)
  "Setup `process-environment' for a jiraz program call and execute BODY."
  `(let
       ((default-directory org-files-to-jira-jiraz-dir)
	(process-environment
	 (cons
	  (concat
	   "JIRAZZZ_CONFIG_FILE=" org-files-to-jira-jiraz-config-file)
	  process-environment)))
     ,@body))


(defun org-files-to-jira-process-lines (program args)
  "Execute PROGRAM with ARGS, returning its output as a list of lines.
Pop to the process buffer, if non 0 exit."
  (with-current-buffer (get-buffer-create "*org-files-to-jira-bb*")
    (let ((inhibit-read-only t))
      (erase-buffer))
    (let ((status (apply #'call-process program nil (current-buffer) nil args)))
      (unless (eq status 0)
	(pop-to-buffer (current-buffer))
	(error "%s exited with status %s" program status))
      (goto-char (point-min))
      (let (lines)
	(while (not (eobp))
	  (setq lines (cons (buffer-substring-no-properties
			     (line-beginning-position)
			     (line-end-position))
			    lines))
	  (forward-line 1))
	(nreverse lines)))))

(defun org-files-to-jira-output-line (&rest args)
  "Return the first line of jiraz output.
Call jiraz with ARGS, which should start with a function symbol."
  (car
   (org-files-to-jira-jiraz-env
    (org-files-to-jira-process-lines
     org-files-to-jira-bb-program
     `("--init"
       "./jirazzz"
       "-x"
       ,@args)))))

(defun org-files-to-jira-output-1 (&rest args)
  "Call `org-files-to-jira-output-line' with ARGS and read it as lisp data."
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
  (cl-labels ((insert-1 (ticket-fields beg)
			(defvar my-ticket-fields ticket-fields)
		(insert (parseedn-print-str ticket-fields))
			(insert "\n")
			(let ((end (point-marker)))
			  (goto-char beg)
			  (while (re-search-forward "," nil t)
			    (replace-match "\n"))
			  (goto-char end))))
    (if (not
	 (save-excursion
	   (re-search-forward
	    "#\\+begin_src jinote-ticket-fields" nil t)))
	(save-excursion
	  (goto-char (point-min))
	  (open-line 1)
	  (insert "#+begin_src jinote-ticket-fields\n")
	  (defvar my-m-2 (funcall op (parseedn-read-str "{}")))
	  (insert-1 (funcall op (parseedn-read-str "{}")) (point))
	  (insert "#+end_src\n"))
      (let ((ticket-fields))
	(org-babel-map-src-blocks
	    (buffer-file-name)
	  (unless ticket-fields
	    (setf
	     ticket-fields
	     (funcall op (parseedn-read-str body)))
	    (delete-region beg-body end-body)
	    (goto-char beg-body)
	    (insert-1 (funcall op ticket-fields))))
	ticket-fields))))

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

(defun org-files-to-jira-temp-file () (make-temp-file "org-files-to-jira-ticket" nil ".edn"))

(defun org-files-to-jira-create-ticket-file (temp-file)
  "Create a ticket temp file that we would use for creating a ticket."
  (-let* ((tkt (ticket-fiels))
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
    temp-file))

(defun org-files-to-jira-data-file ()
  "Create a ticket temp file that we would use for creating a ticket.
Pop to that file and nothing else."
  (interactive)
  (find-file (org-files-to-jira-create-ticket-file (org-files-to-jira-temp-file))))

(defun org-files-to-jira-create-ticket ()
  "Make a ticket with current org-files-to-jira buffer."
  (interactive)
  (let ((temp-file (org-files-to-jira-temp-file)))
    (unwind-protect
	(org-files-to-jira-create-ticket-file
	 temp-file)
      (when (file-exists-p temp-file)
	(delete-file temp-file)))))

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

(defun org-files-to-jira-create-1 (&optional file)
  "Create a jira card. Assume that FILE contains field edn."
  (interactive "fticket-fields edn:")
  (org-files-to-jira-output-line
   "jirazzz/create-issue!"
   ":in-file"
   file))

(defun org-files-to-jira-select-keys (m keys)
  (cl-loop for key being the hash-keys of m
	   when (not (member key keys))
	   do
	   (remhash key m))
  m)

(defun org-files-to-jira-make-org-file (&optional file)
  "Insert a org-files-to-jira source block,
create FILE if it does not exist."
  (interactive)
  (let ((file (or file (buffer-file-name))))
    (with-current-buffer (find-file file)
      (org-files-to-jira-transform-fields-block
       (lambda (m1)
	 (if
	     (gethash :project m1)
	     m1
	   (with-current-buffer
	       (find-file-noselect org-files-to-jira-jiraz-config-file)
	     (goto-char (point-min))
	     (org-files-to-jira-select-keys
	      (car (parseedn-read))
	      '(:description :below
		:project
		:assignee
		:issue-type)))))))))

(provide 'org-files-to-jira)

;;; org-files-to-jira.el ends here
