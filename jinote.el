;; -*- lexical-binding: t; -*-

(defvar jinote-bb-program (executable-find "bb"))

(defvar jinote-jiraz-dir "/home/benj/repos/sg/jirazzz/")

(defmacro jinote-jiraz-env (&rest body)
  `(let
       ((default-directory jinote-jiraz-dir)
	(process-environment
	 (cons
	  "JIRAZZZ_CONFIG_FILE=my-jira-config.edn"
	  process-environment)))
     ,@body))

(defun jinote-jiraz-call (args)
  (let ((default-directory jinote-jiraz-dir)
	(process-environment
	 (cons
	  "JIRAZZZ_CONFIG_FILE=my-jira-config.edn"
	  process-environment)))
    (apply
     (append
      (list
       start-process
       "*jinote-jiraz*"
       "*jinote-jiraz*"
       jinote-bb-program
       "--init"
       "jirazzz")
      args))))


;; magit like issue buff

;; or really if you wonder then
;; consult + embark export is better

;; that way I can literally extend via embark
;; keymap
;; that is just the best

;; some default consult with mult sources sounds
;; cool


(defun jinote-consult-jql-issue-source (jql)

  )

;;
;;

(defun jinote-issue-buffer-setup ()
  )

(defun jinote-issue (key) (jinote-jiraz-call))

(get-buffer-create)
(jinote-jiraz-call "")

(defun jinote-select-assigne ()
  (interactive)
  (completing-read "Assigne: " '("foo")))

(call-interactively #'jinote-create-menu)


(easy-menu-define )


(transient-define-prefix transient-toys-hello ()
  "Say hello"
   [("h" "hello" (lambda () (interactive) (message "hello")))])

(transient-toys-hello)


(defun transient-toys--wave () (interactive) (message "foo"))

(transient-define-prefix transient-toys-wave ()
  "Wave at the user"
  [("w" "wave" transient-toys--wave :transient t)])

(transient-toys-wave)

(transient-define-suffix transient-toys--wave ()
  "Wave at the user"
  :transient nil
  :description "wave"
  (interactive)
  (message (propertize
            (format "Waves at %s" (current-time-string))
            'face 'success)))

(transient-define-prefix transient-toys-wave ()
  "Wave at the user"
  :value '("--switch" "-switch=default")
  [["Arguments"
    "-s" "switch" "--switch"]
   ["group one"
    ("wo" "wave one" transient-toys--wave)]])

(transient-toys-wave)




(defun transient-toys--animal-choices (complete-me filter-p completion-type)
 ;; complete-me: whatever the user has typed so far
 ;; filter-p: function you should use to filter candidates (only nil seen so far)
 ;; completion-type: t on first input and (metadata . alist) thereafter
 ;;
 ;; Documentation is from Emacs.  This is not transient-specific behavior
 ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Programmed-Completion.html

 (if (eq 0 (random 2))
     '("fox" "kitten" "otter")
   '("ant" "peregrine" "zebra")))


(transient-define-prefix transient-toys-animals ()
  "Select animal"
  ["Arguments"
   ("-a" "Animal" "--animal="
    :always-read t ; don't allow unsetting, just read a new value
    :choices transient-toys--animal-choices)])
(transient-define-prefix transient-toys-animals ()
  "Select animal"
  ["Arguments"
   ("-a" "Animal" "--animal="
    :choices ("fox" "foo"))])

(transient-toys-animals)

(defun jinote-users ())

(defun memoize (op)
  (let ((m (make-hash-table :test 'equal)))
    (lambda (&rest args)
      (or
       (gethash args m)
       (apply op args)))))

(defvar
  jinote-users-fn
  (memoize
   (lambda ()
     (jinote-jiraz-env
      (process-lines
       jinote-bb-program
       "--init"
       "./jirazzz"
       "-x"
       "jirazzz/print-users")))))

(jinote-jiraz-env
 (start-process
  "ji"
  "ji"
  jinote-bb-program
 "--init"
  "./jirazzz"
  "-x"
  "jirazzz/print-users"))

(defun jinote-user->id (user-string)
  (cadr (s-split " - " user-string)))

(defun jinote-read-user (prompt initial-input history)
  (interactive)
  (completing-read prompt jinote-users nil nil initial-input history))

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

(transient-define-prefix jinote-create-issue ()
  "Create issue."
  ["Arguments"
   ("-a" "Assignee" ":asignee="
    :reader
    jinote-read-user)]
  [("c" "create"
    (lambda ()
      (interactive)
      (setq my-args
	(transient-args 'jinote-create-issue))))])

(defun jinote-parse-transient-arg (user-string)
  (s-split "=" user-string))

(mapcar #'jinote-parse-transient-arg my-args)

(jinote-jiraz-env
 (process-lines
  jinote-bb-program
  "--init"
  "./jirazzz"
  "-x"
  "jirazzz/create-issue!"
  ":assignee"
  "5ace46054fe2de2a7f6302bf"))

(jinote-create-issue)


(defvar jinote-buffer "*jinote*")

(pop-to-buffer
 (get-buffer-create jinote-buffer))



;; (gethash
;;  :fields
;;  (-let* (((m)
;; 	  (with-current-buffer
;; 	      (find-file-noselect "ticket-1.edn")
;; 	    (goto-char (point-min))
;; 	    (parseedn-read)))
;; 	 ;; (&hash :fields fields) m
;; 	 )
;;    m
;;    ;; fields
;;    ))


;; (defun jinote--tags-data ()
;;   (save-excursion
;;     (goto-char (point-min))
;;     (let ((res)
;; 	  (end (save-excursion
;; 		 (or (re-search-forward "^$" nil t) (point-max)))))
;;       (while (re-search-forward "#\\+\\(.+?\\):\\s-+\\(.+?\\)$" end t)
;; 	(push
;; 	 (cons (match-string 1)
;; 	       (match-string 2))
;; 	 res))
;;       res)))

;; (defun jinote-tags-data (file)
;;   (with-temp-buffer
;;     (insert-file-contents file)
;;     (jinote--tags-data)))

;; (defun jinote-add-tag (tag value)
;;   (goto-char (point-min))
;;   (or (re-search-forward "^$" nil t)
;;       (goto-char (point-max)))
;;   (insert "#+" tag ":" value))

;; (defun jinote-update-tag (tag value)
;;   (if
;;       (assoc tag (jinote--tags-data))
;;       'fo
;;     'fa
;;     ;; (jinote-add-tag tag value)
;;     ))

(jinote-update-tag "foo" "bar")

;; (-let* (((m)
;; 	 (with-current-buffer
;; 	     (find-file-noselect
;; 	      "ticket-1.edn")
;; 	   (goto-char (point-min))
;; 	   (parseedn-read)))
;; 	((&hash :fields fields) m)
;; 	((&hash :summary summary) fields)
;; 	((&hash :issuetype issue-type) fields)
;; 	((&hash :description description) fields))
;;   ;; summary
;;   (with-current-buffer
;;       (get-buffer-create jinote-buffer)
;;     (erase-buffer)
;;     (insert"summary: ")
;;     (insert summary)
;;     (insert "\n")
;;     (insert "issuetype:" (gethash :name issue-type))
;;     (insert "\n")
;;     (insert "\n")
;;     (insert "description:\n")
;;     (insert "\n")
;;     (insert description)
;;     (insert "\n")
;;     (pop-to-buffer (current-buffer))))


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

(with-current-buffer
    "ticket.org"
  (-let* ((tkt (ticket-fiels))
	  ((&hash :description description) tkt))
    (pcase description
      (:below
       (puthash :description
		(jinote-org-export-to-jira-markup)
		tkt))
      (_ nil))
    (with-current-buffer
	(find-file-noselect "1.edn")
      (erase-buffer)
      (parseedn-print
       `(:fields ,tkt))
      (save-buffer))))

(jinote-jiraz-env
 (process-lines
  jinote-bb-program
  "--init"
  "./jirazzz"
  "-x"
  "jirazzz/create-issue!"
  ":in-file"
  "1.edn"))

(defvar
  jinote-users
  (car
   (read-from-string
    (car
     (jinote-jiraz-env
      (process-lines
       jinote-bb-program
       "--init"
       "./jirazzz"
       "-x"
       "jirazzz/users!"))))))

(completing-read "Jira user: " jinote-users)

(defun jinote-user-id (user-string)
  (when (string-match
	 ".+? - \\(.+?\\)$"
	 user-string)
    (match-string 1 user-string)))

(let ((user-string "fo - bar-fol"))
  (when (string-match  ".+? - \\(.+?\\)$" user-string)
    (match-string 1 user-string)))

(defun jinote-set-assignee ()
  (interactive)
  (jinote-user-id
   (completing-read "Assignee: " jinote-users)))

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
	   jinote-users)))
      fields)
     fields)))

(defun jinote-make-ticket ()
  "Make a ticket with current jinote buffer."
  (interactive)
  (-let* ((tkt (ticket-fiels))
	  ((&hash :description description) tkt))
    (pcase description
      (:below
       (puthash :description
		(jinote-org-export-to-jira-markup)
		tkt))
      (_ nil))
    (with-current-buffer
	(find-file-noselect "1.edn")
      (erase-buffer)
      (parseedn-print
       `(:fields ,tkt))
      (save-buffer)))
  (shell-command
   (concat
    "browse-sg-ticket.clj "
    (car
     (jinote-jiraz-env
      (process-lines
       jinote-bb-program
       "--init"
       "./jirazzz"
       "-x"
       "jirazzz/create-issue!"
       ":in-file"
       "1.edn"))))))

;; idea:
;; make a prog mode for the ticket fields
;; then capf
(defun jinote-delete-what-you-just-said (str)
  (delete-region
   (save-excursion
     (forward-char
      (* -1 (length str)))
     (point))
   (point)))

(defun jinote-simple-memoize (f)
  (let ((val))
    (lambda (&rest _)
      (or val (setf val (funcall f))))))

(defun jinote-insert-into-vec-or-make-one (str)
(let ((v (if (looking-at-p "\\s-?\]")
	     (save-excursion
	       (up-list 1)
	       (save-restriction
		 (narrow-to-region
		  (save-excursion
		    (backward-list))
		  (point))
		 (goto-char (point-min))
		 (prog1
		     (car (read-from-string
			   (buffer-string)))
		   (delete-region
		    (point-min)
		    (point-max)))))
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
		  (jinote-simple-memoize
		   (lambda () jinote-users)))
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
		 (lambda (_) jinote-users))
		:exit-function (lambda (str status)
				 (jinote-delete-what-you-just-said
				  str)
				 (insert
				  "{:accountId "
				  (format
				   "\"%s\""
				   (jinote-user-id str))
				  "}"))))
    (:labels (list
		(completion-table-dynamic
		 jinote-labels)
		:exit-function (lambda (str status)
				 (jinote-delete-what-you-just-said
				  str)
				 (insert
				  "{:accountId "
				  (format
				   "\"%s\""
				   (jinote-user-id str))
				  "}"))))
    (:components
     (list
      (completion-table-dynamic jinote-components)
      :exit-function
      (lambda (str status)
	(jinote-delete-what-you-just-said str)
	(jinote-insert-into-vec-or-make-one str))))
    (_ nil)))

(file-name-base (buffer-name (current-buffer)))

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
	       ":labels"))))
	 )))
    `(,p ,p ,@completion)))

(define-derived-mode jinote-ticket-fields-mode
  clojure-mode
  "Jinote ticket"
  "Mode for modifying jinote ticket fields."
  (add-hook
   'completion-at-point-functions
   #'jinote-ticket-fields-capf nil t))

completion-at-point-functions
;; labels
;; components

(defvar jinote-components
  (jinote-simple-memoize
   (lambda ()
     (car
      (read-from-string
       (car
	(jinote-jiraz-env
	 (process-lines
	  jinote-bb-program
	  "--init"
	  "./jirazzz"
	  "-x"
	  "jirazzz/components!"))))))))

(defvar jinote-labels
  (jinote-simple-memoize
   (lambda ()
     (car
      (read-from-string
       (car
	(jinote-jiraz-env
	 (process-lines
	  jinote-bb-program
	  "--init"
	  "./jirazzz"
	  "-x"
	  "jirazzz/labels!"))))))))
