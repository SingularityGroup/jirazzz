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

(defun jinote-user->id (s)
  (cadr (s-split " - " s)))

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

(defun jinote-parse-transient-arg (s)
  (s-split "=" s))

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






(gethash
 :fields
 (-let* (((m)
	  (with-current-buffer
	      (find-file-noselect "ticket-1.edn")
	    (goto-char (point-min))
	    (parseedn-read)))
	 ;; (&hash :fields fields) m
	 )
   m
   ;; fields
   ))


(defun jinote--tags-data ()
  (save-excursion
    (goto-char (point-min))
    (let ((res)
	  (end (save-excursion
		 (or (re-search-forward "^$" nil t) (point-max)))))
      (while (re-search-forward "#\\+\\(.+?\\):\\s-+\\(.+?\\)$" end t)
	(push
	 (cons (match-string 1)
	       (match-string 2))
	 res))
      res)))

(defun jinote-tags-data (file)
  (with-temp-buffer
    (insert-file-contents file)
    (jinote--tags-data)))

(defun jinote-add-tag (tag value)
  (goto-char (point-min))
  (or (re-search-forward "^$" nil t)
      (goto-char (point-max)))
  (insert "#+" tag ":" value))

(defun jinote-update-tag (tag value)
  (if
      (assoc tag (jinote--tags-data))
      'fo
    'fa
    ;; (jinote-add-tag tag value)
    ))

(jinote-update-tag "foo" "bar")


(-let* (((m)
	 (with-current-buffer
	     (find-file-noselect
	      "ticket-1.edn")
	   (goto-char (point-min))
	   (parseedn-read)))
	((&hash :fields fields) m)
	((&hash :summary summary) fields)
	((&hash :issuetype issue-type) fields)
	((&hash :description description) fields))
  ;; summary
  (with-current-buffer
      (get-buffer-create jinote-buffer)
    (erase-buffer)
    (insert"summary: ")
    (insert summary)
    (insert "\n")
    (insert "issuetype:" (gethash :name issue-type))
    (insert "\n")
    (insert "\n")
    (insert "description:\n")
    (insert "\n")
    (insert description)
    (insert "\n")
    (pop-to-buffer (current-buffer))))



(let ((first))
  (org-babel-map-src-blocks (buffer-file-name)
    (unless
	first
      ( full-block)

      (setf first t))))
