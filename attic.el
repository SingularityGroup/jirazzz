
;; magit like issue buff

;; or really if you wonder then
;; consult + embark export is better

;; that way I can literally extend via embark
;; keymap
;; that is just the best

;; some default consult with mult sources sounds
;; cool


(transient-define-prefix transient-toys-hello ()
  "Say hello"
   [("h" "hello" (lambda () (interactive) (message "hello")))])

(transient-toys-hello)


(defun transient-toys--wave () (interactive) (message "foo"))

(transient-define-prefix transient-toys-wave ()
  "Wave at the user"
  [("w" "wave" transient-toys--wave :transient t)])

(transient-toys-wave)



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
