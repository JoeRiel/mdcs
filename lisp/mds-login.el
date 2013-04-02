;;; mds-login.el --- Login methods
;; Attempt at providing some login stuff.
;; We'll see if it sticks...


;;; Commentary:
;; 


;;; Code:

(require 'sha1)
(require 'mds-client)
(declare-function mds-client-set-id "mds-client")
(declare-function mds-client-set-status "mds-client")


(defconst mds-login-id-re ":\\([^:]+\\):\\([^:]+\\):\\([^:]+\\):\\([^:]+\\):"
  "Regular expression to match identifier expected from client.
The first group matches the label, the second the major version of Maple,
the third the OS, the fourth the process id of the Maple job.")

(defvar mds-logins nil "Assoc-list of logins, indexed by proc.")

(defun mds-login-add (proc)
  "Add an entry to `mds-logins' for PROC."
  (setq mds-logins
	(cons (list proc 'get-userid "anonymous") mds-logins)))

(defun mds-login-reset ()
  "Clear the list of logins."
  (setq mds-logins nil))

(defun mds-login-delete (proc)
  "Delete the PROC entry in `mds-logins'."
  (setq mds-logins (delq proc mds-logins)))

(defun mds-login-get-status (proc)
  "Return the status of the PROC login."
  (let ((entry (assq proc mds-logins)))
    (and entry (cadr entry))))

(defun mds-login-set-status (proc status)
  "Assign the status of the PROC login to STATUS."
  (let ((entry (assq proc mds-logins)))
    (if entry
	(setcar (cdr entry) status))))

(defun mds-login-get-userid (proc)
  "Return the userid of the PROC login."
  (let ((entry (assq proc mds-logins)))
    (and entry (cddr entry))))

(defun mds-login-set-userid (proc userid)
  "Assign the userid of the PROC login to USERID."
  (let ((entry (assq proc mds-logins)))
    (if entry
	(setcdr (cdr entry) userid))))

(defun mds-login-query-userid (proc)
  "Query Maple (PROC) for the userid."
  (process-send-string proc "userid: "))

(defun mds-login-greet (proc msg)
  "Send a greeting to Maple (PROC).  MSG is supposed to be the user name."
  (process-send-string proc (format "Welcome %s" msg)))

(defun mds-login (proc msg)
  "Process login of PROC.  MSG is its latest response."
  (let ((status (mds-login-get-status proc)))
    (cond
     ((null status)
      ;; Create entry
      (mds-login-add proc)
      (mds-login-query-userid proc))
     ((eq status 'get-userid)
      ;; For now, finish login process
      (if (not (string-match mds-login-id-re msg))
	  ;; id has incorrect format
	  (mds-login-query-userid proc)
	;; id has correct format
	(let ((label (match-string 1 msg)) ; label
	      (ver (match-string 2 msg))   ; maple version (16)
	      (os (match-string 3 msg))    ; unix/window/etc
	      (pid (match-string 4 msg))   ; maple pid
	      (client (cdr (assq proc mds-clients))))
	  (when client
	    (mds-client-set-id client (list label ver os pid))
	    (mds-client-set-status client 'start-debugging)
	    (mds-login-greet proc label)))
	(mds-login-delete proc))))))

(provide 'mds-login)

;;; mds-login.el ends here
