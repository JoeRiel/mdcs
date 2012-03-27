;; Attempt at providing some login stuff.
;; We'll see if it sticks...

(require 'sha1)

(eval-when-compile
  (require 'mds-client))
(declare-function mds-client-set-id "mds")
(declare-function mds-client-set-status "mds")


(defconst mds-login-id-re ":\\([^:]+\\):\\([^:]+\\):\\([^:]+\\):\\([^:]+\\):"
  "Regular expression to match identifier expected from client.
The first group matches the label, the second the major version of Maple,
the third the OS, the fourth the process id of the Maple job.")

(defvar mds-logins nil)

(defun mds-login-add (proc)
  (setq mds-logins
	(cons (list proc 'get-userid "anonymous") mds-logins)))

(defun mds-login-delete (proc)
  (setq mds-logins (delq proc mds-logins)))

(defun mds-login-get-status (proc)
  (let ((entry (assq proc mds-logins)))
    (and entry (cadr entry))))

(defun mds-login-set-status (proc status)
  (let ((entry (assq proc mds-logins)))
    (if entry
	(setcar (cdr entry) status))))

(defun mds-login-get-userid (proc)
  (let ((entry (assq proc mds-logins)))
    (and entry (cddr entry))))

(defun mds-login-set-userid (proc userid)
  (let ((entry (assq proc mds-logins)))
    (if entry
	(setcdr (cdr entry) userid))))

(defun mds-login-query-userid (proc)
  (process-send-string proc "userid: "))

(defun mds-login-greet (proc msg)
  (process-send-string proc (format "Welcome %s" msg)))

(defun mds-login (proc msg)
  "Handle login message MSG from process PROC."
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
	      (client (assq proc mds-clients)))
	  (when client
	    (mds-client-set-id client (list label ver os pid))
	    (mds-client-set-status client 'start-debugging)
	    (mds-login-greet proc label)))
	(mds-login-delete proc))))))


(defvar mds-login-passkey-alist nil
  "Alist with each entry (userid . passkey).
Both userid and passkey are strings.  Passkey
is the sha1 transformation of the actual password.")

(defun mds-login-get-passkey (userid)
  (let ((passkey (assoc userid mds-login-passkey-alist)))
    (if passkey (cdr passkey)
      (error "No passkey for user %s" userid))))
  

(defun mds-login-generate-challenge (userid)
  "Generate a challenge, which consists of a
cons-cell, (random-string . encrypted-random-string).  Each
string is 40 characters long."
  (let ((rand (sha1 (format "%d" (random t)))))
    (cons rand (sha1 (concat rand (mds-login-get-passkey userid))))))

;; (defun mds-login-verify-response (userid challenge response)
;;   "Verify the RESPONSE from USERID to a CHALLENGE.  Return non-nil
;; if the challeng is correct, nil otherwise."
;;   (let ((passkey (mds-login-get-passkey userid)))
;;     (string= response (sha1 (concat (mds-login-get-passkey userid) (car rand))))))

(provide 'mds-login)
