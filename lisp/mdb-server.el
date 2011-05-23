(defconst mdb-server-port 10000
  "Port of the mdb server")

(defvar mdb-server-clients '() 
  "Alist where KEY is a client process and VALUE is the string")

(defun mdb-server-start nil
  "Starts an emacs mdb server"
  (interactive)
  (unless (process-status "mdb-server")
    (make-network-process 
     :name "mdb-server" 
     :buffer "*mdb-server*" 
     :family 'ipv4 
     :service mdb-server-port 
     :sentinel 'mdb-server-sentinel 
     :filter 'mdb-server-filter 
     :server 't) 
    (setq mdb-server-clients '())
    ))

(defun mdb-server-stop nil
  "Stop an emacs mdb server"
  (interactive)
  (while  mdb-server-clients
    (delete-process (car (car mdb-server-clients)))
    (setq mdb-server-clients (cdr mdb-server-clients)))
  (delete-process "mdb-server"))

(defun mdb-server-filter (proc string)
  (let ((pending (assoc proc mdb-server-clients))
        message
        index)
    ;;create entry if required
    (unless pending
      (setq mdb-server-clients (cons (cons proc "") mdb-server-clients))
      (setq pending (assoc proc mdb-server-clients)))
    (setq message (concat (cdr pending) string))
    (while (setq index (string-match "\n" message))
      (setq index (1+ index))
      (let ((msg (substring-no-properties message 0 index)))
	(process-send-string proc msg)
	(mdb-server-log msg proc))
      (setq message (substring-no-properties message index)))
    (setcdr pending message)))

(defun mdb-server-sentinel (proc msg)
  "Handle change to client status.  PROC is client process,
MSG is the message indicating the changed status."
  (cond
   ((eq 't (compare-strings msg 0 10 "open from " 0 10))
    ;; A Maple client has attached.
    ;; Create a showstat buffer
    )
   ((eq 't (compare-strings msg 0 32 "connection broken by remote peer" 0 32))
    ;; A Maple client has closed.
    (setq mdb-servers-clients (assq-delete-all proc mdb-server-clients))
    (mdb-server-log (format "client %s has quit" proc))))))
   
(defun mdb-server-log (string &optional client)
  "If an *mdb-server* buffer exists, write STRING to it for logging purposes."
  (if (get-buffer "*mdb-server*")
      (with-current-buffer "*mdb-server*"
	(goto-char (point-max))
	(insert (current-time-string)
		(if client (format " %s:" client) " ")
		string)
	(or (bolp) (newline)))))


;; (load "/home/joe/emacs/mdb/lisp/mdb-server.el")
;; (mdb-server-start)
;; (mdb-server-stop)

