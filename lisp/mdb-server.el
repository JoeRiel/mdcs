(defconst mdb-server-port 10000
  "Port of the mdb server")

(defvar mdb-server-proc nil)

(defconst mdb-server-buffer "*mdb-server*"
  "Buffer associated with mdb server")

(defvar mdb-server-client-id 0)

(defvar mdb-server-clients '() 
  "Alist where KEY is a client process and VALUE is the string")

(defun mdb-server-start ()
  "Start an mdb server; return the process."
  (interactive)
  (unless (process-status "mdb-server")
    (setq mdb-server-clients '()
	  mdb-server-client-id 0
	  mdb-server-proc (make-network-process 
			   :name "mdb-server" 
			   :buffer mdb-server-buffer
			   :family 'ipv4 
			   :service mdb-server-port 
			   :sentinel 'mdb-server-sentinel 
			   :filter 'mdb-server-filter 
			   :server 't))))

(defun mdb-server-stop nil
  "Stop an Emacs mdb server"
  (interactive)
  (while mdb-server-clients
    (delete-process (car (car mdb-server-clients)))
    (setq mdb-server-clients (cdr mdb-server-clients)))
  (if (process-status mdb-server-proc)
      (delete-process mdb-server-proc)))

(defun mdb-server-filter (proc msg)
  "Filter to handle the Maple clients.
PROC identifies the client, MSG is the message."
  (mdb-server-log proc msg))

(defun mdb-server-sentinel (proc msg)
  "Handle change to client status.  PROC is client process,
MSG is the message indicating the changed status."
  ;; Consider adding a validation message.
  ;; Possibly other stuff.
  (cond
   ((eq 't (compare-strings msg 0 10 "open from " 0 10))
    ;; A Maple client has attached.
    ;; Create mdb and showstat buffers, and add to alist
    (mdb-server-add-client proc))
   ((string= msg "connection broken by remote peer\n")
    ;; A Maple client has closed.
    (mdb-server-delete-client proc))
   ((string= msg "deleted\n"))
   (t (error "unexpected sentinel message: %s" msg))))

(defun mdb-server-add-client (proc)
  "Add a Maple client.  Buffers are created and added to the
`mdb-servers-clients' alist."
  (if (assoc proc mdb-server-clients)
      (error "client already exists in list")
    ;; Create and add mdb buffers, add to 
    (setq mdb-server-client-id (1+ mdb-server-client-id)
	  mdb-server-clients (cons (cons proc 
					 (mdb-server-alist-entry proc mdb-server-client-id))
				   mdb-server-clients))
    (mdb-server-log proc "added client")))

(defun mdb-server-delete-client (proc)
  (let ((entry (assoc proc mdb-server-clients)))
    (if (null entry)
	(error "client does not exist")
      (mdb-server-log proc "removed client")
      (setq mdb-server-clients (delq entry mdb-server-clients)))))

(defun mdb-server-alist-entry (proc id)
  "Create an entry in the `mdb-servers-client-alist'."
  id)

(defun mdb-server-log (proc msg)
  (with-current-buffer mdb-server-buffer
    (goto-char (point-max))
    (let ((id (cdr (assoc proc mdb-server-clients))))
      (insert (format "client %d: %s\n" id msg)))
    (set-window-point (get-buffer-window) (point))))
	      
  
(defun mdb-server-send-client (proc msg)
  "Send MSG to Maple client with process PROC."
  (process-send-string proc msg))

(defun mdb-server-send-client-id (id msg)
  "Send MSG to the Maple client with ID."
  (interactive)
  (let ((clients mdb-server-clients)
	client proc)
    (while (and clients (null proc))
      (setq client (car clients))
      (if (eq id (cdr client))
    	  (setq proc (car client))
    	(setq clients (cdr clients))))
    (if proc
    	(mdb-server-send-client proc msg)
      (error "no client with id: %d" id))))

;; Manual Test
;;
;; (load "/home/joe/emacs/mdb/lisp/mdb-server.el")
;; (mdb-server-start)
;; (mdb-server-stop)
;;
;; (mdb-server-send-client-id 2 "showstat")
;; (mdb-server-send-client-id 2 "cont")
;; (mdb-server-send-client-id 2 "showstack")
;; (mdb-server-send-client-id 2 "where")
;; (mdb-server-send-client-id 2 "stopat 10")
;; (mdb-server-send-client-id 2 "cont")
;; (mdb-server-send-client-id 2 "step")
;; (mdb-server-send-client-id 2 "showstat")
;; (mdb-server-send-client-id 2 "cont")


