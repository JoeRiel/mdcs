;;; mds.el

;; Copyright (C) 2011 Joseph S. Riel, all rights reserved

;; Author:     Joseph S. Riel <jriel@maplesoft.com>
;; Created:    Jan 2011
;; Keywords:   maple, debugger 
;;
;;; Commentary:

;; This file contains the source for the Emacs Maple debugger server.
;; It is a part of the Maple Debugger Client Server package.

;;; Code:

;;{{{ License

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc.,  51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;}}}
;;{{{ Commentary

;; This package provides a major mode, mds-showstat, for debugging
;; Maple code.  It uses the Maple debugger to step through interpreted
;; Maple code, which is displayed in a buffer. It is not a true source
;; code debugger, that is, one that allows stepping through the source
;; *file*; however, it is the next best thing.

;;}}}
;;{{{ TODO

;;}}}

;;{{{ Lisp Requirements

(require 'mds-output)
(require 'mds-showstat)
(require 'maplev)
(eval-when-compile
  (require 'hl-line))

;;}}}

;;{{{ Constants

(defconst mds-version "0.1" "Version number of mds.")
(defconst mds-port 10000  "Port used by mds server")
(defconst mds-max-number-clients 4  "Maximum number of clients allowed.")

(defconst mds-log-buffer-name "*mds-log*"  "Name of buffer used to log connections.")

;;{{{ Regular Expressions

(defconst mds--debugger-status-re
  (concat "^\\(" maplev--name-re "\\):\n\\s-*\\([1-9][0-9]*\\)[ *?]")
  "Regexp that matches the status output of the debugger.
The first group matches the procedure name, the second group the
state number.")

(defconst mds-start-tag-re "^<\\([^>]+\\)>"
  "Regular expression that matches start tag.
The tag has format <tag-name>.  Group 0 matches the tag,
group 1 matches tag-name.")

(defconst mds--client-attach-re "^open from \\([^\n]+\\)\n$"
  "Regexp to match message when a client attaches.
The first group identifies SOMETHING.")

(defconst mds-end-of-msg-re "---EOM---")

;;}}}

(defvar mds-proc nil "process for the server.")


;;}}}

;;{{{ Variables

(defvar mds-log-buffer nil
  "Buffer used to record log entries. 
Name given by `mds-log-buffer-name'.")

(defvar mds-pre-Maple-14 nil
  "Boolean flag indicating the Maple client is a release earlier
  than Maple 14.")

(defvar mds-number-clients 0
  "Current number of clients.
Maximum is given by `mds-max-number-clients'.")

;; data structures

(defvar mds-proc-status '()
  "Alist containing status of each known proc.
An entry consists of (proc . status).
Status is either `accepted', `pending', or `rejected'.")

(defvar mds-clients '() 
  "Alist containing info of accepted clients, indexed by the associated process.
See `mds-create-client' for the form of each entry.")

(defvar mds-log-messages 't
  "When non-nil, write all messages to `mds-log-buffer'.")


;;}}}

;;{{{ Client structure and creation/destruction

;; Each entry in the alist, including the key (proc),
;; has the structure
;;
;;    (proc ss-buf id . queue)
;;    (proc queue id live-buf out-buf dead-buf)
;;
;; Access elements of a client (entry in `mds-clients').

(defsubst mds--get-client-proc     (client) (car client))
(defsubst mds--get-client-status   (client) (nth 1 client))
(defsubst mds--get-client-queue    (client) (nth 2 client))
(defsubst mds--get-client-id       (client) (nth 3 client))
(defsubst mds--get-client-live-buf (client) (nth 4 client))
(defsubst mds--get-client-dead-buf (client) (nth 5 client))
(defsubst mds--get-client-out-buf  (client) (nth 6 client))

(defun mds-create-client (proc id)
  "Create a client that is associated with process PROC and has identity ID.
The returned client structure is a list (PROC status queue ID
live-buf dead-buf out-buf), where status is initialized to 'new'."
  (let ((client (list proc)))
    (setcdr client (list
		    'new
		    (mds-queue-create proc)
		    id
		    (mds-showstat-create-buffer client 'live)
		    (mds-showstat-create-buffer client)
		    (mds-output-create-buffer   client)))
    client))
  
(defun mds-destroy-client (client)
  "Destroy a client."
  ;; kill the process and buffers
  (delete-process  (mds--get-client-proc client))
  (mds-kill-buffer (mds--get-client-live-buf client))
  (mds-kill-buffer (mds--get-client-dead-buf client))
  (mds-kill-buffer (mds--get-client-out-buf client)))

(defun mds-find-client-with-id (id)
  "Return entry in `mds-clients' with matching ID.
If none, then return nil."
  (let ((clients mds-clients)
	client match)
    (while (and clients (null match))
      (setq client (car clients))
      (if (eq id (mds--get-client-id client))
    	  (setq match client)
    	(setq clients (cdr clients))))
    match))

(defun mds-swap-proc-in-clients (oldproc newproc)
    (setq mds-clients (cons (cons newproc (cdr (assoc oldproc mds-clients)))
			    (assq-delete-all oldproc mds-clients))))

(defun mds-get-client-status (proc)
  (let ((client (assoc proc mds-clients)))
    (if client (cadr client))))
	
    ;; (if (>= mds-number-clients mds-max-number-clients)
    ;; 	'rejected
    ;;   (mds-add-client (mds-create-client proc "dummy id"))
    ;;   'accepted)))

(defun mds-set-client-status (proc status)
  (let ((client (assoc proc mds-clients)))
    (if client (setcar (cdr client) status))))

;;}}}
;;{{{ Client association list

(defsubst mds-get-client (proc) (assoc proc mds-clients))

(defun mds-delete-client (client)
  "Delete CLIENT from `mds-clients'.  Stop the associated process,
kill the buffers, and decrement `mds-number-clients'."
  (if client
      (let ((proc (mds--get-client-proc client)))
	(mds-writeto-log proc "removing client")
	(mds-destroy-client client)
	;; update `mds-clients' and `mds-number-clients'
	(setq mds-clients (delq client mds-clients)
	      mds-number-clients (1- mds-number-clients)))))

(defun mds-add-client (client)
  (setq mds-clients (cons client mds-clients)
	mds-number-clients (1+ mds-number-clients)))

(defun mds-kill-clients ()
  (while mds-clients
    (let ((client (car mds-clients)))
      (mds-delete-client client))))

;;}}}

;;{{{ Start and stop server

;; (setq mds-clients nil)

(defun mds ()
  "Start an mds server; return the process.
If server is already running, stop then restart it."
  (interactive)
  (setq mds-log-buffer (get-buffer-create mds-log-buffer-name))
  (if (null (process-status "mds"))
      (message "starting Maple Debugger Server")
    (progn
      (mds-stop)
      (message "restarting Maple Debugger Server")))
  (setq mds-clients '()
	mds-proc (make-network-process 
		  :name "mds" 
		  :buffer mds-log-buffer
		  :family 'ipv4 
		  :service mds-port
		  :sentinel 'mds-sentinel 
		  :filter 'mds-filter
		  ;; :log 'mds-log
		  :server 't)))
(defun mds-stop ()
  "Kill all clients, stop the Emacs mds server.
Do not touch `mds-log-buffer'."
  (interactive)
  (mds-kill-clients)
  ;; Kill the server process
  (if (process-status mds-proc)
      (delete-process mds-proc))
  (message "Maple Debugger Server stopped"))

;;}}}
;;{{{ Sentinel

(defun mds-sentinel (proc msg)
  (unless (eq msg "")
    (cond
     ((string-match mds--client-attach-re msg)
      ;; A client has attached.
      (let ((conn (match-string 1 msg)))
	(mds-writeto-log proc (format "client has attached: %s" (substring msg 0 -2)))
	(let ((status (mds-get-client-status proc)))
	  (cond
	   ((eq status 'accepted)
	    (ding)
	    (mds-writeto-log proc "accepted client"))
	   ((null status)
	    ;; not yet registered
	    (mds-add-client (mds-create-client proc "dummy id")))
	   ((eq status 'rejected) 
	    ;; (mds-send-client proc "Sorry, cannot connect at this time.\n")
	    (mds-writeto-log proc "rejected client"))
	   ((eq status 'login) (mds-writeto-log proc "begin login"))
	   ))))
     ((string= msg "connection broken by remote peer\n")
      ;; A client has unattached.
      ;; Delete associated buffers.
      (mds-writeto-log proc
	       (format "%sclient has unattached"
		       (if (mds-delete-client (mds-get-client proc))
			   "accepted " ""))))
     ((string= msg "deleted\n"))
     (t (error "unexpected sentinel message: %s" msg)))))


;;}}}
;;{{{ Filter

(defun mds-filter (proc msg)
  "Dispatch message MSG.  If PROC is an accepted client, send the message to its queue."
  (let ((status (mds-get-client-status proc)))
    (cond
     ((eq status 'accepted)
      (when mds-log-messages
	(mds-writeto-log proc "{{{")
	(mds-writeto-log proc msg)
	(mds-writeto-log proc "}}}"))
      ;; route msg to queue
      (let ((queue (mds--get-client-queue (mds-get-client proc))))
	(mds-queue-filter queue msg)))
     ((eq status 'new)
      (beep)
      (mds-set-client-status proc 'accepted)
      (mds-filter proc msg))
     ((eq status 'rejected)
      (mds-writeto-log proc "ignoring msg from rejected client")))))

;;}}}

;;{{{ Queue

;; A queue structure consists of a single cons cell,
;; ( proc . buffer ), where proc is the process
;; and buffer is the temporary buffer used for the queue.

(defsubst mds-queue-proc   (queue) (car queue))
(defsubst mds-queue-buffer (queue) (cdr queue))

(defun mds-queue-create (proc)
  "Create and return a queue associated with process PROC.
A temporary (hidden) buffer is created and used, not surprisngly,
to buffer the incoming data. The returned queue has the
form (PROC . buf).  PROC identifies the process; the queue
functions do not directly communicate with the process. Data is
sent to the queue via `mds-queue-filter'."
  (let ((buf (get-buffer-create (concat " mds-queue-temp-"
					(process-name proc)))))
    (buffer-disable-undo buf)
    (cons proc buf)))

(defun mds-queue-filter (queue string)
  "Append STRING to QUEUE's buffer; then process the new data."
  (let ((buffer (mds-queue-buffer queue)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
	(goto-char (point-max))
	(insert string)
	(mds-queue-process-buffer queue)))))


(defun mds-queue-process-buffer (queue)
  "Check QUEUE's buffer for the regexp at the head of the queue.
If found, pass it to the function in the queue."
  (let ((buffer (mds-queue-buffer queue)))
    (when (buffer-live-p buffer)
      (set-buffer buffer)
      (unless (= 0 (buffer-size))
	(goto-char (point-min))
	(while (re-search-forward mds-end-of-msg-re nil t)
	  ;; get the complete message, minus the eom,
	  (let ((msg (buffer-substring (point-min) (match-beginning 0))))
	    ;; delete msg, including eom
	    (delete-region (point-min) (point))
	    ;; send msg to correspond showstat filter
	    (let* ((proc (mds-queue-proc queue))
		   (client (mds-get-client proc)))
	      (with-current-buffer (mds--get-client-live-buf client)
		(mds-handle-stream proc msg)))))))))

;;}}}

;;{{{ mds-send-client

(defun mds-send-client (client msg)
  "Send MSG to CLIENT."
  (let ((proc (mds--get-client-proc client)))
    (process-send-string proc msg)))

;;}}}

;;{{{ mds-extract-tag

(defun mds-extract-tag (msg) 
  "Return (tag . msg), where the tags have been been removed from MSG.
The format of MSG must be \"<tag>msg</tag>\", however, the closing tag
is not checked and will likely be removed from the protocol."
  (if (string-match mds-start-tag-re msg)
      (let* ((tag (match-string 1 msg))
	     (len (match-end 0)))
	(cons tag (substring msg len (- (1+ len)))))
    ;; FIXME: this error gets caught and not displayed
    (error "no tag in message: '%s...'" (substring msg 0 (min 10 (length msg))))))

;;}}}

;;{{{ mds-handle-stream

(defun mds-handle-stream (proc msg)
  "Handle tagged message MSG from the client process PROC.
The end of message marker has been removed.  Strip the tags and
use them to route the message."

  (let* ((client (mds-get-client proc))
	 (live-buf (mds--get-client-live-buf client))
	 (dead-buf (mds--get-client-dead-buf client))
	 (out-buf  (mds--get-client-out-buf client))
	 (tag-msg (mds-extract-tag msg))
	 (tag (car tag-msg))  ; name of tag
	 (msg (cdr tag-msg))) ; msg with no tags
    
    ;; route MSG to proper buffer
    ;;    (with-syntax-table maplev--symbol-syntax-table
    (cond
     ;; msg is the state output from debugger.  
     ;; Extract the procname and state number
     ;; and update the showstat buffer
     ((string= tag "DBG_STATE")
      (if (not (string-match mds--debugger-status-re msg))
	  (error "cannot parse current state")
	(mds-showstat-update live-buf 
			     (match-string 1 msg)    ; procname
			     (match-string 2 msg)))) ; state
     ;; msg is showstat output (printout of procedure).
     ;; Display in showstat buffer.
     ((string= tag "DBG_SHOW")
      (mds-showstat-display live-buf msg))
     ;; msg is an inactive showstat output.
     ;; Display in showstat buffer.
     ((string= tag "DBG_SHOW_INACTIVE")
      (mds-showstat-display dead-buf msg))
     
     ((string= tag "DBG_WHERE")
      (mds-output-display out-buf msg 'where))

     ((string= tag "DBG_ARGS")
      (mds-output-display out-buf msg 'args))
     
     ((string= tag "DBG_STACK")
      (mds-output-display out-buf msg 'stack))
     
     ((string= tag "DBG_WARN")
      (mds-output-display out-buf msg 'warn))

     ((string= tag "DBG_ERR2")
      (mds-output-display out-buf msg 'parser-err))

     ((string= tag "MPL_ERR")
      (mds-output-display out-buf msg 'maple-err))
     
     ;; otherwise print to debugger output buffer
     (t (mds-output-display out-buf msg tag)))))

;;}}}

;;{{{ mds-kill-buffer

(defun mds-kill-buffer (buf)
  "Kill buffer BUF; verify that it is a live buffer."
  (and (bufferp buf)
       (buffer-name buf)
       (kill-buffer buf)))

;;}}}

;;{{{ log stuff

(defun mds-writeto-log (proc msg)
  "Write PROC: MSG to log buffer."
  (with-current-buffer mds-log-buffer
    (goto-char (point-max))
    (insert (format "%s: %s\n" (prin1-to-string proc) msg))
    (set-window-point (get-buffer-window) (point))))

;;}}}

(provide 'mds)

;;{{{ Manual Tests

;;
;; (load "mds-showstat.el")
;; (load "mds.el")
;; (mds-start)
;; (mds-stop)

;;}}}

;;; mds.el ends here

