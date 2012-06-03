;;; mds.el --- Maple Debugger Server
;;; mds.el

;; Copyright (C) 2011 Joseph S. Riel, all rights reserved

;; Author:     Joseph S. Riel <jriel@maplesoft.com>
;; Created:    Jan 2011
;; Keywords:   maple, debugger
;;
;;; Commentary:

;; This file contains the source for the Emacs Maple debugger server.
;; It is a part of the Maple Debugger Client Server package.

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


;;}}}
;;{{{ TODO

;;}}}

;;; Code:

;;{{{ Lisp Requirements

(require 'maplev)
(require 'mds-client)
(require 'mds-login)
(require 'mds-out)
(require 'mds-patch)
(require 'mds-re)
(require 'mds-ss)
(require 'mds-thing)
(require 'mds-wm)
(require 'mds-li)

;;}}}

;;{{{ customizations

(defgroup mds nil
  "Maple Debugger Server."
  :group 'tools)

(defcustom mds-port 10000  "Port used by mds server."
  :type 'integer
  :group 'mds)

(defcustom mds-get-focus-function
    (and (= 0 (shell-command "which wmctrl"))
	 #'mds-wm-get-focus-wmctrl)
  "Function called to give Emacs the focus when starting debugging.
The default works on a linux system with wmctrl installed.
Automatically assigned to nil if wmctrl is not available."
  :type 'function
  :group 'mds)

(defcustom mds-show-args-flag t
  "Non-nil means display args on entry to procedure."
  :type 'boolean
  :group 'mds)

(defcustom mds-track-input-flag t
  "Non-nil means track (echo) the input line to the output buffer after each command."
  :type 'boolean
  :group 'mds)

(defcustom mds-stop-trace-at-error-flag t
  "Non-nil means stop tracing when an error occurs."
  :type 'boolean
  :group 'mds)

(defcustom mds-log-messages-flag nil
  "Non-nil means write all messages to `mds-log-buffer'."
  :type 'boolean
  :group 'mds)

;;}}}

;;{{{ Constants

(defconst mds-version "1.13.6" "Version number of mds.")
(defconst mds-max-number-clients 4  "Maximum number of clients allowed.")
(defconst mds-log-buffer-name "*mds-log*"  "Name of buffer used to log connections.")

;;}}}

;;{{{ Variables

(defvar mds-log-buffer nil
  "Buffer used to record log entries.
Name given by `mds-log-buffer-name'.")

(defvar mds-proc nil "Process for the Maple debugger server.")

(defvar mds-keep-dead-clients nil
  "*Non-nil means keep dead clients.
Useful for inspecting the output buffer of a traced procedure that crashes.")

;;}}}

;;{{{ Start and stop server

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
  (or (framep mds-frame)
      ;; pick the first frame in the current configuration.
      ;; not sure this is a good idea
      (setq mds-frame (car (nth 1 (current-frame-configuration)))))
  (setq mds-clients '()
	mds-proc (make-network-process
		  :name "mds"
		  :buffer mds-log-buffer
		  :family 'ipv4
		  :service mds-port
		  :sentinel 'mds-sentinel
		  :filter 'mds-filter
		  ;; :log 'mds-log
		  :server 't))
  (mds-login-reset))
(defun mds-stop ()
  "Kill all clients, stop the Emacs mds server.
Do not touch `mds-log-buffer'."
  (interactive)
  (mds-clients-kill)
  ;; Kill the server process
  (if (process-status mds-proc)
      (delete-process mds-proc))
  (message "Maple Debugger Server stopped"))

;;}}}
;;{{{ Sentinel

(defun mds-sentinel (proc msg)
  "Monitor the client processes and handle any changes."
  (unless (eq msg "")
    (cond
     ((string-match mds--client-attach-re msg)
      ;; A client has attached.
      (let ((conn (match-string 1 msg)))
	(mds-writeto-log proc (format "client has attached: %s" (substring msg 0 -2)))
	(let* ((client (cdr (assq proc mds-clients)))
	       (status (and client (mds-client-status client))))
	  (cond
	   ((eq status 'accepted)
	    ;; Accepted!
	    (ding)
	    (mds-writeto-log proc "accepted client"))

	   ((null status)
	    ;; not yet registered
	    (mds-client-add (mds-client-create proc '("anonymous" "unknown" "unknown")))
	    (mds-login proc msg))

	   ((eq status 'rejected)
	    ;; Rejected
	    (mds-writeto-log proc "rejected client"))

	   ((eq status 'login)
	    ;; Continue the login process
	    (mds-login proc msg))
	   ))))
     ((string= msg "connection broken by remote peer\n")
      ;; A client has unattached.
      ;; Delete associated buffers.
      (mds-writeto-log proc
	       (format "%sclient has unattached"
		       (if (and (not mds-keep-dead-clients)
				     (mds-client-delete (cdr (assq proc mds-clients))))
			   "accepted " "")))
      (mds-wm-group-update mds-clients))
     ((string= msg "deleted\n"))
     (t (error "Unexpected sentinel message: %s" msg)))))


(defun mds-start-debugging (proc msg)
  "Called when debugging first starts.
PROC is input process from the client; MSG is the initial output
of the debug Maple kernel.  Set the status of the client to
'accepted, pass the message along for handling by the filter,
display the client windows, and get the focus."
  (ding)
  (let ((client (cdr (assq proc mds-clients))))
    (mds-client-set-status client 'accepted)
    (mds-filter proc msg)
    ;; update groups
    (mds-wm-group-update mds-clients)
    (mds-wm-display-client client)
    ;; switch focus
    (if (functionp mds-get-focus-function)
      (funcall mds-get-focus-function))))

;;}}}
;;{{{ Filter

(defun mds-filter (proc msg)
  "Dispatch message MSG.  If PROC is an accepted client, send the message to its queue."
  (let* ((client (cdr (assq proc mds-clients)))
	 (status (and client (mds-client-status client))))
    (cond
     ((eq status 'accepted)
      (when mds-log-messages-flag
	(mds-writeto-log proc "{{{")
	(mds-writeto-log proc msg)
	(mds-writeto-log proc "}}}"))
      ;; route msg to queue
      (mds-queue-filter (mds-client-queue client) msg))
     ((eq status 'login)
      ;; initiate the login process.
      (ding)
      (mds-login proc msg))
     
     ((eq status 'start-debugging)
      (mds-start-debugging proc msg))

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
to buffer the incoming data.  The returned queue has the
form (PROC . buf).  PROC identifies the process; the queue
functions do not directly communicate with the process.  Data is
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
	    (let ((proc (mds-queue-proc queue)))
	      (with-current-buffer (mds-client-live-buf (cdr (assq proc mds-clients)))
		(mds-handle-stream proc msg)))))))))

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
    (error "No tag in message: '%s...'" (substring msg 0 (min 10 (length msg))))))

;;}}}

;;{{{ mds-handle-stream

(defun mds-handle-stream (proc msg)
  "Handle tagged message MSG from the client process PROC.
The end of message marker has been removed.  Strip the tags and
use them to route the message."

  (let* ((client (cdr (assq proc mds-clients)))
	 (live-buf (mds-client-live-buf client))
	 (dead-buf (mds-client-dead-buf client))
	 (out-buf  (mds-client-out-buf client))
	 (tag-msg (mds-extract-tag msg))
	 (tag (car tag-msg))  ; name of tag
	 (msg (cdr tag-msg))) ; msg with no tags
    
    ;; route MSG to proper buffer
    ;;    (with-syntax-table maplev--symbol-syntax-table
    (cond
     ((string= tag "DBG_PROMPT")
      ;; Extract the state-number and pass it along
      (mds-out-display out-buf
			  (buffer-local-value 'mds-ss-state live-buf)
			  'prompt)
      (mds-client-set-allow-input client t)
      (mds-wm-select-code-window client))
     
     ((string= tag "DBG_STATE")
     ;; msg is the state output from debugger.
     ;; Extract the procname and state number
     ;; and update the showstat buffer
      (if (not (string-match mds--debugger-status-re msg))
	  (error "Cannot parse current state")
	(let ((addr      (match-string 1 msg))
	      (procname  (match-string 2 msg))
	      (state     (match-string 3 msg))
	      (statement (match-string 4 msg)))
	  (mds-ss-update live-buf addr procname state statement))))

     ((string= tag "DBG_SAME_STATE")
	(mds-goto-current-state)
	(mds-wm-select-code-window client))

     ((string= tag "LINE_INFO")
      (unless (string-match mds--line-info-re msg)
	(error "Problem with format in LINE_INFO tag"))
      (let ((file (match-string 1 msg))
	    ;;(line (string-to-number (match-string 2 msg)))
	    (beg  (1+ (string-to-number (match-string 3 msg))))
	    (addr      (match-string 5 msg))
	    (procname  (match-string 6 msg))
	    (state     (match-string 7 msg))
	    (statement (match-string 8 msg))
	    (li-buf (mds-client-li-buf client)))
	(mds-ss-update live-buf addr procname state statement)
	(mds-li-update li-buf file beg)
	(mds-client-set-display-source client t)))

     ((string= tag "DBG_SHOW")
     ;; msg is showstat output (printout of procedure).
     ;; Display in showstat buffer.
      (mds-ss-display live-buf msg))

     ((string= tag "DBG_SHOW_INACTIVE")
     ;; msg is an inactive showstat output.
     ;; Display in showstat buffer.
      (mds-ss-display dead-buf msg))

     ((string= tag "DBG_EVAL")
      (mds-out-display out-buf msg 'output))
      
     ((string= tag "DBG_WHERE")
      (mds-out-display out-buf msg 'where))

     ((string= tag "DBG_ARGS")
      (mds-out-display out-buf msg 'args))

     ((string= tag "MONITOR")
      (mds-out-display out-buf msg 'monitor))
     
     ((string= tag "DBG_STACK")
      (mds-out-display out-buf msg 'stack))
     
     ((string= tag "DBG_WARN")
      (mds-out-display out-buf msg 'warn))

     ((string= tag "DBG_PARSE_ERR")
      (mds-out-display out-buf msg 'parse-err))

     ((string= tag "DBG_ERR")
      (mds-out-display out-buf msg 'maple-err))

     ((string= tag "MPL_ERR")
      (mds-out-display out-buf msg 'maple-err))

     ((string= tag "MDC_PRINTF")
      (mds-out-display out-buf msg 'printf))

     ((string= tag "MDC_RESPONSE")
      (mds-client-assign-result client msg))

     ((string= tag "DBG_ERROR")
      (mds-out-display out-buf msg 'maple-err))

     ((string= tag "SHOW_EXCEPTION")
      (mds-out-display out-buf msg 'maple-err))

     ((string= tag "DBG_INFO")
      (mds-out-display out-buf msg 'debug-info))

     ((string= tag "WATCHED_CONDS")
      (mds-out-display out-buf msg 'watch-conds))

     ((string= tag "WATCHED_ERRS")
      (mds-out-display out-buf msg 'watch-errs))

     ((string= tag "DBG_STOP")
      (mds-out-display out-buf msg 'stop))

     ((string= tag "CLEAR_ECHO")
      (message ""))

     ((string= tag "DBG_NULL"))

     ;; otherwise print to debugger output buffer
     (t (mds-out-display out-buf msg tag)))))

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
    (insert (format "%s: %s\n" proc msg))
    (set-window-point (get-buffer-window (current-buffer)) (point))))

;;}}}

;;{{{ miscellaneous

(defun mds-toggle-show-args ()
  "Toggle the variable `mds-show-args-flag'.
When true, the arguments of a procedure are automatically printed
when entering a procedure."
  (interactive)
  (message "display args: %s"
	   (if (setq mds-show-args-flag (not mds-show-args-flag))
	       "enabled"
	     "disabled")))

(defun mds-toggle-track-input ()
  "Toggle the variable `mds-track-input-flag'.
When true, executed input statements from the showstat buffer
are printed to the output buffer."
  (interactive)
  (message "track input: %s"
	   (if (setq mds-track-input-flag (not mds-track-input-flag))
	       "enabled"
	     "disabled")))


(defun mds-toggle-stop-trace-at-error ()
  "Toggle the variable `mds-stop-trace-at-error-flag'.
When true, tracing stops if an error is raised."
  (interactive)
  (message "stop tracing at error: %s"
	   (if (setq mds-track-input-flag (not mds-stop-trace-at-error-flag))
	       "enabled"
	     "disabled")))

(defun mds-version ()
  "Display the version of mds."
  (interactive)
  (message "mds version: %s" mds-version))

;;}}}

(provide 'mds)

;;; mds.el ends here

