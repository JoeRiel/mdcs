;;; mds.el --- Maple Debugger Server

;; Copyright (C) 2011 Joseph S. Riel, all rights reserved

;; Author:     Joseph S. Riel <jriel@maplesoft.com>
;; Created:    Jan 2011
;; Keywords:   maple, debugger

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

;;; Commentary:

;; This file contains source for the Emacs Maple debugger server.
;; It is a part of the Maple Debugger Client Server package.

;;}}}
;;{{{ TODO

;;}}}

;;; Code:

;;{{{ Lisp Requirements

(require 'maplev)
(require 'mds-client)  ; client structure
(require 'mds-custom)  ; variables to customize
(require 'mds-login)   ; 
(require 'mds-out)     ; output buffer
(require 'mds-patch)   ; live patching
(require 'mds-queue)   ; handle the queue and dispatch tags
(require 'mds-re)      ; regular expressions
(require 'mds-ss)      ; showstat buffers
(require 'mds-thing)
(require 'mds-wm)      ; window manager
(require 'mds-li)      ; lineinfo buffer

;;}}}

;;{{{ Customization

;; This is here rather than mds-custom to avoid
;; some unpleasant output during byte-compilation.

(defcustom mds-get-focus-function
    (and (= 0 (shell-command "which wmctrl"))
	 #'mds-wm-get-focus-wmctrl)
  "Function called to give Emacs the focus when starting debugging.
The default works on a Linux system with wmctrl installed.
Automatically assigned to nil if wmctrl is not available."
  :type 'function
  :group 'mds)

;;}}}

;;{{{ Constants

(defconst mds-version "2.4.3" "Version number of mds.")
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
		  ;; :buffer mds-log-buffer
		  :family 'ipv4
		  :service mds-port
		  :sentinel 'mds-sentinel
		  :filter 'mds-filter
		  ;; :log 'mds-log
		  :server 't))
  (set-process-query-on-exit-flag mds-proc mds-query-on-exit-flag)
  (mds-login-reset))

(defun mds-stop ()
  "Kill all clients, stop the Emacs mds server.
Do not touch `mds-log-buffer'."
  (interactive)
  (mds-clients-kill)
  ;; Kill the server process
  (if (process-status mds-proc)
      (delete-process mds-proc))
  ;; Clear log buffer
  (if (buffer-live-p mds-log-buffer)
      (with-current-buffer mds-log-buffer
	(delete-region (point-min) (point-max))))
  (message "Maple Debugger Server stopped"))

;;}}}
;;{{{ Sentinel

(defun mds-sentinel (proc msg)
  "Monitor the client processes and handle any change.
PROC is a client process, MSG is the message from the client."
  (unless (eq msg "")
    (cond
     ((string-match mds-re-client-attach msg)
      ;; A client has attached.
      (let ((conn (match-string 1 msg)))
	(mds-writeto-log-proc proc (format "client has attached: %s" (substring msg 0 -2)))
	(let* ((client (cdr (assq proc mds-clients)))
	       (status (and client (mds-client-status client))))
	  (cond
	   ((eq status 'accepted)
	    ;; Accepted!
	    (ding)
	    (mds-writeto-log-proc proc "accepted client"))

	   ((null status)
	    ;; not yet registered
	    (mds-client-add (mds-client-create proc '("anonymous" "unknown" "unknown")))
	    (mds-login proc msg))

	   ((eq status 'rejected)
	    ;; Rejected
	    (mds-writeto-log-proc proc "rejected client"))

	   ((eq status 'login)
	    ;; Continue the login process
	    (mds-login proc msg))
	   ))))
     ((string= msg "connection broken by remote peer\n")
      ;; A client has unattached.
      ;; Delete associated buffers.
      (mds-writeto-log-proc proc
	       (format "%sclient has unattached"
		       (if (and (not mds-keep-dead-clients)
				     (mds-client-delete (cdr (assq proc mds-clients))))
			   "accepted " "")))
      (mds-wm-group-update mds-clients))
     ((string= msg "deleted\n"))
     (t (error "Unexpected sentinel message: %s" msg)))))


(defun mds-start-debugging (proc msg)
  "Start debugging a Maple client.
PROC is input process from the client, MSG is the initial output
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
  "If PROC is an accepted client, send MSG to its queue."
  (let* ((client (cdr (assq proc mds-clients)))
	 (status (and client (mds-client-status client))))
    (cond
     ((eq status 'accepted)
      (when mds-log-messages-flag
	(mds-writeto-log-proc proc "{{{")
	(mds-writeto-log-proc proc msg)
	(mds-writeto-log-proc proc "}}}"))
      ;; route msg to queue
      (mds-queue-filter (mds-client-queue client) msg))
     ((eq status 'login)
      ;; initiate the login process.
      (ding)
      (mds-login proc msg))
     
     ((eq status 'start-debugging)
      (mds-start-debugging proc msg))

     ((eq status 'rejected)
      (mds-writeto-log-proc proc "ignoring msg from rejected client")))))

;;}}}

;;{{{ mds-kill-buffer

(defun mds-kill-buffer (buf)
  "Kill buffer BUF; verify that it is a live buffer."
  (and (bufferp buf)
       (buffer-name buf)
       (kill-buffer buf)))

;;}}}

;;{{{ log stuff

(defun mds-writeto-log-proc (proc msg)
  "Write PROC: MSG to log buffer."
  (mds-writeto-log (format "%s\n" msg)))

(defun mds-writeto-log (msg)
"Insert MSG into the log buffer."
  (with-current-buffer mds-log-buffer
    (goto-char (point-max))
    (insert msg)))

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
are printed to the output buffer.  Return the new value."
  (interactive)
  (message "track input: %s"
	   (if (setq mds-track-input-flag (not mds-track-input-flag))
	       "enabled"
	     "disabled"))
  mds-track-input-flag)


(defun mds-toggle-stop-trace-at-trapped-error ()
  "Toggle the variable `mds-stop-trace-at-trapped-error-flag'.
When true, tracing stops if an error is raised."
  (interactive)
  (message "stop tracing at error: %s"
	   (if (setq mds-stop-trace-at-trapped-error-flag (not mds-stop-trace-at-trapped-error-flag))
	       "enabled"
	     "disabled")))

(defun mds-version ()
  "Display the version of mds."
  (interactive)
  (message "mds version: %s" mds-version))

;;}}}

(provide 'mds)

;;; mds.el ends here

