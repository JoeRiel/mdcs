;;; mds-queue.el --- Queue for Maple Debugger Server

;; Copyright (C) 2012 Joseph S. Riel, all rights reserved

;; Author:     Joseph S. Riel <jriel@maplesoft.com>
;; Created:    July 2012
;; Keywords:   maple, debugger, queue
;;
;;; Commentary:

;; This file contains source for the Emacs Maple debugger server.
;; It is a part of the Maple Debugger Client Server package.
;;
;; Each connection has an associated queue-buffer, which is used to
;; process the responses from the client.  When a message arrives, it
;; is appended to the queue-buffer.  The header of the message is
;; read.  The header includes a tag that identifies the message type
;; and a length field that specifies its length.  When an entire
;; message is present, it is removed and dispatched by
;; `mds-queue-dispatch-tags'.

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

;;; Code:

(eval-when-compile
  (require 'mds-client)
  (require 'mds-re)
  (require 'mds-ss)
  (require 'mds-li)
  (require 'mds-out)
  (require 'mds-wm))


;; A queue structure consists of a single cons-cell,
;; ( proc . buffer ), where proc is the process
;; and buffer is the queue-buffer.

(defsubst mds-queue-proc   (queue) "Return the process component of QUEUE." (car queue))
(defsubst mds-queue-buffer (queue) "Return the buffer component of QUEUE." (cdr queue))

;;{{{ mds-queue-create

(defun mds-queue-create (proc)
  "Create and return a queue associated with process PROC.
A temporary (hidden) buffer is created and used to buffer the
incoming data.  The returned queue has the form (PROC . buf).
PROC identifies the process; the queue functions do not directly
communicate with the process.  Data is sent to the queue via
`mds-queue-filter'."

  ;; FIXME: is this name unique?
  (let ((buf (get-buffer-create (concat " mds-queue-temp-"
					(process-name proc)))))
    (buffer-disable-undo buf)
    (cons proc buf)))

;;}}}
;;{{{ mds-queue-filter

(defun mds-queue-filter (queue string)
  "Append to QUEUE's buffer the string STRING, then process the new data."
  (let ((buffer (mds-queue-buffer queue)))
    (when (buffer-live-p buffer)
      ;; Use set-buffer rather than with-current-buffer so that buffer
      ;; can be properly changed.
      (set-buffer buffer)
      (goto-char (point-max))
      (insert string)
      (while (with-current-buffer buffer
	       (when (< 2 (point-max))
		 ;; Assign tag, which identifies the message type,
		 ;; and beg, the beginning of the message.
		 (let ((tag (char-after 1))
		       (beg (- (char-after 2) ?0 -3))
		       end)
		   (when (<= beg (point-max))
		     ;; When the length field is accessible, read and
		     ;; parse it and use it to compute the end of the
		     ;; message.
		     (setq end (+ beg (string-to-number (buffer-substring 3 beg))))
		     (when (<= end (point-max))
		       ;; When the end of the message is accessible,
		       ;; read it, delete the tag and message, and
		       ;; pass it to `mds-queue-dispatch-tags'.
		       ;; Loop to check for more.
		       (let ((msg (buffer-substring beg end)))
			 (delete-region (point-min) end)
			 (mds-queue-dispatch-tags (mds-queue-proc queue) tag msg))
		       t)))))))))

;;}}}
;;{{{ define tags

(eval-and-compile
  (defconst mds-tag-clear-echo	?C  "Clear message displayed at bottom of screen")
  (defconst mds-tag-error	?E  "Message is an error")
  (defconst mds-tag-eval	?O  "Message is the output from a computation" )
  (defconst mds-tag-info	?I  "Message is debugger query information")
  (defconst mds-tag-monitor	?M  "Message is the output of a monitored expression")
  (defconst mds-tag-printf	?P)
  (defconst mds-tag-prompt	?>  "Display a prompt.  Message is empty.")
  (defconst mds-tag-result	?R  "Message is the result of a call to `mds-ss-request'.")
  (defconst mds-tag-same	?%  "Indicates the state and procedure have not changed.")
  (defconst mds-tag-ss-dead	?D  "Message is a showstat output for the dead buffer." )
  (defconst mds-tag-ss-live	?L  "Message is a showstat output for the live buffer." )
  (defconst mds-tag-stack	?K  "Stack output")
  (defconst mds-tag-state	?S  "Message defines current state")
  (defconst mds-tag-warn	?W  "Message is a warning")
  (defconst mds-tag-watched	?w  "Message is watched errors, conditiona"))

;;}}}
;;{{{ mds-queue-dispatch-tags

(defun mds-queue-dispatch-tags (proc tag msg)
  "Handle message MSG from process PROC with TAG, which indicates the purpose of the message."

  (let* ((client (cdr (assq proc mds-clients)))
	 (live-buf (mds-client-live-buf client))
	 (out-buf  (mds-client-out-buf client)))
    
    ;; route MSG to proper buffer
    (cond
     
     ((= tag (eval-when-compile mds-tag-eval))
      (mds-out-display out-buf msg 'output))


     ((= tag (eval-when-compile mds-tag-state))
      (unless (string-match mds--line-info-re msg)
	(error "Problem with format in LINE_INFO tag"))
      (let ((file (match-string 1 msg))
	    (beg  (1+ (string-to-number (match-string 3 msg))))
	    (breakpoints (match-string 5 msg))
	    (addr      	 (match-string 6 msg))
	    (procname  	 (match-string 7 msg))
	    (state     	 (match-string 8 msg))
	    (statement 	 (match-string 9 msg))
	    (li-buf (mds-client-li-buf client)))
	(if (string= file "0")
	    (mds-client-set-has-source client nil)
	  (mds-li-update li-buf file addr procname state beg
			 (mapcar 'string-to-number (split-string breakpoints)))
	  (mds-client-set-has-source client t))

	(mds-client-set-addr client addr)
	(mds-client-set-procname client procname)
	(mds-client-set-state client state)
	(mds-client-set-statement client statement)

	;; This is needed with do loops missing a 'for'; because of
	;; the bug in the kernel, they have to switch to the showstat
	;; buffer.  Maybe there is a cheaper method.
	(mds-goto-current-state client)

	(let ((trace-mode (mds-client-get-trace client)))
	  (when trace-mode
	    (mds-out-display out-buf trace-mode 'cmd)
	    (mds-client-send client (concat trace-mode "\n"))))
	))

     ((= tag (eval-when-compile mds-tag-prompt))
      ;; Extract the state-number and pass it along
      (mds-out-display out-buf
		       (buffer-local-value 'mds-ss-state live-buf)
		       'prompt)

      (mds-client-set-allow-input client 'unblock)
      (mds-goto-current-state client))
      
     ((= tag (eval-when-compile mds-tag-result))
      (mds-client-set-result client msg))

     ((= tag (eval-when-compile mds-tag-ss-live))
     ;; msg is showstat output (printout of procedure).
     ;; Insert into live-showstat buffer.
      (mds-ss-insert-proc live-buf msg))

     ((= tag (eval-when-compile mds-tag-ss-dead))
     ;; msg is an inactive showstat output.
     ;; Insert into dead-showstat buffer.
      (mds-ss-insert-proc (mds-client-dead-buf client) msg))

     ((= tag (eval-when-compile mds-tag-stack))
      (mds-out-display out-buf msg 'stack))

     ((= tag (eval-when-compile mds-tag-monitor))
      (mds-out-display out-buf msg 'monitor)

      ;; (mds-out-display out-buf (mds-client-get-state client) 'prompt)
      ;; (mds-goto-current-state client)
      ;; (mds-client-set-allow-input client 'unblock)

      )
     
     ((= tag (eval-when-compile mds-tag-warn))
      (mds-out-display out-buf msg 'warn))

     ((= tag (eval-when-compile mds-tag-error))
      (mds-out-display out-buf msg 'error))

     ((= tag (eval-when-compile mds-tag-printf))
      (mds-out-display out-buf msg 'printf))

     ((= tag (eval-when-compile mds-tag-info))
      (mds-out-display out-buf msg 'info))

     ((= tag (eval-when-compile mds-tag-watched))
      (mds-out-display out-buf msg 'watched))

     ((= tag (eval-when-compile mds-tag-clear-echo))
      (message ""))

     ;; otherwise print to debugger output buffer
     (t (mds-out-display out-buf msg tag)))

    ))

;;}}}


(provide 'mds-queue)

;;; mds-queue.el ends here
