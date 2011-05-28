;;; mds.el
;;; -*- mode: emacs-lisp -*-

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

(require 'mds-showstat)
(require 'maplev)
(eval-when-compile
  (require 'hl-line))

;;}}}

;;{{{ customization

(defgroup mds nil
  "Major mode for debugging Maple."
  :group 'tools)

(defcustom mds-maple-cmd "emaple"
  "*Shell command to launch command-line maple.
The default, emaple, is a customizable script that calls a
binary, pmaple.  It does not use cmaple, which does not properly
handle prompts in a pipe."
  :type 'string
  :group 'mds)

(defcustom mds-maple-setup-switches  nil
  "*List of command-line switches passed to `mds-maple-cmd'."
  :type '(repeat string)
  :group 'mds)

(defcustom mds-pre-Maple-14 nil
  "*Boolean flag.  Set to non-nil if Maple is a release earlier than Maple 14."
  :type 'boolean
  :group 'mds)

;;{{{   prompts and cursors

(defcustom mds-prompt "(**) "
  "*eMaple prompt.
Changing this, alas, does not currently change the prompt because the
prompt is defined as a C-preprocessor-macro in the emaple source."
  :type 'string
  :group 'mds)

(defcustom mds-debug-prompt "(*DBG*) "
  "*eMaple debug prompt.
Changing this, alas, does not currently change the prompt because the
prompt is defined as a C-preprocessor-macro in the emaple source."
  :type 'string
  :group 'mds)

(defcustom mds-cursor-waiting 'hollow
  "Cursor used in showstat buffer when waiting for Maple to respond."
  :type 'symbol
  :group 'mds)

(defcustom mds-cursor-ready 'box
  "Cursor used in showstat buffer when ready for a user input."
  :type 'symbol
  :group 'mds)

;;}}}

(defcustom mds-history-size 50
  "Number of inputs the input-ring can hold."
  :type 'integer ; ensure positive
  :group 'mds)

(defcustom mds-debugger-break (format "\n%s\n" (make-string 40 ?-))
  "String inserted into `mds-debugger-output-buffer' when debugging starts."
  :type 'string
  :group 'mds)

;;{{{   faces

(defgroup mds-faces nil
  "Faces for mds and related modes."
  :group 'mds)

(defface mds-face-arg
  '((((class color) (background dark)) (:foreground "magenta")))
  "Face for arguments in a showstat buffer."
  :group 'mds-faces)

(defface mds-face-prompt
  '((((class color) (background dark)) (:foreground "Green")))
  "Face for the prompt in an mds buffer."
  :group 'mds-faces)

(defface mds-face-procname-entered
  '((((class color) (background dark)) (:foreground "Cyan")))
  "Face for the procname at entry in a debugger output buffer."
  :group 'mds-faces)

(defface mds-face-procname-cont
  '((((class color) (background dark)) (:foreground "LightBlue")))
  "Face for the procname when continued in a debugger output buffer."
  :group 'mds-faces)

;;}}}


;;}}}

;;{{{ Constants

(defconst mds-version "1.0" "Version number the mds.")

(defconst mds--prompt-re (format "^\\(?:\\(%s\\)\\|%s\\)"
				 (regexp-quote mds-debug-prompt)
				 (regexp-quote mds-prompt))
  "Regexp matching Maple prompt.  If the first group matches,
then this is a debug-prompt.")

(defconst mds--prompt-with-cr-re (concat mds--prompt-re "$")
  "Regexp matching Maple prompt with preceding carriage return.
This is the prompt as output from the maple process.")

(defconst mds--debugger-status-re
  (concat "^\\(" maplev--name-re "\\):\n\\s-*\\([1-9][0-9]*\\)[ *?]")
  "Regexp that matches the status output of the debugger.
3The first group matches the procedure name, the second group the
state number.")

(defconst mds--maple-output-re
  (concat "^\\([^ \n][^\n]*\\):\n\\s-*\\([1-9][0-9]*\\)\ " ; (1,2) procname: state
	  "\\(?:[^\r]*\\)"                                 ; next line
	  "\\(" mds--prompt-re "\\)$"))                    ; (3) prompt

(defconst mds--showstat-re
  (concat "^\n\\(" maplev--name-re "\\) := proc("))

(defconst mds-start-tag-re "^<\\([^>]+\\)>"
  "Regular expression that matches start tag.
The tag has format <tag-name>.  Group 0 matches the tag,
group 1 matches tag-name.")

(defconst mds--client-attach-re "^open from \\([^\n]+\\)\n$"
  "Regexp to match message when a client attaches.
The first group identifies SOMETHING.")
  

(defconst mds--emaple-done-re "That's all, folks.\n"
  "Regexp that matches the final message send by emaple
before the process terminates.")

(defconst mds-port 10000
  "Port used by mds server")

(defvar mds-proc nil "process for the server.")

(defconst mds-log-buffer-name "*mds-log*"
  "Name of buffer used to log connections.")

(defconst mds-max-number-clients 4
  "Maximum number of clients allowed.")

(defconst mds-end-of-msg-re "---EOM---")

;;}}}
;;{{{ variables

(defvar mds-debugging-flag nil "Non-nil when debugging.")
(defvar mds-last-debug-cmd "" "Stores the last debugger command.")
(defvar mds-maple-buffer nil "Temporary buffer associated with maple process.")
(defvar mds-pmark nil "Prompt mark in `mds-buffer'.")
(defvar mds-process nil "Maple process used by mds")
(defvar mds-show-args-on-entry t "Non-nil means print the arguments to a procedure when entering it.")
(defvar mds-showstat-arrow-position nil "Marker for state arrow.")
(defvar mds-showstat-buffer nil "Buffer that displays showstat info.")

(defvar mds-number-pending-clients 0)
(defvar mds-log-buffer nil)

;;}}}
;;{{{ Variables

;;; data structures

(defvar mds-proc-status '()
  "Alist containing status of each known proc.
An entry consists of (proc . status).
Status is either `accepted', `pending', or `rejected'.")

(defvar mds-clients '() 
  "Alist containing info of accepted clients.
An entry consists of (proc buffer . id).")

(defvar mds-number-clients 0
  "Current number of clients.
Maximum is given by `mds-max-number-clients'.")


;;}}}

;;{{{ Access fields in client entry of alist

;; Each entry in the alist, including the key (proc),
;; has the structure
;;
;;    (proc . (ss-buf id . queue))
;;
;; Access elements of a client (entry in `mds-clients').

(defsubst mds--get-proc            (entry) (car entry))
(defsubst mds--get-showstat-buffer (entry) (cadr entry))
(defsubst mds--get-id              (entry) (cadr (cdr entry)))
(defsubst mds--get-queue           (entry) (cddr (cdr entry)))

(defsubst mds-get-client (proc) (assoc proc mds-clients))

(defun mds-find-client-with-id (id)
  "Return entry in `mds-clients' with matching ID.
If none, then return nil."
  (let ((clients mds-clients)
	client match)
    (while (and clients (null match))
      (setq client (car clients))
      (if (eq id (mds--get-id client))
    	  (setq match client)
    	(setq clients (cdr clients))))
    match))

(defun mds-swap-proc-in-clients (oldproc newproc)
    (setq mds-clients (cons (cons newproc (cdr (assoc oldproc mds-clients)))
			    (assq-delete-all oldproc mds-clients))))

(defun mds-add-client-to-alist (proc id)
  (setq mds-clients (cons (mds-alist-entry proc id) mds-clients)))

(defun mds-alist-entry (proc id)
  "Create an entry for the `mds-clients' alist.
Generate new buffers for the showstat and Maple output."
  (let ((buf (mds-showstat-generate-buffers proc))
	(queue (mds-queue-create proc)))
    (cons proc 
	  (cons buf (cons id queue)))))

;;(setq client1 (car mds-clients))
;;(mds--get-showstat-buffer client1)
;;(mds--get-id client1)
;;(mds--get-queue client1)

;;}}}

;;{{{ Start and stop server

(defun mds-start ()
  "Start an mds server; return the process."
  (interactive)
  (setq mds-log-buffer (get-buffer-create mds-log-buffer-name))
  (unless (process-status "mds")
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
    (message "Maple Debugger Server started")))

(defun mds-stop ()
  "Stop the Emacs mds server."
  (interactive)
  ;; Delete each entry, killing process and buffers
  (while mds-clients
    (mds-delete-client (mds--get-proc (car mds-clients))))
  ;; Kill the server process and buffer
  (if (process-status mds-proc)
      (delete-process mds-proc))
  (mds-kill-buffer mds-log-buffer)
  (message "Maple Debugger Server stopped"))

(defun toggle-mds (&optional arg)
  "Toggle the Maple Debugger Server on or off.
With prefix argument ARG, start the server if ARG
is positive, otherwise stop the server."
  (interactive "P")
  (let ((up (process-status "mds")))
    (if (and arg
	   (if (> (prefix-numeric-value arg) 0)
	       up
	     (not up)))
	(message (format "Maple Debugger Server already %s"
			 (if up "started" "stopped")))
      (if up
	  (mds-stop)
	(mds-start)))))

(defun mds-restart ()
  "Restart the Maple Debugger Server."
  (interactive)
  (if (process-status "mds")
      (mds-stop))
  (mds-start))
    


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
	   ((eq status 'rejected) 
	    ;; (mds-send-client proc "Sorry, cannot connect at this time.\n")
	    (mds-writeto-log proc "rejected client"))
	   ((eq status 'login)    (mds-writeto-log proc "begin login"))))))
     ((string= msg "connection broken by remote peer\n")
      ;; A client has unattached.
      ;; Delete associated buffers.
      (mds-writeto-log proc
	       (format "%sclient has unattached"
		       (if (mds-delete-client proc)
			   "accepted " ""))))
     ((string= msg "deleted\n"))
     (t (error "unexpected sentinel message: %s" msg)))))


(defun mds-get-client-status (proc)
  (if (assoc proc mds-clients)
      'accepted
    (if (>= mds-number-clients mds-max-number-clients)
	'rejected
      (mds-add-client-to-alist proc "dummy id")
      (setq mds-number-clients (1+ mds-number-clients))
      'accepted)))



;;}}}
;;{{{ Filter

(defun mds-filter (proc msg)
  (let ((status (mds-get-client-status proc)))
    (cond
     ((eq status 'accepted)
      ;; route msg to queue
      (mds-writeto-log proc "{{{")
      (mds-writeto-log proc msg)
      (mds-writeto-log proc "}}}")
      (let ((queue (mds--get-queue (mds-get-client proc))))
	(mds-queue-filter queue msg)))
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
  "Create and return a queue for PROC.
The structure has the form (proc . buf)."
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
	      (with-current-buffer (mds--get-showstat-buffer client)
		(mds-handle-stream proc msg)))))))))

;;}}}

;;{{{ Handle Clients

(defun mds-add-client (proc id)
  "Add a Maple client.  Buffers are created and added to the `mds-clients' alist."
  (if (assoc proc mds-clients)
      (error "client already exists in list")
    ;; Look for a matching id in the atable.
    ;; If it exists, then associate this proc with it
    (let ((entry (mds-find-client-with-id id)))
      (if entry
	  (progn 
	    (mds-swap-proc-in-clients (mds--get-proc entry) proc)
	    (mds-writeto-log proc "moved client"))
	;; Create and add mds buffers, add to
	(mds-add-client-to-alist proc id)
	(mds-writeto-log proc "added client")))))


(defun mds-delete-client (proc)
  "If PROC is a client process, delete PROC it associated buffers, 
remove the entry from the alist, and decrement `mds-number-clients'."
  (let ((entry (assoc proc mds-clients)))
    (and entry
	 (progn
	   (mds-writeto-log proc "removing client")
	   ;; kill the process and buffers
	   (delete-process proc)
	   (mds-showstat-kill-buffers (mds--get-showstat-buffer entry))
	   (setq mds-clients (delq entry mds-clients)
		 mds-number-clients (1- mds-number-clients))))))

;;}}}

;;{{{ talk to client

(defun mds-send-client (proc msg)
  "Send MSG to client with process PROC."
  (let ((client (mds-get-client proc)))
    (process-send-string proc msg)))

;;}}}

(defun mds-extract-tag (msg) 
  "Return (tag . MSG), where the tags have been removed from MSG.
The format of MSG must be \"<tag>msg</tag>\"."
  (if (string-match mds-start-tag-re msg)
      (let* ((tag (match-string 1 msg))
	     (len (match-end 0)))
	(cons tag (substring msg len (- (1+ len)))))
    ;; FIXME: this error gets caught and not displayed
    (error "no tag in message: '%s...'" (substring msg 0 (min 10 (length msg))))))

;;{{{ mds-handle-stream

(defun mds-handle-stream (proc msg)
  (let* ((client (mds-get-client proc))
	 (buf (mds--get-showstat-buffer client))
	 (tag-msg (mds-extract-tag msg))
	 (tag (car tag-msg))  ; name of tag
	 (msg (cdr tag-msg))) ; msg with no tags
		  
    ;; route MSG to proper buffer
      (with-current-buffer buf
	(with-syntax-table maplev--symbol-syntax-table
	  (cond
	   ((string= tag "DBG_STATE")
	    ;;{{{ msg contains debugger status
	    
	    (string-match mds--debugger-status-re msg)
	    (let ((cmd-output (substring msg 0 (match-beginning 1)))
		  (procname (match-string 1 msg))
		  (state    (match-string 2 msg))
		  (rest (substring msg (match-end 2)))
		  ;;(exec (nth 0 closure))
		  ;;(func (nth 1 closure))
		  ;;(proc (nth 2 closure)))
		  )

	      ;; Assign global variables.
	      (mds-showstat-set-debugging-flag t)

	      ;; (if exec
	      ;;     ;; A statement was executed in showstat;
	      ;;     ;; update the showstat buffer.
	      (mds-showstat-update procname state)

	      ;; Move focus to showstat buffer.
	      ;; (switch-to-buffer mds-showstat-buffer)
	      ;; Display the Maple output, stored in cmd-output.  If func is
	      ;; assigned, then first apply it to the string in cmd-output.
	      ;; The proc procedure, if assigned, will be applied to the
	      ;; generated output region.
	      (mds-showstat-display-debugger-output
	       ;; (if func
	       ;;     (funcall func cmd-output)
	       ;;   cmd-output)
	       ;; proc
	       cmd-output))

	    ;;}}}
	    )
	   ((string= tag "DBG_SHOW")
	    ;; (string-match mds--showstat-re msg)
	    ;; handle showstat output
	    (mds-showstat-display-proc msg))
	   ;; otherwise print to debugger output buffer
	   (t (mds-showstat-display-debugger-output msg)))))))

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

(provide 'mds '(mds-start))

;;{{{ Manual Tests
;;
;; (load "mds-showstat.el")
;; (load "mds.el")
;; (mds-start)
;; (mds-stop)

;;}}}

;;; mds.el ends here

