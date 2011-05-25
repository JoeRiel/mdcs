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
The first group matches the procedure name, the second group the
state number.")

(defconst mds--maple-output-re
  (concat "^\\([^ \n][^\n]*\\):\n\\s-*\\([1-9][0-9]*\\)\ " ; (1,2) procname: state
	  "\\(?:[^\r]*\\)"                                 ; next line
	  "\\(" mds--prompt-re "\\)$"))                    ; (3) prompt

(defconst mds--showstat-re
  (concat "^\n\\(" maplev--name-re "\\) := proc("))

(defconst mds--client-attach-re "^open from \\([^\n]+\\)\n$"
  "Regexp to match message when a client attaches.
The first group identifies SOMETHING.")
  

(defconst mds--emaple-done-re "That's all, folks.\n"
  "Regexp that matches the final message send by emaple
before the process terminates.")

(defconst mds-port 10000
  "Port used by mds server")

(defvar mds-proc nil "process for the server")

(defconst mds-buffer "*mds*"
  "Buffer associated with mds server")

(defconst mds-max-number-clients 4
  "Maximum number of clients allowed.")

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

;; Access elements of a client (entry in `mds-clients')
;; Don't forget the entry, which comes from (assoc ...),
;; includes the key, which is the proc.
(defsubst mds--get-proc            (entry) (car entry))
(defsubst mds--get-showstat-buffer (entry) (cadr entry))
(defsubst mds--get-id              (entry) (cddr entry))

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
  (setq mds-clients (cons (cons proc (mds-alist-entry proc id)) mds-clients)))


(defun mds-alist-entry (proc id)
  "Create an entry for the `mds-clients' alist.
Generate new buffers for the showstat and Maple output."
  (cons (mds-showstat-generate-buffer proc) id))





;;}}}

;;{{{ Start and stop server

(defun mds-start ()
  "Start an mds server; return the process."
  (interactive)
  (get-buffer-create mds-buffer)
  (unless (process-status "mds")
    (setq mds-clients '()
	  mds-proc (make-network-process 
			   :name "mds" 
			   :buffer mds-buffer
			   :family 'ipv4 
			   :service mds-port 
			   :sentinel 'mds-sentinel 
			   :filter 'mds-filter
			   :server 't))))

(defun mds-stop nil
  "Stop the Emacs mds server."
  (interactive)
  ;; Delete each entry, killing process and buffers
  (while mds-clients
    (let ((entry (car mds-clients)))
      (delete-process (mds--get-proc entry))
      (mds-kill-buffer (mds--get-showstat-buffer entry)))
    (setq mds-clients (cdr mds-clients)))
  ;; Kill the server process and buffer
  (if (process-status mds-proc)
      (delete-process mds-proc))
  (mds-kill-buffer mds-buffer))

;;}}}
;;{{{ Sentinel

(defun mds-sentinel (proc msg)
  (unless (eq msg "")
    (cond
     ((string-match mds--client-attach-re msg)
      ;; A client has attached.
      (let ((conn (match-string 1 msg)))
	(mds-log proc (format "client has attached: %s" (substring msg 0 -2)))
	(let ((status (mds-get-client-status proc)))
	  (cond
	   ((eq status 'accepted) (mds-log proc "accepted client"))
	   ((eq status 'rejected) (mds-log proc "rejected client"))
	   ((eq status 'login)    (mds-log proc "begin login"))))))
     ((string= msg "connection broken by remote peer\n")
      ;; A client has unattached.
      (mds-log proc
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
  "CLOSURE is a list, \(EXEC FUNC PROC\), MSG is a Maple output string.
This procedure is a filter passed to `tq-enqueue'.  If MSG
contains debugger status, the `mds-showstat-buffer' is updated.

The EXEC element of CLOSURE is a flag; if non-nil then the output
is from executing statements in the debugged code (rather than
evaluating expressions entered by the user).

If `mds-showstat-debugging-flag' is non-nil, MSG is first processed by
FUNC (if non-nil), then written to `mds-debugger-output-buffer',
and the new region is processed by PROC (if non-nil); otherwise
MSG is written to `mds-buffer'."

  (let ((status (mds-get-client-status proc)))
    (cond
     ((eq status 'accepted)
      ;; route MSG to proper buffer
      (with-current-buffer (mds--get-showstat-buffer
			    (assoc proc mds-clients))
	(with-syntax-table maplev--symbol-syntax-table
	  (cond
	   ((string-match mds--debugger-status-re msg)
	    ;;{{{ msg contains debugger status

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
	   ((string-match mds--showstat-re msg)
	    ;; handle showstat output
	    (mds-showstat-display-proc msg))
	   ;; otherwise print to debugger output buffer
	   (t (mds-showstat-display-debugger-output msg))))))
     ((eq status 'pending)
      ;;(mds-login proc msg)
      )
     ((eq status 'rejected)
      (mds-log proc "ignoring msg from rejected client")))))

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
	    (mds-log proc "moved client"))
	;; Create and add mds buffers, add to
	(mds-add-client-to-alist proc id)
	(mds-log proc "added client")))))


(defun mds-delete-client (proc)
  (let ((entry (assoc proc mds-clients)))
    (and entry
	 (progn
	   (mds-log proc "removing client")
	   ;; kill the showstat buffer
	   (mds-kill-buffer (mds--get-showstat-buffer entry))
	   (setq mds-clients (delq entry mds-clients)
		 mds-number-clients (1- mds-number-clients))))))
				   
	   

;;}}}

;;{{{ talk to client

(defun mds-send-client (proc msg)
  "Send MSG to client with process PROC."
  (process-send-string proc msg))

;;}}}


(defun mds-kill-buffer (buf)
  "Kill buffer BUF; verify that it is a live buffer."
  (and (bufferp buf)
       (not (buffer-name buf))
       (kill-buffer buf)))

;;{{{ log stuff

(defun mds-log (proc msg)
  (with-current-buffer mds-buffer
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

