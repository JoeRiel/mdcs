;;; mdcs-server.el
;;; -*- mode: emacs-lisp -*-

;; Copyright (C) 2011 Joseph S. Riel, all rights reserved

;; Author:     Joseph S. Riel <jriel@maplesoft.com>
;; Created:    Jan 2011
;; Keywords:   maple, debugger
;;
;;; Commentary:

;; This file contains the source for the Maple debugger server.

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

;; This package provides a major mode, mdb, for debugging Maple code.
;; It uses the Maple debugger to step through interpreted Maple code,
;; which is displayed in a buffer. It is not a true source code
;; debugger, that is, one that allows stepping through the source
;; *file*; however, it is the next best thing.

;;}}}
;;{{{ TODO

;; A useful feature would be the ability to jump from the *showstat*
;; buffer to the corresponding location in the source file.  That
;; requires (1) a method to map a Maple procedure to its source; (2) a
;; means to locate the matching source line from the interpreted
;; value.  The latter is significantly more difficult than the former.
;; The source can look significantly different from the output.
;; Differences include:
;;
;; (*) indentation
;; (*) comments
;; (*) parenthese
;; (*) macros
;; (*) use-variables
;; (*) conversions ( _passed --> args, etc)
;; (*) ...
;;
;; A way to finesse the problem is to copy the source to a temporary
;; buffer, insert numbered dummy statements, interpret that procedure,
;; go to the line in this procedure, and then uses its adjacent dummy
;; statement to index into the original source.  Inserting dummy
;; statements in a robust way is not trivial, but doable.
;;
;; A simpler approach is pragmatic; generate a partial match and
;; search for it.  If the search fails, then move point to the top of
;; the procedure.  Or count lines and move the same percentage into
;; the procedure.  Or count, say, assignments.  A reasonable approach
;; might to determine the type of the statement (assignment, function,
;; control, ... ) then count forward from the start of the procedure.
;; That won't always work, but may be close enough most of the time.
;; For example, if the statement is the third assignment, then move to
;; the third assignment (search on ":=") in the source.  We would have
;; to skip commented-out code.  Handling stuff in $ifdef macros is a
;; problem.

;; Add stopwhenif to showstat actions (done).  There is a bug in the
;; debugger (SCR64943) that currently prevents this from working.  It
;; isn't all that useful, because it only applies to global variables.
;; Suggest that that be improved.

;; Add function to intelligently select variable/expression at point.
;; For example, if `mdcs-stopwhen' is activated at a for-loop, the
;; default should not be the symbol 'for', but rather the
;; loop-variable.

;; Expand stopwhen to operate on local variables.  That has been done
;; however, it is less useful than it could be because debugopts
;; does not currently handly local variables of module exports.

;;}}}

;;{{{ Lisp Requirements

(require 'mdcs-showstat)
(require 'maplev)
(eval-when-compile
  (require 'hl-line))

;;}}}

;;{{{ customization

(defgroup mdb nil
  "Major mode for debugging Maple."
  :group 'tools)

(defcustom mdcs-maple-cmd "emaple"
  "*Shell command to launch command-line maple.
The default, emaple, is a customizable script that calls a
binary, pmaple.  It does not use cmaple, which does not properly
handle prompts in a pipe."
  :type 'string
  :group 'mdb)

(defcustom mdcs-maple-setup-switches  nil
  "*List of command-line switches passed to `mdcs-maple-cmd'."
  :type '(repeat string)
  :group 'mdb)

(defcustom mdcs-pre-Maple-14 nil
  "*Boolean flag.  Set to non-nil if Maple is a release earlier than Maple 14."
  :type 'boolean
  :group 'mdb)

;;{{{   prompts and cursors

(defcustom mdcs-prompt "(**) "
  "*eMaple prompt.
Changing this, alas, does not currently change the prompt because the
prompt is defined as a C-preprocessor-macro in the emaple source."
  :type 'string
  :group 'mdb)

(defcustom mdcs-debug-prompt "(*DBG*) "
  "*eMaple debug prompt.
Changing this, alas, does not currently change the prompt because the
prompt is defined as a C-preprocessor-macro in the emaple source."
  :type 'string
  :group 'mdb)

(defcustom mdcs-cursor-waiting 'hollow
  "Cursor used in showstat buffer when waiting for Maple to respond."
  :type 'symbol
  :group 'mdb)

(defcustom mdcs-cursor-ready 'box
  "Cursor used in showstat buffer when ready for a user input."
  :type 'symbol
  :group 'mdb)

;;}}}

(defcustom mdcs-history-size 50
  "Number of inputs the input-ring can hold."
  :type 'integer ; ensure positive
  :group 'mdb)

(defcustom mdcs-debugger-break (format "\n%s\n" (make-string 40 ?-))
  "String inserted into `mdcs-debugger-output-buffer' when debugging starts."
  :type 'string
  :group 'mdb)

;;{{{   faces

(defgroup mdcs-faces nil
  "Faces for mdb and related modes."
  :group 'mdb)

(defface mdcs-face-arg
  '((((class color) (background dark)) (:foreground "magenta")))
  "Face for arguments in a showstat buffer."
  :group 'mdcs-faces)

(defface mdcs-face-prompt
  '((((class color) (background dark)) (:foreground "Green")))
  "Face for the prompt in an mdb buffer."
  :group 'mdcs-faces)

(defface mdcs-face-procname-entered
  '((((class color) (background dark)) (:foreground "Cyan")))
  "Face for the procname at entry in a debugger output buffer."
  :group 'mdcs-faces)

(defface mdcs-face-procname-cont
  '((((class color) (background dark)) (:foreground "LightBlue")))
  "Face for the procname when continued in a debugger output buffer."
  :group 'mdcs-faces)

;;}}}


;;}}}

;;{{{ Constants

(defconst mdcs-server-version "1.0" "Version number the mdcs-server.")

(defconst mdcs--prompt-re (format "^\\(?:\\(%s\\)\\|%s\\)"
				 (regexp-quote mdcs-debug-prompt)
				 (regexp-quote mdcs-prompt))
  "Regexp matching Maple prompt.  If the first group matches,
then this is a debug-prompt.")

(defconst mdcs--prompt-with-cr-re (concat mdcs--prompt-re "$")
  "Regexp matching Maple prompt with preceding carriage return.
This is the prompt as output from the maple process.")

(defconst mdcs--debugger-status-re
  (concat "^\\(" maplev--name-re "\\):\n\\s-*\\([1-9][0-9]*\\)[ *?]")
  "Regexp that matches the status output of the debugger.
The first group matches the procedure name, the second group the
state number.")

(defconst mdcs--maple-output-re
  (concat "^\\([^ \n][^\n]*\\):\n\\s-*\\([1-9][0-9]*\\)\ " ; (1,2) procname: state
	  "\\(?:[^\r]*\\)"                                 ; next line
	  "\\(" mdcs--prompt-re "\\)$"))                    ; (3) prompt

(defconst mdcs--showstat-re
  (concat "^\n\\(" maplev--name-re "\\) := proc("))
  

(defconst mdcs--emaple-done-re "That's all, folks.\n"
  "Regexp that matches the final message send by emaple
before the process terminates.")

(defconst mdcs-server-port 10000
  "Port used by mdb server")

(defvar mdcs-server-proc nil "process for the server")

(defconst mdcs-server-buffer "*mdcs-server*"
  "Buffer associated with mdb server")

;;}}}
;;{{{ variables

(defvar mdcs-debugging-flag nil "Non-nil when debugging.")
(defvar mdcs-last-debug-cmd "" "Stores the last debugger command.")
(defvar mdcs-maple-buffer nil "Temporary buffer associated with maple process.")
(defvar mdcs-pmark nil "Prompt mark in `mdcs-buffer'.")
(defvar mdcs-process nil "Maple process used by mdb")
(defvar mdcs-show-args-on-entry t "Non-nil means print the arguments to a procedure when entering it.")
(defvar mdcs-showstat-arrow-position nil "Marker for state arrow.")
(defvar mdcs-showstat-buffer nil "Buffer that displays showstat info.")
;;(defvar mdcs-tq-buffer nil "Buffer used by tq.")

;;}}}
;;{{{ Variables

(defvar mdcs-server-client-id 0)

(defvar mdcs-server-clients '() 
  "Alist where KEY is a client/server process.")

;;}}}

;;{{{ Access fields in client entry of alist

(defun mdcs-server-alist-entry (proc id)
  "Create an entry for the `mdcs-servers-clients' alist.
Generate new buffers for the showstat and Maple output."
  (cons (mdcs-showstat-generate-buffer proc) id))

;; Access elements of a client (entry in `mdcs-server-clients')
;; Don't forget the entry, which comes from (assoc ...),
;; is a cons cell of the key and 
(defsubst mdcs-server--get-proc            (entry) (car entry))
(defsubst mdcs-server--get-showstat-buffer (entry) (cadr entry))
(defsubst mdcs-server--get-id              (entry) (cddr entry))

;;}}}

;;{{{ Start and stop server

(defun mdcs-server-start ()
  "Start an mdb server; return the process."
  (interactive)
  (get-buffer-create mdcs-server-buffer)
  (unless (process-status "mdcs-server")
    (setq mdcs-server-clients '()
	  mdcs-server-client-id 0
	  mdcs-server-proc (make-network-process 
			   :name "mdcs-server" 
			   :buffer mdcs-server-buffer
			   :family 'ipv4 
			   :service mdcs-server-port 
			   :sentinel 'mdcs-server-sentinel 
			   :filter 'mdcs-server-filter
			   :server 't))))

(defun mdcs-server-stop nil
  "Stop the Emacs mdb server."
  (interactive)
  ;; Delete each entry, killing process and buffers
  (while mdcs-server-clients
    (let ((entry (car mdcs-server-clients)))
      (delete-process (mdcs-server--get-proc entry))
      (mdcs-showstat-kill-buffer (mdcs-server--get-showstat-buffer entry)))
    (setq mdcs-server-clients (cdr mdcs-server-clients)))
  ;; Kill the server process and buffer
  (if (process-status mdcs-server-proc)
      (delete-process mdcs-server-proc))
  (kill-buffer mdcs-server-buffer))

;;}}}
;;{{{ Filter and sentinel


(defun mdcs-server-sentinel (proc msg)
  (cond
   ((eq 't (compare-strings msg 0 10 "open from " 0 10))
    ;; A Maple client has attached.
    ;; Create mdb and showstat buffers, and add to alist
    (mdcs-server-add-client proc))
   ((string= msg "connection broken by remote peer\n")
    ;; A Maple client has closed.
    (mdcs-server-delete-client proc))
   ((string= msg "deleted\n"))
   (t (error "unexpected sentinel message: %s" msg))))

;;}}}

;;{{{ mdcs-server-filter

(defun mdcs-server-filter (proc msg)
  "CLOSURE is a list, \(EXEC FUNC PROC\), MSG is a Maple output string.
This procedure is a filter passed to `tq-enqueue'.  If MSG
contains debugger status, the `mdcs-showstat-buffer' is updated.

The EXEC element of CLOSURE is a flag; if non-nil then the output
is from executing statements in the debugged code (rather than
evaluating expressions entered by the user).

If `mdcs-showstat-debugging-flag' is non-nil, MSG is first processed by
FUNC (if non-nil), then written to `mdcs-debugger-output-buffer',
and the new region is processed by PROC (if non-nil); otherwise
MSG is written to `mdcs-buffer'."

  (with-current-buffer (mdcs-server--get-showstat-buffer
			(assoc proc mdcs-server-clients))

    (with-syntax-table maplev--symbol-syntax-table
      (cond
       ((string-match mdcs--debugger-status-re msg)
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
	  (mdcs-showstat-set-debugging-flag t)

	  ;; (if exec
	  ;;     ;; A statement was executed in showstat;
	  ;;     ;; update the showstat buffer.
	  (mdcs-showstat-update procname state)

	  ;; Move focus to showstat buffer.
	  ;; (switch-to-buffer mdcs-showstat-buffer)
	  ;; Display the Maple output, stored in cmd-output.  If func is
	  ;; assigned, then first apply it to the string in cmd-output.
	  ;; The proc procedure, if assigned, will be applied to the
	  ;; generated output region.
	  (mdcs-showstat-display-debugger-output
	   ;; (if func
	   ;;     (funcall func cmd-output)
	   ;;   cmd-output)
	   ;; proc
	   cmd-output))

	;;}}}
	)
      ((string-match mdcs--showstat-re msg)
       ;; handle showstat output
       (mdcs-showstat-display-proc msg))
      ;; otherwise print to debugger output buffer
      ('t (mdcs-showstat-display-debugger-output msg))))))

;;}}}

;;{{{ Handle Clients

(defun mdcs-server-add-client (proc)
  "Add a Maple client.  Buffers are created and added to the `mdcs-servers-clients' alist."
  (if (assoc proc mdcs-server-clients)
      (error "client already exists in list")
    ;; Create and add mdb buffers, add to 
    (setq mdcs-server-client-id (1+ mdcs-server-client-id)
	  mdcs-server-clients (cons (cons proc 
					 (mdcs-server-alist-entry proc mdcs-server-client-id))
				   mdcs-server-clients))
    (mdcs-server-log proc "added client")))

(defun mdcs-server-delete-client (proc)
  (let ((entry (assoc proc mdcs-server-clients)))
    (if (null entry)
	(error "client does not exist")
      (mdcs-server-log proc "removing client")
      ;; kill the showstat 
      (mdcs-showstat-kill-buffer (mdcs-server--get-showstat-buffer entry))
      (setq mdcs-server-clients (delq entry mdcs-server-clients)))))

;;}}}

;;{{{ talk to client

(defun mdcs-server-send-client (proc msg)
  "Send MSG to Maple client with process PROC."
  (process-send-string proc msg))

(defun mdcs-server-send-client-id (id msg)
  "Send MSG to the Maple client with ID."
  (interactive)
  (let ((clients mdcs-server-clients)
	client proc)
    (while (and clients (null proc))
      (setq client (car clients))
      (if (eq id (mdcs-server--get-id client))
    	  (setq proc (car client))
    	(setq clients (cdr clients))))
    (if proc
    	(mdcs-server-send-client proc msg)
      (error "no client with id: %d" id))))

;;}}}

;;{{{ log stuff

(defun mdcs-server-log (proc msg)
  (with-current-buffer mdcs-server-buffer
    (goto-char (point-max))
    (let* ((client (assoc proc mdcs-server-clients))
	   (id (mdcs-server--get-id client)))
      (insert (format "client %d: %s\n" id msg)))
    (set-window-point (get-buffer-window) (point))))

;;}}}

(provide 'mdcs-server)

;;{{{ Manual Tests
;;
;; (load "/home/joe/emacs/mdb/lisp/mdcs-showstat.el")
;; (load "/home/joe/emacs/mdb/lisp/mdcs-server.el")
;; (mdcs-server-start)
;; (mdcs-server-stop)
;; 
;; (mdcs-server-send-client-id 1 "cont")

;;}}}

;;; mdcs-server.el ends here

