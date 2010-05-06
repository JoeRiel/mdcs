;; mdb.el --- major mode for debugging Maple code
;; Copyright (C) 2009 Joseph S. Riel, all rights reserved

;; Author:     Joseph S. Riel <jriel@maplesoft.com>
;; Created:    Jan 2009
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
;; For example, if `mdb-stopwhen' is activated at a for-loop, the
;; default should not be the symbol 'for', but rather the
;; loop-variable.

;; Expand stopwhen to operate on local variables.  That has been done
;; however, it is less useful than it could be because debugopts
;; does not currently handly local variables of module exports.

;;}}}

;;{{{ requirements

(require 'tq)
(require 'mdb-ir)
(require 'maplev)
(eval-when-compile
  (require 'hl-line))
(declare-function mdb-showstat-update            "mdb-showstat" (procname state))
(declare-function mdb-showstat-get-buffer-create "mdb-showstat" ())

(autoload 'mdb-showstat-update            "mdb-showstat" "" t)
(autoload 'mdb-showstat-get-buffer-create "mdb-showstat" "" t)

;;}}}

;;{{{ customization

(defgroup mdb nil
  "Major mode for debugging Maple."
  :group 'tools)

(defcustom mdb-maple-cmd "emaple"
  "*Shell command to launch command-line maple.
The default, emaple, is a customizable script that calls a
binary, pmaple.  It does not use cmaple, which does not properly
handle prompts in a pipe."
  :type 'string
  :group 'mdb)

(defcustom mdb-maple-setup-switches
  nil
  "*List of command-line switches passed to `mdb-maple-cmd'."
  :type '(repeat list (string))
  :group 'mdb)

;;{{{   prompts and cursors

(defcustom mdb-prompt "(**) "
  "*eMaple prompt.
Changing this, alas, does not currently change the prompt because the
prompt is defined as a C-preprocessor-macro in the emaple source."
  :type 'string
  :group 'mdb)

(defcustom mdb-debug-prompt "(*DBG*) "
  "*eMaple debug prompt.
Changing this, alas, does not currently change the prompt because the
prompt is defined as a C-preprocessor-macro in the emaple source."
  :type 'string
  :group 'mdb)

(defcustom mdb-cursor-waiting 'hollow
  "Cursor used in showstat buffer when waiting for Maple to respond."
  :type 'symbol
  :group 'mdb)

(defcustom mdb-cursor-ready 'box
  "Cursor used in showstat buffer when ready for a user input."
  :type 'symbol
  :group 'mdb)

;;}}}

(defcustom mdb-history-size 50
  "Number of inputs the input-ring can hold."
  :type 'integer ; ensure positive
  :group 'mdb)

(defcustom mdb-debugger-break (format "\n%s\n" (make-string 40 ?-))
  "String inserted into `mdb-debugger-output-buffer' when debugging starts."
  :type 'string
  :group 'mdb)

;;{{{   faces

(defgroup mdb-faces nil
  "Faces for mdb and related modes."
  :group 'mdb)

(defface mdb-face-arg
  '((((class color) (background dark)) (:foreground "magenta")))
  "Face for arguments in a showstat buffer."
  :group 'mdb-faces)

(defface mdb-face-prompt
  '((((class color) (background dark)) (:foreground "Green")))
  "Face for the prompt in an mdb buffer."
  :group 'mdb-faces)

(defface mdb-face-procname-entered
  '((((class color) (background dark)) (:foreground "Cyan")))
  "Face for the procname at entry in a debugger output buffer."
  :group 'mdb-faces)

(defface mdb-face-procname-cont
  '((((class color) (background dark)) (:foreground "LightBlue")))
  "Face for the procname when continued in a debugger output buffer."
  :group 'mdb-faces)

;;}}}


;;}}}
;;{{{ constants

(defconst mdb-version "1.2" "Version number of this version of mdb.")

(defconst mdb--prompt-re (format "^\\(?:\\(%s\\)\\|%s\\)"
				 (regexp-quote mdb-debug-prompt)
				 (regexp-quote mdb-prompt))
  "Regexp matching Maple prompt.  If the first group matches,
then this is a debug-prompt.")

(defconst mdb--prompt-with-cr-re (concat mdb--prompt-re "$")
  "Regexp matching Maple prompt with preceding carriage return.
This is the prompt as output from the maple process.")

(defconst mdb--debugger-status-re
  (concat "^\\(" maplev--name-re "\\):\n\\s-*\\([1-9][0-9]*\\)[ *]")
  "Regexp that matches the status output of the debugger.
The first group matches the procedure name, the second group the
state number.")

(defconst mdb--maple-output-re
  (concat "^\\([^ \n][^\n]*\\):\n\\s-*\\([1-9][0-9]*\\)\ " ; (1,2) procname: state
	  "\\(?:[^\r]*\\)"                                 ; next line
	  "\\(" mdb--prompt-re "\\)$"))                    ; (3) prompt

(defconst mdb--emaple-done-re "That's all, folks.\n"
  "Regexp that matches the final message send by emaple
before the process terminates.")

(defconst mdb--end-of-process-output-re
  (concat "^\\(?:"
	  mdb--prompt-re
	  "[ \n]*\\|"
	  mdb--emaple-done-re
	  "\\)\\'")
  "Regexp that matches the end of process output.")



;;}}}
;;{{{ variables

;; N.B. All variables are global, which means that only one maple
;; debugging session can be run in one emacs session.  This may be
;; redone after a prototype is operational.

(defvar mdb-buffer nil "Buffer used as shell.")
(defvar mdb-debugging-flag nil "Non-nil when debugging.")
(defvar mdb-debugger-output-buffer nil "Buffer used for debugger output.")
(defvar mdb-ir nil "Input ring used by mdb for history completion.")
(defvar mdb-last-debug-cmd "" "Stores the last debugger command.")
(defvar mdb-maple-buffer nil "Temporary buffer associated with maple process.")
(defvar mdb-maple-procname nil "Name of current procedure being debugged.
This can be avoided with thisproc, but that requires Maple 14.")
(defvar mdb-pmark nil "Prompt mark in `mdb-buffer'.")
(defvar mdb-process nil "Maple process used by mdb")
(defvar mdb-show-args-on-entry t "Non-nil means print the arguments to a procedure when entering it.")
(defvar mdb-showstat-arrow-position nil "Marker for state arrow.")
(defvar mdb-showstat-buffer nil "Buffer that displays showstat info.")
(defvar mdb-showstat-procname "" "Name of current showstat procedure.")
(defvar mdb-showstat-state "1")
(defvar mdb-tq nil "Transaction-queue used by mdb.")
(defvar mdb-watch-alist nil
  "Alist for storing watch variables.  The keys are procedure names,
the values are additional alists.")

;;}}}
;;{{{ functions

(defun mdb-toggle-show-args (&optional arg)
  "Toggle whether to display arguments when entering a procedure.
With prefix argument ARG, show arguments if ARG is positive,
otherwise do not show them."
  (interactive "P")
  (setq mdb-show-args-on-entry
	(if (null arg) (not mdb-show-args-on-entry)
	  (> (prefix-numeric-value arg) 0)))
  (message "Show arguments when entering a procedure %s"
	   (if mdb-show-args-on-entry "enabled" "disabled")))


(defun mdb-shutdown ()
  "Shutdown the debugger and kill the temporary buffers."
  (if (and mdb-tq (tq-process mdb-tq)) (tq-close mdb-tq))
  (if mdb-maple-buffer (kill-buffer mdb-maple-buffer))
  (if mdb-debugger-output-buffer (kill-buffer mdb-debugger-output-buffer))
  (if mdb-showstat-buffer (kill-buffer mdb-showstat-buffer)))

(defun mdb-kill-maple ()
  "Kill the current maple process.
Switch to the `mdb-buffer' and restart the Maple process."
  (interactive)
  (pop-to-buffer mdb-buffer)
  (if (and mdb-tq (tq-process mdb-tq)) (tq-close mdb-tq))
  (mdb-start-maple))

(defun mdb-start-maple ()
  "Start the maple process.
Assign the global variables `mdb-maple-buffer', `mdb-process', and `mdb-tq'."
  (setq mdb-maple-buffer (get-buffer-create "*maple*"))
  (setq mdb-process (mdb-start-maple-process))
  ;; Start and assign the transaction queue.
  (setq mdb-tq (tq-create mdb-process)))

(defun mdb-handle-maple-output (closure msg)
  "CLOSURE is a list, \(EXEC FUNC PROC\), MSG is a Maple output string.
This procedure is a filter passed to `tq-enqueue'.  If MSG
contains debugger status, the `mdb-showstat-buffer' is updated.

The EXEC element of CLOSURE is a flag; if non-nil then the output
is from executing statements in the debugged code (rather than
evaluating expressions entered by the user).

If `mdb-debugging-flag' is non-nil, MSG is first processed by
FUNC (if non-nil), then written to `mdb-debugger-output-buffer',
and the new region is processed by PROC (if non-nil); otherwise
MSG is written to `mdb-buffer'."

  (if (string-match mdb--debugger-status-re msg)
      ;;{{{ msg contains debugger status

      (let ((cmd-output (substring msg 0 (match-beginning 1)))
	    (procname (match-string 1 msg))
	    (state    (match-string 2 msg))
	    (rest (substring msg (match-end 2)))
	    (exec (nth 0 closure))
	    (func (nth 1 closure))
	    (proc (nth 2 closure)))

	;; Assign global variables.
	(setq mdb-maple-procname procname)  ;; FIXME: not used (at least in M14)
	(mdb-set-debugging-flag t)

	(if exec
	    ;; A statement was executed in showstat;
	    ;; update the showstat buffer.
	    (mdb-showstat-update procname state))

	;; Move focus to showstat buffer.
	(switch-to-buffer mdb-showstat-buffer)
	;; Display the Maple output, stored in cmd-output.  If func is
	;; assigned, then first apply it to the string in cmd-output.
	;; The proc procedure, if assigned, will be applied to the
	;; generated output region.
	(mdb-display-debugger-output
	 (if func
	     (funcall func cmd-output)
	   cmd-output)
	 proc))

    ;;}}}
    ;;{{{ msg does not contain debugger status

    ;; Update the `mdb-debugging-flag' variable, provided msg contains
    ;; a prompt.  
    ;; TODO: doesn't it *have* to contain a prompt?
    (when (string-match mdb--prompt-re msg)
      (mdb-set-debugging-flag (match-string 1 msg))
      ;; font-lock the prompt.
      (set-text-properties (match-beginning 0) (match-end 0)
			   '(face mdb-face-prompt rear-nonsticky t)
			   msg))

    ;; Determine what to do with msg.
    (if mdb-debugging-flag
	;; display, but strip any prompt
	(mdb-display-debugger-output (if (string-match mdb--prompt-re msg)
					 (substring msg 0 (match-beginning 1))
				       msg))

      ;; Not debugging, so MSG goes to mdb-buffer.
      (let ((buffer mdb-buffer))
	(with-current-buffer buffer
	  (goto-char (point-max))
	  (insert "\n" msg)
	  (if (string-match mdb--emaple-done-re msg)
	      (progn
		(message msg)
		(kill-buffer buffer))
	    (set-marker mdb-pmark (point))))
	;; Move point to `mdb-buffer' if we are finished debugging.  Using
	;; mdb-showstat-arrow-position is a bit of a hack and may not work
	;; once we provide an option to move to showstat...
	(unless (marker-buffer mdb-showstat-arrow-position)
	  (switch-to-buffer buffer)))))

  ;;}}}
  )

(defun mdb-set-debugging-flag (debugging)
  "Compare DEBUGGING with `mdb-debugging-flag'.
A difference indicates that debugging has started/stopped.  Reassign
`mdb-debugging-flag' and run either `mdb-start-debugging' or
`mdb-finish-debugging'."
  (if mdb-debugging-flag
      (unless debugging
	;; turn-off debugging.
	(setq mdb-debugging-flag nil)
	(mdb-finish-debugging))
    (when debugging
      ;; turn-on debugging
      (mdb-start-debugging)
      (setq mdb-debugging-flag t))))

(defun mdb-start-debugging ()
  "Called when the debugger starts."
  (mdb-display-debugger-output
   (propertize mdb-debugger-break
	       'face 'mdb-face-prompt
	       'rear-nonsticky t)))


(defun mdb-finish-debugging ()
  "Called when the debugger finishes."
  (ding)
  (message "finished debugging")
  ;; Clear overlay in showstat buffer.
  ;; Does this handle hl-line?
  (set-marker mdb-showstat-arrow-position nil)
  ;; Reset the showstat variables.
  (setq mdb-showstat-procname ""
	mdb-showstat-state "1"))

(defun mdb-start-maple-process ()
  "Launch a maple process and return the process.
This command must be run from the maple debugger buffer."
  ;; Fire up the maple process.  Use pipes to communicate.
  (let* ((process-connection-type nil)
	 (proc (apply 'start-process
		      "maple"            ;; name of process; arbitrary
		      mdb-maple-buffer   ;; buffer associated with process
		      mdb-maple-cmd      ;; program file name
		      mdb-maple-setup-switches)))
    ;; Wait for Maple prompt before launching tq.
    (with-current-buffer mdb-maple-buffer
      ;; (display-buffer (current-buffer))
      (with-temp-message
	  "waiting for prompt..."
	(let ((cnt 100))
	  (while (and
		  (< 0 cnt)
		  (progn
		    (sleep-for 0.1)
		    (goto-char (point-max))
		    (forward-line 0)
		    (not (looking-at mdb--prompt-re))))
	    (setq cnt (1- cnt))
	    (if (zerop (% cnt 10))
		(message "waiting for prompt (%d)" (/ cnt 10))))
	  (when (not (< 0 cnt))
	    (lwarn '(mdb) ':error "%s%s"
		   " cannot start emaple; switching to its output buffer.  "
		   " It might provide a clue as to what is wrong."
		   )
	    (switch-to-buffer mdb-maple-buffer)))))
    proc))

(defun mdb--make-surround (prefix suffix)
  "Return an anonymous procedure, lambda (MSG) ...), that surrounds MSG with PREFIX and SUFFIX.
If either is nil, use the empty string."
  `(lambda (msg) (format "%s%s%s"
			 ,(or prefix "")
			 msg
			 ,(or suffix "")
			 )))

(defun mdb-send-string (str exec &optional prefix suffix proc)
  "Send STR to the maple process.
The output from the maple process is handled by `mdb-handle-maple-out'.
The optional PREFIX and SUFFIX are added to the displayed output, unless
PROC is also assigned, in which case it is used to process the region."
  (tq-enqueue mdb-tq
	      str
	      mdb--end-of-process-output-re
	      (list exec (mdb--make-surround prefix suffix) proc) ; closure
	      #'mdb-handle-maple-output                           ; handler
	      'delay))

(defun mdb-send-last-command ()
  "Reexecute the last debugger command, `mdb-last-debug-cmd'."
  ;; We cannot use the debugger's history mechanism because showstat
  ;; commands have been used to display the procedure.  That, and
  ;; emaple does not (yet) have a history mechanism.
  (interactive)
  (mdb-send-string (concat mdb-last-debug-cmd "\n") t))



;;}}}

;;{{{ experimental

(defun mdb--get-state ()
  "Return the current Maple state number."
  (interactive)
  (save-excursion
    (end-of-line)
    (if (re-search-backward "^ *\\([1-9][0-9]*\\)\\([* ]\\)" nil t)
	(match-string-no-properties 1)
      (ding)
      (message "no previous state in buffer"))))

;;}}}

;;{{{ mdb-mode-map and electric functions

(defun mdb-skip-prompt ()
  "Skip past the text matching regexp `mdb--prompt-re'."
  (when (looking-at mdb--prompt-re)
    (goto-char (match-end 0))))

(defun mdb-bol (arg)
  "Go back to the beginning of line, then skip forward past the prompt, if any.
If a prefix argument is given (\\[universal-argument]), then do
not skip prompt -- go straight to column 0.  The prompt skip is
done by skipping text matching the regular expression `mdb--prompt-re'."
  (interactive "P")
  (forward-line 0)
  (when (null arg) (mdb-skip-prompt)))

(defun mdb-beginning-of-debug-line-p ()
  "Return non-nil if POINT is at the beginning of a debug input line.
Otherwise return nil."
  (save-excursion
    (forward-line 0)
    (looking-at (concat mdb-debug-prompt "$"))))

(defun mdb-beginning-of-line-p ()
  "Return non-nil if POINT is at the beginning of an input line.
Otherwise return nil."
  (save-excursion
    (forward-line 0)
    (looking-at (concat mdb--prompt-re "$"))))

(defun mdb-debug-line-p ()
  "Return non-nil if the input line is a debug line."
  (save-excursion
    (forward-line 0)
    (looking-at mdb-debug-prompt)))

(defun mdb-electric-newline ()
  "Process the newline key in the mdb buffer."
  (interactive)
  (let (pos input)
    (goto-char mdb-pmark)
    ;;(setq pos (line-end-position))
    (while (re-search-forward "\\(?:\\s-*\n\\)+\\s-*" (point-max) t)
      (replace-match " " nil t))
    (goto-char mdb-pmark)
    (setq input (buffer-substring-no-properties mdb-pmark (point-max)))
    (goto-char (point-max))
    (if (and (mdb-debug-line-p)
	     (string-match "^ *\\(?:\\(cont\\|into\\|next\\|step\\|outfrom\\|return\\)\n\\|\\(\\(?:un\\)?stopat\\>\\)\\)" input))
	(if (match-string 1 input)
	    (setq mdb-last-debug-cmd (match-string-no-properties 1 input))
	  ;; This is currently not handled.  Maybe it should be passed
	  ;; to `mdb-send-string' as an optional argument.  The
	  ;; showstat buffer must be updated, but *after* the command
	  ;; has been sent and processed.
	  ;; (setq mdb-showstat-update-p t)))
	  ))
    ;; Enter the line into history.
    (mdb-ir-enter mdb-ir)
    (mdb-send-string (concat input "\n") t)))

(defun mdb-electric-space ()
  "If at start of debugger input, execute `mdb-send-last-command'.
Otherwise insert a space."
  (interactive)
  (if (mdb-beginning-of-debug-line-p)
      (mdb-send-last-command)
    (self-insert-command 1)))

(defun mdb-history-next ()
  "Scroll input-ring to next item."
  (interactive)
  (mdb-ir-scroll mdb-ir 'next))

(defun mdb-history-prev ()
  "Scroll input-ring previous next item."
  (interactive)
  (mdb-ir-scroll mdb-ir))

(defvar mdb-mode-map
  (let ((map (make-sparse-keymap))
	(bindings
	 '((" "    . mdb-electric-space)
	   ("\C-a" . mdb-bol)
	   ("\C-m" . mdb-electric-newline)
	   ("\ep" . mdb-history-prev)
	   ("\en" . mdb-history-next)
	   ([(up)] . mdb-history-prev)
	   ([(down)] . mdb-history-next)
	   )))
    (mapc (lambda (binding) (define-key map (car binding) (cdr binding)))
	  bindings)
    map)
  "Key bindings for `mdb-mode'.")

;;}}}
;;{{{ mdb-mode

(defun mdb-mode ()
  "Major mode for debugging Maple.
Special commands:
\\{mdb-mode-map}"
  (interactive)
  (kill-all-local-variables)
  ;; Setup key/mouse bindings
  (use-local-map mdb-mode-map)
  (setq major-mode 'mdb-mode)
  (setq mode-name "Maple Debugger")

  (setq mdb-debugging-flag nil
	mdb-last-debug-cmd "next"
	mdb-maple-buffer nil
	mdb-pmark nil
	mdb-showstat-buffer nil
	mdb-debugger-output-buffer nil ;; why nil?
	mdb-tq nil)

  ;; Create prompt maker
  (setq mdb-pmark (make-marker))
  (set-marker mdb-pmark (point-max))

  ;; Setup the history ring
  (setq mdb-ir (mdb-ir-create mdb-history-size mdb-pmark))

  ;; Create the showstat buffer and assign it to `mdb-showstat-buffer'.
  (setq mdb-showstat-buffer (mdb-showstat-get-buffer-create))

  
  (mdb-start-maple)

  ;; Add hook to cleanup when buffer is killed
  (add-hook 'kill-buffer-hook 'mdb-shutdown nil 'local)

  ;; Run hooks
  (run-hooks 'mdb-mode-hook))

;;}}}

;;{{{ mdb-debugger-output

(defun mdb-display-debugger-output (msg &optional func)
  "Display MSG in `mdb-debugger-output-buffer'."
  (unless (string= msg "")
    (let ((buf (mdb-debugger-get-output-buffer-create)))
      (display-buffer buf)
      (with-selected-window (get-buffer-window buf)
	(with-current-buffer buf
	  (goto-char (point-max))
	  (let ((beg (point)))
	    (insert msg)
	    (if func
		(funcall func beg (point)))))
	;; Move point (end of buffer) to bottom of window.
	(recenter -1)))))


(defun mdb-debugger-get-output-buffer-create ()
  "Return the `mdb-debugger-output-buffer' buffer, or create and return it.
A new buffer is created if there is no live buffer."
  (or (and (buffer-live-p mdb-debugger-output-buffer) 
	   mdb-debugger-output-buffer)
      (progn
	(setq mdb-debugger-output-buffer (get-buffer-create "*mdb debugger output*"))
	(with-current-buffer mdb-debugger-output-buffer
	  (delete-region (point-min) (point-max))
	  mdb-debugger-output-buffer))))

(defun mdb-debugger-clear-output ()
  "Clear the debugger output buffer."
  (interactive)
  (when (bufferp mdb-debugger-output-buffer)
    (with-current-buffer mdb-debugger-output-buffer
      (delete-region (point-min) (point-max)))))

;;}}}

;;{{{ Evaluate maple expressions

;; This is experimental and not robust.
;; 

(defun mdb-eval-filter (result-ptr msg)

  ;; need to handle exceptions, maple shutting down, debugger...
  (if (string-match mdb--prompt-re msg)
      (set result-ptr (substring msg 0 (1- (match-beginning 0))))
    (error "illegal msg")))


(defun mdb-eval-and-get-maple (expr)
  (let (result)
    (tq-enqueue mdb-tq
		expr
		;; This regex indicates the end of the process output.
		(concat "^\\(?:"
			mdb--prompt-re
			"[ \n]*\\|"
			mdb--emaple-done-re
			"\\)\\'")
		'result
		'mdb-eval-filter
		'delay)
    (let ((cnt 0))
      (while (null result)
	(setq cnt (1+ cnt))
	(sleep-for 0.00001))
      result)))

;;}}}

;;{{{ mdb command

(defun mdb ()
  "Launch a Maple debugger session.  
If one already exists, then pop to the debugging buffer."
  (interactive)

  (if (buffer-live-p mdb-buffer)
      ;; buffer exists; bring up that buffer
      (pop-to-buffer mdb-buffer)

    ;; buffer does not exist:
    ;; launch maple
    (pop-to-buffer (setq mdb-buffer (generate-new-buffer "mdb")))
    (mdb-mode)
    ;; Generate 'fake' prompt.
    (insert (concat (propertize mdb-prompt
				'face 'mdb-face-prompt
				'rear-nonsticky t)))
    (set-marker mdb-pmark (point))))

;;}}}

(provide 'mdb)

;;; mdb.el ends here


