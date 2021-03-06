;;; mds-ss.el --- Assign mds-ss-mode

;; Copyright (C) 2011 Joseph S. Riel, all rights reserved

;; Author:     Joseph S. Riel <jriel@maplesoft.com>
;; Created:    May 2011
;; Keywords:   maple, debugger
;;
;;; Commentary:

;; This file contains the source for the showstat functions.

;;; Code:

(eval-and-compile
  (require 'hl-line)
  (require 'maplev)
  (require 'mds-client)
  (require 'mds-custom)
  (require 'mds-out)
  (require 'mds-re)
  (require 'mds-thing)
  (require 'mds-wm))

;;{{{ declarations

;; avoid compiler warnings
(declare-function mds-li-goto-current-state "mds-li")

(eval-when-compile
  (defvar mds-show-args-flag))

;;}}}

;;{{{ constants

;; regular expressions
(defconst mds-ss-where-procname-re "^\\([^ \t\n]+\\): "
  "Match the procname printed by the where command.
It is flush-left, presumably contains no spaces, and is terminated
with a colon.  The colon is omitted from the group-one match.
A more precise regular expression would allow spaces inside back-quotes,
however, such an abomination should break something.")

;;}}}
;;{{{ variables

(defvar mds-ss-addr           nil "Address of current displayed procedure.")
(defvar mds-ss-arrow-position nil "Marker for state arrow.")
(defvar mds-ss-show-args-flag nil "Non-nil means show args when entering a procedure.")
(defvar mds-ss-dead-flag      nil "Non-nil means this is the ss-dead buffer.")
(defvar mds-ss-procname       nil "Name of displayed showstat procedure.")
(defvar mds-ss-state          "1" "Current state of procedure.")
(defvar mds-ss-statement      ""  "String matching a statement; used by dead buffer.")

;; Make variables buffer-local
(mapc #'make-variable-buffer-local
      '(mds-client
	mds-ss-addr
	mds-ss-arrow-position
	mds-ss-show-args-flag
	mds-ss-dead-flag
	mds-ss-procname
	mds-ss-state
	mds-ss-statement
	))

(add-to-list 'overlay-arrow-variable-list 'mds-ss-arrow-position)

;;}}}

;;{{{ send strings to maple client

;; Each of these functions sends a string to the maple engine.

(defun mds-ss-send-client (msg)
  "Send MSG to the Maple client."
  (mds-client-send mds-client msg))

(defun mds-ss-eval-debug-code (cmd &optional hide)
  "Send CMD, with appended newline, to the Maple process.
Echo the command to the output buffer unless HIDE is non-nil."
  (mds-ss-block-input)
  (unless hide
    (mds-out-append-input (mds-client-out-buf mds-client) cmd 'mds-debugger-cmd))
  (mds-ss-send-client (concat "!" cmd "\n")))

(defun mds-ss-eval-expr (expr &optional display unlimited)
  "Send EXPR, with newline, to the Maple process.
Optional DISPLAY, if t, means send EXPR to the output buffer.  If
not nil and not t, send DISPLAY (should be a string).  If nil,
send nothing.  Optional UNLIMITED, if non-nil, means do not
truncate the output.  This function is intended to be used for
evaluating Maple expressions."
  (mds-ss-block-input)
  (if display
      (mds-out-append-input (mds-client-out-buf mds-client)
			    (if (eq display t) expr display)
			    'mds-user-input))
  (mds-ss-send-client (format "!%s%s\n"
			      (if unlimited "_mds_unlimited " "")
			      expr)))

(defun mds-ss-eval-proc-statement (cmd &optional save)
  "Send CMD to the Maple process and the output buffer, tagged as 'cmd.
Append a newline.  If SAVE is non-nil, then save 'cmd as the last
command.  Change cursor type to `mds-cursor-waiting', which
indicates we are waiting for a response from Maple.  This
function assumes we are in the appropriate `mds-ss-buffer'.  This
function is to be used with commands that cause Maple to execute
procedural code."
  (mds-ss-block-input)
  (if save (mds-client-set-last-cmd mds-client cmd))
  (setq cursor-type mds-cursor-waiting)
  (unless (eobp) (forward-char)) ; this indicates 'waiting' in tty Emacs, where cursor doesn't change
  (unless (mds-client-quiet-p mds-client)
    (mds-out-display (mds-client-out-buf mds-client) cmd 'cmd))
  (mds-ss-send-client cmd))

(defun mds-ss-request (expr &optional unlimited)
  "Send the string EXPR to Maple and return the response, as a string.
A newline is appended to EXPR before it is sent (why?).  EXPR
should have no spaces.  If UNLIMITED is non-nil, the complete
response is returned, regardless its size."
  (let ((client mds-client)
	result)
    (save-current-buffer
	(mds-client-set-result client nil)
	(mds-client-send client (format "_mds_request %s%s\n"
					expr
					(if unlimited " unlimited" "")))
	;; Loop until the result is returned.
	(while (null result)
	  (sleep-for 0.0001)
	  (setq result (mds-client-get-result client)))
	(substring result 0 -1))))

;;}}}

;;{{{ buffer creation and update

;;{{{ (*) mds-ss-create-buffer

(defun mds-ss-create-buffer (client &optional alive)
  "Create and return an `mds-ss-buffer' buffer for CLIENT.
If ALIVE is non-nil, create a live buffer."
  (let ((buf (generate-new-buffer (if alive
				      "*mds-ss-live*"
				    "*mds-ss-dead*"))))
    (with-current-buffer buf
      (mds-ss-mode)
      (setq mds-client client
	    mds-ss-arrow-position nil
	    mds-ss-dead-flag (not alive)
	    mds-ss-procname ""
	    mds-ss-state "1"
	    buffer-read-only t
	    maplev-config maplev-config-default)
      (if mds-truncate-lines-flag
	  (toggle-truncate-lines 1)))
    buf))

;;}}}
;;{{{ (*) mds-ss-update

(defun mds-ss-update (buf addr procname state &optional statement)
  "Update the showstat buffer BUF and associated buffer-local variables.
The client field addr, and the buffer local variables
`mds-ss-procname', and `mds-ss-state' are updated.
ADDR is the address of PROCNAME, which is the name of the procedure,
STATE is the current state; all are strings.  If the buffer is already
displaying PROCNAME, then just move the arrow; otherwise
call (maple) showstat to display the new procedure.
The optional STATEMENT is saved in `mds-ss-statement'."

  (with-current-buffer buf
    (let ((trace (mds-client-get-trace mds-client)))
      (unless trace
	;; Revert cursor-type to ready status.
	(setq cursor-type mds-cursor-ready))
      (if (string= addr mds-ss-addr)
	  ;; Address has not changed; move the arrow
	  (progn
	    (unless trace
	      (mds-ss-move-state state))
	    (setq mds-ss-procname procname
		  mds-ss-state    state
		  mds-ss-statement statement))

	;; New procedure; send address and procname to the output buffer.
	(mds-out-display (mds-client-out-buf mds-client)
			 (format "<%s>\n%s" addr procname)
			 'addr-procname)
	;; Update buffer-local variables
	(setq mds-ss-addr     addr
	      mds-ss-procname procname
	      mds-ss-state    state
	      mds-ss-statement statement)
	(unless trace
	  ;; Call Maple showstat routine to update the showstat buffer.
	  (mds-ss-insert-proc buf (format "<%s>\n%s"
					  addr
					  (mds-ss-request (format "debugopts('procdump'=pointto(%s))" addr)
							  'unlimited)))
	  (mds-ss-move-state state))
	(when (and mds-show-args-flag
		   (string= state "1"))
	  (setq mds-ss-show-args-flag t))))))

;;}}}
;;{{{ (*) mds-ss-determine-state

(defun mds-ss-determine-state (statement)
  "Search buffer for STATEMENT and return the statement number.
If not found, display a message."
  (let ((case-fold-search nil)
	(point (point-min))
	term-statement
	cmp done len line state)
    ;; remove 'while true ' from statement.
    ;; debugopts(callstack) inserts that into 'for x in X do ... end do' loops
    (while (string-match "while true " statement)
      (setq statement
	    (concat (substring statement 0 (match-beginning 0))
		    (substring statement (match-end 0)))))
    (setq term-statement (concat statement ";"))
    (while (not done)
      (goto-char point)
      (forward-line)
      (setq point (point))
      ;; move to next line in procedure
      (if (not (re-search-forward mds-re-ss-line nil 'move))
	  (setq done 'failure)
	(setq state (match-string-no-properties 1)
	      line  (match-string-no-properties 2)
	      len   (length line)
	      cmp (compare-strings line nil nil term-statement nil nil))
	(if (eq cmp t)
	    (setq done 'success)
	  (if (<= cmp (- -1 len))
	      ;; line matches start of term-statement;
	      ;; check whether it matches statement (added ;)
	      (if (string= line statement)
		  (setq done 'success)
		;; need to check more lines
		(let ((pos 0))
		  (while
		      (when (re-search-forward mds-re-ss-line nil 'move)
			(setq pos (+ pos len 1)
			      line (match-string-no-properties 2)
			      len (length line)
			      cmp (compare-strings line nil nil term-statement pos nil))
			(if (eq cmp t)
			    (null (setq done 'success))
			  (if (<= cmp (- -1 len))
			      ;; line matches term-statement;
			      ;; check whether it matches statement
			      (if (eq t (compare-strings line nil nil statement pos nil))
				  (null (setq done 'success))
				'continue)))))))))))
    (if (eq done 'success)
	state
      (message "cannot find statement in procedure body"))))

(defun mds-ss-determine-state-OLD (statement)
  "Search buffer for STATEMENT and return the statement number.
Two search routines are used; the second only if the first fails.
Both go to the first match and do not check for additional matches."

  (goto-char (point-min))
  (let ((case-fold-search nil))
    ;; Try simple technique first; works for one-liners.
    ;; FIXME: generate warning if multiple matches.
    (if (search-forward (concat " " statement) nil t)
      (progn
	;; clear buffer-local `mds-ss-statement'
	(setq mds-ss-statement "")
	(mds-ss-get-state))

      ;; Search technique suitable for multi-line `statement'; that
      ;; typically occurs when the procedure on the stack occurs in
      ;; the controlling part (condition of conditional, etc) of the
      ;; statement.  Create str by catenating statements of procedure,
      ;; removing statement numbers, etc.  Store the position of the
      ;; beginning of each line in bols.

      (let (str bols line)
	(while (re-search-forward mds-re-ss-statement nil 'move)
	  (setq	bols (cons (length str) bols)
		line (match-string-no-properties 3)
		str (concat str " " line)))

	;; NB: There is a bug in debugopts(callstack); the `statement'
	;; it returns may have more than one space following a
	;; semicolon.  The following replaces multiple spaces
	;; following a semicolon with a single space.  It does not
	;; test whether that occurs inside a string.

	(let* ((state-re (regexp-quote (replace-regexp-in-string ";  +" "; " statement)))
	       (pos (string-match state-re str)))
	  (if (null pos)
	      (message "cannot find statement in procedure body")
	    (setq bols (nreverse bols))
	    (goto-char (point-min))
	    (re-search-forward mds-re-ss-statement)
	    (while (and bols
			(>= pos (car bols)))
	      (setq bols (cdr bols))
	      (forward-line))
	    ;; move backward to end of preceding line
	    (backward-char)
	    ;; clear buffer-local `mds-ss-statement'
	    (setq mds-ss-statement "")
	    (mds-ss-get-state)))))))

;;}}}
;;{{{ (*) mds-ss-view-dead-proc

(defun mds-ss-view-dead-proc (addr procname state statement)
  "View procedure with address ADDR and name PROCNAME in the dead buffer.
If the STATE is non-nil, use that as the state number to display.
Otherwise, find the statement number from STATEMENT."
  (with-current-buffer (mds-client-dead-buf mds-client)
    (unless (string= procname "")
      (setq mds-ss-procname   procname
	    mds-ss-statement  statement
	    mds-ss-state      state)

      ;; Update the dead buffer.
      (mds-ss-send-client (format "mdc:-Debugger:-ShowstatAddr(%s,'dead')" addr)))))

;;}}}
;;{{{ (*) mds-ss-insert-proc

(defun mds-ss-insert-proc (buf proc)
  "Insert into the showstat buffer, BUF, the Maple procedure PROC.
PROC is the output of a call to showstat.  Use and update
the buffer-local variables `mds-ss-state' and `mds-ss-statement'."

  (with-current-buffer buf

    (let ((buffer-read-only nil))
      ;; Delete old contents then insert the new.
      (erase-buffer)
      (insert proc)

      ;; Hide the address and assign the client addr
      (goto-char (point-min))
      (let ((addr-procname (mds-out-activate-addr-procname)))
	(setq mds-ss-addr (car addr-procname)
	      mds-ss-procname (cdr addr-procname)))

      ;; Update the mode-line; this adds the procname to the mode-line
      (mds-ss-set-mode-line mds-ss-procname (car (mds-client-id mds-client)))

      (cond
       ((not mds-ss-dead-flag)
	;; Move the state arrow
	;; FIXME: only do if necessary
	(mds-ss-move-state mds-ss-state))

       ;; From here down, we are in the dead ss-buf

       (mds-ss-state
	(mds-ss-move-state mds-ss-state))

       ((string= "" mds-ss-statement)
	(setq mds-ss-state "1")
	(mds-ss-move-state "1"))

       (t
	(let ((state (mds-ss-determine-state mds-ss-statement)))
	  (when (null state)
	    (ding)
	    (message "cannot find statement in procedure body"))
	  ;; save state and clear statement
	  (setq mds-ss-state state)
	  mds-ss-statement "")
	;; Move the state arrow
	(mds-ss-move-state mds-ss-state))))))

;;}}}
;;{{{ (*) mds-ss-move-state

(defun mds-ss-move-state (state)
  "Move the overlay arrow in the showstat buffer to STATE.
Move POINT to the indentation of the current line.
If the `hl-line' feature is present in this session,
highlight the line."
  (if state
      (let ((buffer-read-only nil))
	;; Find the location of STATE in the buffer.
	(goto-char (point-min))
	(when (re-search-forward (concat "^ *" state "[ *?]\\(!\\)?") nil 't)
	  ;; Remove the bang, which showstat uses to mark the current state.
	  (if (match-string 1)
	      (replace-match " " nil nil nil 1))
	  ;; Move the arrow marker to the left margin of the state.
	  (beginning-of-line)
	  (or mds-ss-arrow-position
	      (setq mds-ss-arrow-position (make-marker)))
	  (set-marker mds-ss-arrow-position (point))
	  ;; If `hl-line' is enabled, highlight the line.
	  (when (featurep 'hl-line)
	    (cond
	     (global-hl-line-mode
	      (global-hl-line-highlight))
	     ((and hl-line-mode hl-line-sticky-flag)
	      (hl-line-highlight))))
	  ;; Move point to indentation of the current line (not including the state number).
	  (re-search-forward "^ *[1-9][0-9]*[ *?]? *" nil 'move)))))

;;}}}
;;}}}
;;{{{ select maple expressions

(defun mds-expr-at-point-interactive (prompt &optional default complete)
  "Request Maple identifier in minibuffer, using PROMPT.
Default is identifier around point.  If it is empty use DEFAULT.
Minibuffer completion is used if COMPLETE is non-nil."
  ;; Suppress error message
  (if (not default) (setq default t))
  (let ((enable-recursive-minibuffers t)
	(expr (mds-expr-at-point default))
        choice)
    (setq prompt (concat prompt (unless (string-equal expr "")
                                  (concat " [" expr "]"))
                         ": ")
          choice (if complete
                     (completing-read prompt 'maplev--completion
                                      nil nil nil maplev-history-list expr)
                   (read-string prompt nil maplev-history-list expr)))
    ;; Are there situations where we want to suppress the error message??
    (if (string-equal choice "")
        (error "Empty choice"))
    choice))

(defun mds-expr-at-point (&optional default)
  "Return the expression at point; if nothing use DEFAULT.
This is specialized to work with a few routines; needs to be generalized."
  (cond
   ((looking-at " *\\(?:el\\)?if \\(.+?\\) then")
    (format "evalb(%s)" (match-string-no-properties 1)))
   ((looking-at " *for \\([^ ]+\\)")
    (match-string-no-properties 1))
   ;; return expression
   ((looking-at " *return\\> *\\(\\s(?\\)\\(.*\\)")
    (if (not (string= (match-string-no-properties 1) ""))
	;; matched an opening parenthesis (of some variety)
	(buffer-substring-no-properties
	 (1- (match-end 1))
	 (save-excursion
	   (forward-list)
	   (point)))
      (match-string-no-properties 2)))
   ((looking-at (concat " *for " mds-re-symbol " in \\(.*\\) \\(?:do\\|while\\)"))
    (match-string-no-properties 1))
   ;; get lhs of an assignment
   ((looking-at "\\([^:\n]+\\(?::[^=][^:\n]+\\)*\\):=")
    (match-string-no-properties 1))
   (t (if (looking-at "\\s-+")
	  ;; move forward through empty space
	  (goto-char (match-end 0)))
      (maplev--ident-around-point default))))

;;}}}

;;{{{ allow input

(defun mds-ss-block-input ()
  "If input is allowed block further input.
Otherwise raise error indicating Maple is not available."
  (if (or (not mds-wait-until-ready-flag)
	  (mds-client-get-allow-input mds-client))
      (mds-client-set-allow-input mds-client nil)
    (beep)
    (message "Maple busy or debugging finished")))

(defun mds-toggle-wait-until-ready ()
  "Toggle the configuration variable `mds-wait-until-ready-flag'."
  (interactive)
  (setq mds-wait-until-ready-flag (not mds-wait-until-ready-flag)))

;;}}}

;;{{{ functions

(defun mds-ss-get-embedded-addr ()
  "Return the (hidden) address of the current procedure."
  (save-excursion
    (goto-char (point-min))
    (if (looking-at mds-re-addr-procname)
	(match-string-no-properties 2))))

(defun mds-ss-get-state ()
  "Return the statement number in the line at point."
  (save-excursion
    (end-of-line)
    (and (re-search-backward "^ *\\([1-9][0-9]*\\)\\([ *?]\\)" nil t)
       (match-string-no-properties 1))))

;;}}}

;;{{{ commands

;; Define the interactive commands bound to keys

;;{{{ (*) Execution

(defun mds-call-stack (&optional depth)
  "Execute the function at DEPTH on the call stack.
If optional DEPTH is nil, use 1, the top of the stack."
  (interactive "p")
  (let ((depth (or depth 1)))
    (if (< depth 1)
	(error "Stack depth must be positive")
      (mds-ss-eval-expr (format "mdc:-Debugger:-CallStack(%d,debugopts('callstack'))" depth)
			(format "callstack(%d)" depth)))))
		    

(defun mds-cont ()
  "Send the 'cont' (continue) command to the debugger."
  (interactive)
  (mds-ss-eval-proc-statement "cont" 'save))

(defun mds-goto-procname (flag)
  "Goto (stopat) a procedure without stopping.
If FLAG is non-nil, prompt for the procedure name.  If Transient
Mark mode is enabled and the mark is active, the default is the
marked region, otherwise the default is the name at or near
point.  If FLAG is nil, use the default without prompting.

The command sent to Maple (_enter name) uses skipping.  While
Maple executes the debugged code, the debugger searches for a
match of the current procname with the chosen name.  This can
fail if the name of the target procedure has been reassigned.
Consider, for example, the Maple code

   bar := proc(x) x^2 end proc:
   foo := bar:
   main := proc() foo(3); end proc:

If, inside main, you call `mds-goto-procname` with foo as the target,
it will fail since the actual name of foo is bar.

The Maple debugger sets a goback point at the current state so
that debugging can be resumed if no match occurs and the debugger
exits."

  (interactive "P")
  (let ((proc (cond
	       ((use-region-p)
		(buffer-substring-no-properties (mark) (point)))
	       ((looking-at (concat "return \\|.* := "))
		(save-excursion
		  (goto-char (match-end 0))
		  (thing-at-point 'procname)))
	       (t (thing-at-point 'procname)))))
    (if (or flag
	    ;; check for reserved words, builtins, etc
	    (member proc maplev-reserved-words)
	    (member proc maplev-builtin-functions)
	    (member proc maplev-special-words)
	    (member proc maplev-initial-variables)
	    (beep) t)
	(setq proc (read-string (format "procedure [%s]: " (or proc "")) nil nil proc)))
    (message "Stop in procedure %s..." proc)
    (mds-ss-eval-proc-statement (format "_mds_enter %s" proc))))

(defun mds-here (cnt)
  "Skip until the statement at point is reached CNT times."
  (interactive "p")
  (message "Skipping to point...")
  (mds-ss-eval-proc-statement (format "_mds_here %d %s %s"
				      cnt
				      (mds-ss-get-embedded-addr)
				      (mds-ss-get-state))))


(defun mds-into ()
  "Send the 'into' command to the debugger."
  (interactive)
  (mds-ss-eval-proc-statement "into" 'save))

(defun mds-next ()
  "Send the 'next' command to the debugger."
  (interactive)
  (mds-ss-eval-proc-statement "next" 'save))

(defun mds-outfrom ()
  "Send the 'outfrom' command to the debugger."
  (interactive)
  (mds-ss-eval-proc-statement "outfrom" 'save))

(defun mds-quit ()
  "If in the live showstat buffer, send the 'quit' command to the debugger.
Otherwise delete the dead showstat window."
  (interactive)
  (if mds-ss-dead-flag
      (delete-window (get-buffer-window (mds-client-dead-buf mds-client)))
    (mds-ss-eval-proc-statement "quit")))

(defun mds-return ()
  "Send the 'return' command to the debugger."
  (interactive)
  (mds-ss-eval-proc-statement "return" 'save))

(defun mds-skip ()
  "Resume skipping."
  (interactive)
  (message "Skipping...")
  (mds-ss-eval-proc-statement "_mds_skip"))

(defun mds-step ()
  "Send the 'step' command to the debugger."
  (interactive)
  (mds-ss-eval-proc-statement "step" 'save))

(defun mds-select-trace ()
  "Select the tracing state.
If tracing is enabled, use showstat rather than lineinfo buffer
to display the code."
  (interactive)
  (let ((state (ido-completing-read
		"select trace state: " '("none" "cont" "next" "into" "step" "_skip")
		nil 'require-match)))
    (if (string= state "none")
       (setq state nil)
      (if (mds-client-use-lineinfo-p mds-client)
	  (mds-wm-toggle-code-view)))
    (if (string= state "_skip")
	(setq state "_mds_skip"))
    (mds-client-set-trace mds-client state)))

;;}}}
;;{{{ (*) Stop points

(defvar mds-ss-stoperror-history-list '("all" "traperror")
  "History list used by stoperror.")

(defvar mds-ss-stopwhen-history-list nil
  "History list used by stopwhen.")

(defun mds--query-stop-var (cmd type hist)
  "Prompt the user with \"CMD [TYPE]: \", using history list HIST."
  (read-from-minibuffer (format "%s [%s]: " cmd type)
			nil nil nil nil hist))

(defun mds-breakpoint ()
  "Set a breakpoint at the statement at point."
  (interactive)
  (save-excursion
    (let ((state (mds-ss-get-state))
	  (inhibit-read-only t))
      (if state
	  (progn
	    ;; Replace the decoration
	    (replace-match "*" nil nil nil 2)
	    (mds-ss-eval-debug-code (format "debugopts('stopat'=[pointto(%s),%s])"
					    (mds-ss-get-embedded-addr)
					    state)
				    'hide))
	(ding)
	(message "could not find state in buffer")))))

(defun mds-breakpoint-cond ()
  "Set a conditional breakpoint at the current/previous state."
  (interactive)
  ;; Assume we are in the showstat buffer
  (save-excursion
    (end-of-line)
    (if (re-search-backward "^ *\\([1-9][0-9]*\\)\\([ *?]\\)" nil t)
	(let ((state (match-string-no-properties 1))
	      (inhibit-read-only t)
	      (cnd (mds--query-stop-var "stopat-cond" "condition" 'mds-ss-stopwhen-history-list)))
	  (replace-match "?" nil nil nil 2)
	  (mds-ss-eval-debug-code
	   (format "debugopts('stopat'=[pointto(%s),%s,%s])"
		   (mds-ss-get-embedded-addr) state cnd) 'hide))
      (ding)
      (message "no previous state in buffer"))))

(defun mds-stoperror (clear)
  "Query for and set or clear, if CLEAR is non-nil, a watchpoint on an error."
  (interactive "P")
  (let* ((cmd (if clear "unstoperror" "stoperror"))
	 (err (mds--query-stop-var cmd "errMsg" 'mds-ss-stoperror-history-list)))
    (mds-ss-eval-debug-code (format "%s %s" cmd err))))

(defun mds-stoperror-clear ()
  "Query for and clear a watchpoint on an error."
  (interactive)
  (mds-stoperror 'clear))

(defun mds-stopwhen-local (clear)
  "Set or clear, if CLEAR is non-nil, watchpoint on a variable.
Query for local variable, using symbol at point as default."
  (interactive "P")
  (let* ((cmd (if clear "unstopwhen" "stopwhen"))
	 (var (mds--query-stop-var cmd "var" 'mds-ss-stopwhen-history-list)))
    (if (string= var "")
	(mds-ss-eval-expr cmd)
      (mds-ss-eval-debug-code (format "debugopts('%s'=[pointto(%s),'%s'])"
				      (if clear "delwatch" "addwatch")
				      (mds-client-get-addr mds-client) var) 'hide))))

(defun mds-stopwhen-global (clear)
  "Set or clear, if CLEAR is non-nil, watchpoint on a variable.
Query for global variable, using symbol at point as default."
  (interactive "P")
  (let* ((cmd (if clear "unstopwhen" "stopwhen"))
	 (var (mds--query-stop-var cmd "var" 'mds-ss-stopwhen-history-list)))
    (if (string= var "")
	(mds-ss-eval-debug-code cmd)
      (mds-ss-eval-debug-code (format "%s %s" cmd var)))))

(defun mds-stopwhenif ()
  "Query and set a conditional watchpoint on a global variable."
  (interactive)
  (let* ((cmd "stopwhenif")
	 (var (mds--query-stop-var cmd "var" 'mds-ss-stopwhen-history-list))
	 (val (read-string "value: ")))
    (mds-ss-eval-expr (format "%s(%s,%s)" cmd var val))))

(defun mds-stopwhen-clear ()
  "Query and clear a watchpoint on a variable."
  (interactive)
  (mds-stopwhen-global 'clear))

(defun mds-unstopat ()
  "Clear a breakpoint at the current state.
If the state does not have a breakpoint, print a message."
  (interactive)
  (save-excursion			; this does no good ...
    (end-of-line)
    (if (and (re-search-backward "^ *\\([1-9][0-9]*\\)\\(\\*?\\)" nil t)
	     (string= (match-string 2) "*"))
	(let ((state (match-string-no-properties 1))
	      (inhibit-read-only t))
	  (replace-match " " nil nil nil 2)
	  (mds-ss-eval-debug-code
	   (format "debugopts('stopat'=[pointto(%s),-%s])"
		   (mds-ss-get-embedded-addr) state)
	   'hide))
      (ding)
      (message "no breakpoint at this state"))))

;;}}}
;;{{{ (*) Evaluation

(defun mds-eval-and-prettyprint ()
  "Query for an expression and pretty-print it in the output buffer.
The default is taken from expression at point.  The Maple
procedure mdc:-Format:-PrettyPrint is used to break the
expression into multiple lines.  If called with prefix argument,
allow return expression of unlimited size."
  (interactive)
  (let ((expr (mds-expr-at-point-interactive
	       "prettyprint: " "")))
    (mds-ss-eval-expr (format "mdc:-Format:-PrettyPrint(%s)" expr) t current-prefix-arg)))

(defun mds-eval-and-prettyprint-prev ()
  "Call `mds-eval-and-prettyprint' with point at the preceding statement."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (re-search-backward mds-re-ss-statement nil t)
	(progn
	  (goto-char (match-beginning 3))
	  (let ((expr (mds-expr-at-point)))
	    (mds-ss-eval-expr (format "mdc:-Format:-PrettyPrint(%s)" expr) t current-prefix-arg)))
      (beep)
      (message "No preceding statement."))))

(defun mds-eval-and-display-expr (expr)
  "Evaluate a Maple expression, EXPR, display result.
If called interactively, EXPR is queried.  If called with prefix
argument, allow return expression of unlimited size."
  (interactive (list (mds-expr-at-point-interactive
		      "eval: " "")))
  (mds-ss-eval-expr expr t current-prefix-arg))

(defun mds-eval-and-display-expr-global (expr)
  "Evaluate a Maple expression, EXPR, in a global context.
If called interactively, EXPR is queried.  If called with prefix
argument, allow return expression of unlimited size."
  (interactive (list (mds-expr-at-point-interactive
		      "global eval: " "")))
  (mds-ss-eval-expr (concat "statement " expr) t current-prefix-arg))

;;}}}
;;{{{ (*) Information

(defun mds-args ()
  "Display the arguments of the current procedure."
  (interactive)
  (mds-ss-eval-debug-code "args"))

(defun mds-show-args-as-equations ()
  "Display the parameters and arguments of the current Maple procedure as equations."
  (interactive)
  (mds-ss-block-input)
  (if current-prefix-arg (mds-out-clear))
  (mds-out-append-input (mds-client-out-buf mds-client) "Args:" 'mds-args)
					; We need to use a global variable for the index,
					; one that isn't likely to appear in an expression.
					; Alternatively, a module export could be used.
  (mds-ss-send-client (format "mdc:-Format:-ArgsToEqs(%s, [seq([_params[`_|_`]],`_|_`=1.._nparams)],[_rest],[_options])\n"
			      (mds-client-get-addr mds-client))))

(defun mds-showstack ()
  "Send the 'showstack' command to the debugger.
Display the stack content in the output buffer.  Each entry
consists of the hyperlinked name of the calling procedure."
  (interactive)
  (mds-ss-eval-debug-code "showstack"))

(defun mds-showstop ()
  "Send the 'showstop' command to the debugger."
  (interactive)
  (mds-ss-eval-debug-code "showstop"))

(defun mds-showerror (fmt)
  "Send the 'showerror' command to the debugger.
If FMT (prefix arg) is non-nil, display the formatted message,
otherwise hyperlink the raw message."
  (interactive "P")
  (if fmt
      (mds-ss-eval-debug-code "mdc:-Debugger:-ShowError()")
    (mds-ss-block-input)
    (mds-ss-send-client "showerror\n")))

(defun mds-showexception (raw)
  "Send the 'showexception' command to the debugger.
If RAW (prefix arg) is non-nil, display the raw output,
otherwise run through StringTools:-FormatMessage."
  (interactive "P")
  (if (not raw)
      (mds-ss-eval-debug-code "mdc:-Debugger:-ShowException()")
    (mds-ss-block-input)
    (mds-ss-send-client "showexception\n")))

(defun mds-where (&optional depth)
  "Send the 'where' command to the debugger.
Display the stack content in the output buffer.  Each entry
consists of the hyperlinked name of the calling procedure,
followed by the statement from the procedure that was called,
followed by the list of arguments.  The optional DEPTH parameter
is a positive integer that specifies the number of activation
levels to display.  Use `mds-showstack' to see just the
hyperlinks."
  (interactive "P")
  (let ((cmd (if depth
		 (format "where %d" depth)
	       "where")))
    (mds-ss-eval-debug-code cmd)))

;;}}}
;;{{{ (*) Monitoring

(defun mds-monitor-toggle ()
  "Toggle the monitoring feature.
Monitoring provides a continuous display of specified Maple expressions.
See `mds-monitor-define'."
  (interactive)
  (mds-ss-eval-proc-statement "_mds_monitor toggle"))

(defun mds-monitor-define (all)
  "Define a monitor expression for the current procedure.
If ALL is non-nil, the monitor expression applies to all procedures.
The user is queried for the expression in the minibuffer.  The
expression must be valid Maple.  An empty string, or whitespace,
removes the monitor expression for the current procedure.

If ALL is non-nil, the monitored expression is used with all
procedures, otherwise it is used with just the current procedure
in the showstat buffer; it will only be displayed when that
procedure is active.  Expressions can be defined for multiple
procedures.  Only one expression is used with all procedure.
See `mds-monitor-toggle'."
  (interactive "P")
  (let ((expr (read-string (format "%smonitor expr: "
				   (if all "[all] " ""))))
	(addr (if all "0" (mds-client-get-addr mds-client))))
    (mds-ss-eval-proc-statement (format "_mds_monitor define %s %s" addr expr))))

;;}}}
;;{{{ (*) Short cuts

(defun mds-send-last-command ()
  "Reexecute the last command that executed code."
  (interactive)
  (let ((cmd (mds-client-get-last-cmd mds-client)))
    (if cmd
	(mds-ss-eval-proc-statement cmd)
      (ding)
      (message "no previous command"))))

;;}}}

;;{{{ (*) Miscellaneous

(defun mds-ss-refresh (&optional client)
  "Refresh the showstat buffer for CLIENT."
  (interactive)
  (unless client (setq client mds-client))
  (mds-ss-send-client (format "mdc:-Debugger:-ShowstatAddr(%s)" (mds-client-get-addr client))))

(defun mds-ss-goto-current-state (client)
    "Move point to current statement in live-showstat buffer of CLIENT.
Set cursor to ready."
    (mds-wm-select-code-window client)
    (mds-ss-move-state mds-ss-state)
    (setq cursor-type mds-cursor-ready))
    

(defun mds-goto-current-state (&optional client)
  "Move cursor to the current state in the code buffer for CLIENT."
  (interactive)
  (unless client (setq client mds-client))
  (if (and (mds-client-use-lineinfo-p client)
	   (mds-client-has-source-p client))
      (mds-li-goto-current-state client)
    (mds-wm-select-code-window client)
    (mds-ss-update (current-buffer)
		   (mds-client-get-addr client)
		   (mds-client-get-procname client)
		   (mds-client-get-state client)
		   (mds-client-get-statement client))))

(defun mds-goto-state (state)
  "Move POINT to STATE.
STATE is a string corresponding to an integer."
  ;; Assume we are in the showstat buffer.
  (goto-char (point-min))
  (unless (re-search-forward (concat "^ *" state "[ *?]\\s-*") nil t)
    (ding)
    (message "cannot find state %s" state)))

(defun mds-toggle-truncate-lines (output-buffer)
  "Toggle the truncation of long lines.
If OUTPUT-BUFFER is non-nil, do so in the `mds-out-buffer',
otherwise do so in the `mds-ss-buffer'."
  (interactive "P")
  (if output-buffer
      (with-current-buffer (mds-client-out-buf mds-client)
	(toggle-truncate-lines))
    (toggle-truncate-lines)))

(defun mds-help-debugger ()
  "Display the Maple help page for the tty debugger."
  (interactive)
  (maplev-help-show-topic "debugger"))

(defun mds-goback-save ()
  "Save statement at point as go-back point."
  (interactive)
  (message "set go-back point")
  (mds-ss-eval-proc-statement (format "_mds_goback_save %s %s"
				      (mds-ss-get-state)
				      (mds-ss-get-embedded-addr))))

(defun mds-info ()
  "Display the info page for MDS."
  (interactive)
  (info "mds"))

(defun mds-toggle-quiet ()
  "Toggle the quiet mode, which suppresses normal output of executed statements."
  (interactive)
  (mds-ss-eval-expr
   (format "mdc:-Debugger:-SetQuiet(%s)"
	   (if (mds-client-toggle-quiet-p mds-client)
	       (progn
		 (message "suppress output")
		 "true")
	     (message "enable output")
	     "false"))))

;;}}}

;;}}}
;;{{{ mode map

(defvar mds-ss-mode-map
  (let ((map (make-sparse-keymap))
	(bindings
	 '(
	   (" " . mds-send-last-command)
	   ("A" . mds-show-args-as-equations)
	   ("a" . mds-args)
	   ("b" . mds-breakpoint)
	   ("B" . mds-breakpoint-cond)
	   ("c" . mds-cont)
	   ("C" . mds-out-clear)
	   ("d" . self-insert-command)
	   ("e" . mds-eval-and-display-expr)
	   ("E" . mds-eval-and-display-expr-global)
	   ("f" . self-insert-command)
	   ("g" . mds-goto-procname)
	   ("G" . mds-goback-save)
	   ("h" . mds-here)
	   ("H" . mds-info)
	   ("i" . mds-into)
	   ("I" . mds-stopwhenif)
	   ("j" . mds-call-stack)
	   ("k" . mds-showstack)
	   ("K" . mds-where)
	   ("l" . mds-goto-current-state)
	   ("L" . mds-ss-refresh)
	   ("m" . mds-monitor-toggle)
	   ("M" . mds-monitor-define)
	   ("n" . mds-next)
	   ("o" . mds-outfrom)
	   ("p" . mds-showstop)
	   ("P" . mds-patch)
	   ("q" . mds-quit)
	   ("Q" . mds-toggle-quiet)
	   ("r" . mds-return)
	   ("R" . mds-stoperror)
	   ("s" . mds-step)
	   ("S" . mds-skip)
	   ("t" . mds-select-trace)
	   ("T" . mds-toggle-truncate-lines)
	   ("u" . mds-unstopat)
	   ("v" . mds-wm-toggle-code-view)
	   ("w" . mds-stopwhen-local)
	   ("W" . mds-stopwhen-global)
	   ("x" . mds-showexception)
	   ("X" . mds-showerror)
	   ("." . mds-eval-and-prettyprint)
	   ("," . mds-eval-and-prettyprint-prev)
	   )))
    (mapc (lambda (binding) (define-key map (car binding) (cdr binding)))
	  bindings)
    map))

;;}}}

;;{{{ mode-line

(defun mds-ss-set-mode-line (proc &optional label)
  "Set the mode-line of an mds-ss buffer.
PROC is a string corresponding to the displayed procedure,
it is displayed in square brackets after the mode name.
If LABEL is non-nil, insert it in brackes, [LABEL]."
  (setq mode-line-format
	(list
	 mode-line-buffer-identification
;;	 "   "
;;	 mode-line-modes
	 (and label (concat "   " (propertize (format "[%s]" label) 'face 'bold)))
	 "   "
	 (propertize (format "[%s]" proc) 'face 'bold)
	 "-%-")))

(defun mds-ss-modeline-hilite (buf &optional off)
  "Turn the highlighting of the mode-line in the showstat buffer BUF on or off.
If OFF is non-nil, then turn off, otherwise turn on.  The
highlighting only applies when the buffer is inactive (that might
change).  The purpose is to distinguish the window when
multiwindows are present and the control panel is used.  For this
to work, `face-remapping-alist' must be buffer-local."
  (with-current-buffer buf
    (setq face-remapping-alist
	  (if off
	      `((mode-line-inactive
		 :foreground ,(face-attribute 'mode-line-inactive :foreground t)
		 :background ,(face-attribute 'mode-line-inactive :background t)))
	    `((mode-line-inactive
	       ;; make customizable
	       :foreground ,(face-attribute 'mode-line :foreground t)
	       :background ,(face-attribute 'mode-line :background t)))
	    ))))

;;}}}

;;{{{ menu

(defvar mds-ss-menu nil)
(unless mds-ss-menu
  (easy-menu-define
    mds-ss-menu mds-ss-mode-map
    "Menu for mds showstat mode"
    `("Showstat"

      ("Execution"
       ["Continue"	mds-cont t]
       ["Into"		mds-into t]
       ["Next"		mds-next t]
       ["Outfrom"	mds-outfrom t]
       ["Skip"          mds-skip t]
       ["Step"		mds-step t]
       ["Return"	mds-return t]
       "----"
       ["Goto"		mds-goto-procname t]
       ["Goto [query]"  mds-goto-procname :keys "C-u g"]
       ["Here"		mds-here t]
       ["Re-execute"    mds-call-stack t]
       "----"
       ["Select trace"	mds-select-trace t]
       "----"
       ["Quit"		mds-quit t]
       )

      ("Stop points"
       ["Set breakpoint at point"    mds-breakpoint t]
       ["Clear breakpoint at point"  mds-unstopat t]
       ["Clear conditional breakpoint at point"  mds-breakpoint-cond t]
       "----"
       ["Set global watchpoint"      mds-stopwhen-global t]
       ["Set local watchpoint"       mds-stopwhen-local t]
       ["Set conditional watchpoint" mds-stopwhenif t]
       ["Clear watchpoint"           mds-stopwhen-clear :keys "C-u w"]
       "----"
       ["Set watchpoint on error"    mds-stoperror t]
       ["Clear watchpoint on error"  mds-stoperror-clear :keys "C-u R"]
       "----"
       ["Show all stop points"       mds-showstop t]
       "---"
       ["Save goback point"          mds-goback-save t]
       )

      ("Evaluation"
       ["Evaluate expression"			mds-eval-and-display-expr t]
       ["Evaluate expression in global context" mds-eval-and-display-expr-global t]
       ["Prettyprint prettyprint expression"	mds-eval-and-prettyprint t]
       ["Prettyprint expression from previous line"	mds-eval-and-prettyprint-prev t]
       )

      ("Information"
       ["Display parameters and values" mds-show-args-as-equations t]
       ["Show stack"			mds-showstack t]
       ["Show stack with arguments"	mds-where t]
       ["Show error"			mds-showerror t]
       ["Show error raw"		(mds-showerror t) t]
       ["Show exception"		mds-showexception t]
       ["Show exception raw"		(mds-showexception t) t]
       "---"
       ["Toggle monitoring"		mds-monitor-toggle t]
       ["Define monitor expression" 	mds-monitor-define t]
       )

      ("Miscellaneous"
       ["Clear output buffer"           mds-out-clear t]
       ["Patch procedure"               mds-patch t]
       ["Refresh procedure"             mds-ss-refresh t]
       ["Toggle code view"              mds-wm-toggle-code-view t]
       ["Toggle input tracking"         mds-toggle-track-input t]
       ["Toggle mds-wait-until-ready-flag" mds-toggle-wait-until-ready t]
       ["Toggle output of executed statements" mds-toggle-quiet t]
       ["Toggle mds-stop-trace-at-trapped-error-flag" mds-toggle-stop-trace-at-trapped-error-flag t]
       ["Toggle truncate lines"         mds-toggle-truncate-lines t]
       ["Write output buffer"           mds-out-write-buffer t]
       )

      ("Clients"
       ["Cycle clients"                 mds-wm-cycle-clients t]
       ["Cycle groups"                  mds-wm-cycle-groups  t]
       )

      ("Help"
       ["Help Maple debugger"      mds-help-debugger t]
       ["Info for Mds mode"        mds-info t]
       ["Version"                  mds-version]
       )

      )))

;;}}}

;;{{{ ss-mode

(define-derived-mode mds-ss-mode maplev-view-mode "showstat-mode"
  "Major mode for stepping through a debugged Maple procedure.

The following summarizes the key bindings.  The parenthesized
command after a key is the corresponding Maple debugger command.
\\<mds-ss-mode-map>
Execution
---------
\\[mds-send-last-command] repeat the last tracing command
\\[mds-cont] (cont) continue execution until next stop point
\\[mds-next] (next) execute next statement at current nesting level
\\[mds-into] (into) execute next statement at any level in current procedure
\\[mds-outfrom] (outfrom) execute current statement sequence or until stop point
\\[mds-step] (step) execute next statement at any level
\\[mds-skip] resume skipping
\\[mds-return] (return) continue executing until current procedure returns
\\[mds-select-trace] select trace mode
\\[mds-quit] (quit) terminate debugging, return to mds buffer

Stop points
-----------
\\[mds-breakpoint] (stopat) set breakpoint at cursor
\\[mds-unstopat] (unstopat) clear breakpoint at cursor
\\[mds-breakpoint-cond] (stopat-cond) set conditional breakpoint at cursor

\\[mds-stopwhenif] (stopwhenif) set watchpoint on variable = value

\\[mds-stopwhen-local] (stopwhen) set watchpoint on local variable
C-u \\[mds-stopwhen-local] (stopwhen) clear watchpoint on local variable
\\[mds-stopwhen-global] (stopwhen) set watchpoint on global variable
C-u \\[mds-stopwhen-global] (stopwhen) clear watchpoint on global variable

\\[mds-showstop] (showstop) display all breakpoints

Information
-----------
\\[mds-show-args-as-equations] display the parameter names and values
\\[mds-args] display the arguments of the current procedure
\\[mds-help-debugger] display Maple debugger help page
\\[mds-showstack] (showstack) display abbreviated stack
\\[mds-where] (where) display stack of procedure calls
\\[mds-showerror] display the last error
C-u \\[mds-showerror] display the last error (raw)
\\[mds-showexception] display the last exception
C-u \\[mds-showexception] display the last exception (raw)

Evaluation
----------
\\[mds-eval-and-display-expr] evaluate a Maple expression
C-u \\[mds-eval-and-display-expr] clear output then evaluate a Maple expression
\\[mds-eval-and-display-expr-global] evaluate a Maple expression in a global context
\\[mds-eval-and-prettyprint] evaluate and prettyprint a Maple expression
C-u \\[mds-eval-and-prettyprint] clear output then evaluate and prettyprint a Maple expression

Miscellaneous
-------------
\\[mds-goto-current-state] move (return) cursor to current state
\\[mds-out-clear] clear the debugger output buffer
\\[mds-help-debugger] display help page for the Maple debugger
\\[mds-info] display info pages for the Maple debugger
\\[maplev-help-at-point] display a Maple help page
\\[maplev-proc-at-point] display a Maple procedure
\\[mds-wm-toggle-code-view] toggle view of code between showstat and line-info
\\[mds-toggle-quiet] toggle display of output of executed statements
\\[mds-toggle-truncate-lines] toggle whether to fold or truncate long lines
C-u \\[mds-toggle-truncate-lines] toggle truncation in debugger output buffer
\\[mds-patch] patch procedure in the buffer
\\[mds-ss-refresh] refresh procedure in the buffer
"
  :group 'mds
  :abbrev-table nil

  (setq mds-ss-procname ""
	mds-ss-state ""
	mds-ss-arrow-position nil)

  (and mds-ss-menu (easy-menu-add mds-ss-menu))

  ;; Used by control-panel to set the mode-line of the "active" buffer
  (make-local-variable 'face-remapping-alist)

  (add-hook 'kill-buffer-hook '(lambda () (setq mds-update-showstat-p t)) nil 'local))

;;}}}

(provide 'mds-ss)

;;; mds-ss.el ends here
