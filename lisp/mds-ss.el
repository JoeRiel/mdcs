;;; mds-ss.el --- mds-ss-mode

;; Copyright (C) 2011 Joseph S. Riel, all rights reserved

;; Author:     Joseph S. Riel <jriel@maplesoft.com>
;; Created:    May 2011
;; Keywords:   maple, debugger
;;
;;; Commentary:

;; This file contains the source for the showstat functions.

;;; Code:

(eval-when-compile
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
A more precise regular expression would allow spaces inside backquotes,
however, such an abomination should break something.")

;;}}}
;;{{{ variables

(defvar mds-ss-arrow-position nil "Marker for state arrow.")
(defvar mds-ss-show-args-flag nil "Non-nil means show args when entering a procedure.")
(defvar mds-ss-dead-flag      nil "Non-nil means this is the ss-dead buffer.")
(defvar mds-ss-procname       nil "Name of displayed showstat procedure.")
(defvar mds-ss-state          "1" "Current state of procedure.")
(defvar mds-ss-statement      ""  "String matching a statement; used by dead buffer.")

;; Make variables buffer-local
(mapc #'make-variable-buffer-local
      '(mds-client
	mds-ss-arrow-position
	mds-ss-show-args-flag
	mds-ss-dead-flag
	mds-ss-procname
	mds-ss-result
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
  (mds-ss-send-client (concat cmd "\n")))

(defun mds-ss-eval-expr (expr &optional display)
  "Send EXPR, with newline, to the Maple process DISPLAY to the output buffer.
If DISPLAY is nil, send EXPR to the output buffer.  This function
is intended to be used for evaluating Maple expressions."
  (mds-ss-block-input)
  (mds-out-append-input (mds-client-out-buf mds-client) (or display expr) 'mds-user-input)
  (mds-ss-send-client (concat expr "\n")))

(defun mds-ss-eval-proc-statement (cmd &optional save)
  "Send CMD, with appended newline, to the Maple process and to
the output buffer, tagged as 'cmd.  If SAVE is non-nil, then save
it.  Change cursor type to `mds-cursor-waiting', which indicates
we are waiting for a response from Maple.  This function assumes
we are in the appropriate `mds-ss-buffer'.  This function is to
be used with commands that cause Maple to execute procedural
code."
  (mds-ss-block-input)
  (if save (mds-client-set-last-cmd mds-client cmd))
  (setq cursor-type mds-cursor-waiting)
  (unless (eobp) (forward-char)) ; this indicates 'waiting' in tty Emacs, where cursor doesn't change
  (mds-out-display (mds-client-out-buf mds-client) cmd 'cmd)
  (mds-ss-send-client cmd))

(defun mds-ss-request (expr)
  "Send the string EXPR to Maple and return the response, as a string.
A newline is appended to EXPR before it is sent.  EXPR should
have no spaces."
  (mds-client-set-result mds-client nil)
  (mds-client-send mds-client (format "_mds_request %s\n" expr))
  (let (result)
    ;; Loop until the result is returned.
    (while (null result)
      (sleep-for 0.0001)
      (setq result (mds-client-get-result mds-client)))
    (substring result 0 -1)))

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
	    buffer-read-only 't)
      (if mds-truncate-lines
	  (toggle-truncate-lines 1)))
    buf))

;;}}}
;;{{{ (*) mds-ss-update

(defun mds-ss-update (buf addr procname state &optional statement)
  "Update the showstat buffer BUF, the client field addr, and
the buffer local variables `mds-ss-procname', and `mds-ss-state'.
ADDR is the address of PROCNAME, which is the name of the
procedure, STATE is the current state; all are strings.  If the
buffer is already displaying PROCNAME, then just move the arrow;
otherwise call (maple) showstat to display the new procedure."

  (with-current-buffer buf
    (let ((trace (mds-client-get-trace mds-client)))
      (unless trace
	;; Revert cursor-type to ready status.
	(setq cursor-type mds-cursor-ready))
      
      (if (string= addr (mds-client-get-addr mds-client))
	  ;; Address has not changed; move the arrow
	  (unless trace
	    (mds-ss-move-state state))
	
	;; New procedure; send address and procname to the output buffer.
	(mds-out-display (mds-client-out-buf mds-client)
			 (format "<%s>\n%s" addr procname)
			 'addr-procname)
	
	(unless trace
	  ;; Call Maple showstat routine to update the showstat buffer.
	  (mds-ss-send-client (format "mdc:-Debugger:-ShowstatAddr(%s)" addr)))
	(when (and mds-show-args-flag
		   (string= state "1"))
	  (setq mds-ss-show-args-flag t))))

    ;; Update the buffer-local status
    (mds-client-set-addr mds-client addr)
    (setq mds-ss-procname procname
	  mds-ss-state    state
	  mds-ss-statement statement)))

;;}}}
;;{{{ (*) mds-ss-determine-state

(defun mds-ss-determine-state (statement)
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
	(while (re-search-forward mds-ss-statement-re nil 'move)
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
	    (re-search-forward mds-ss-statement-re)
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
      (if (string= addr (mds-client-get-addr mds-client))
	  ;; Already displaying the procedure; just update the arrow.
	  (mds-ss-move-state (or state
				    (mds-ss-determine-state statement)))

	;; Need to fetch from Maple.
	;; Set the buffer locals state info.
	(mds-client-set-addr mds-client addr)
	(setq mds-ss-procname   procname
	      mds-ss-statement  statement
	      mds-ss-state      state)
	
	;; Update the dead buffer.
	(mds-ss-send-client (format "mdc:-Debugger:-ShowstatAddr(%s,'dead')" addr))))))

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
      (let ((addr-procname (mds-activate-addr-procname)))
	(mds-client-set-addr mds-client (car addr-procname))
	(setq mds-ss-procname (cdr addr-procname)))

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
	(maplev-completion-release maplev-release)
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
  "Return the expression at point.
This is specialized to work with a few routines; needs to be generalized."
  (cond
   ((looking-at " *\\(?:el\\)?if \\(.+?\\) then")
    (format "evalb(%s)" (match-string-no-properties 1)))
   ((looking-at " *for \\([^ ]+\\)")
    (match-string-no-properties 1))
   ((looking-at " *return \\(.*\\);?$")
    (match-string-no-properties 1))
   ((looking-at (concat " *for " mds--symbol-re " in \\(.*\\) \\(?:do\\|while\\)"))
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
  (if (and mds-wait-until-ready
	   (not (mds-client-get-allow-input mds-client)))
      (error "Maple busy or debugging finished")
    (mds-client-set-allow-input mds-client nil)))

(defun mds-toggle-wait-until-ready ()
  "Toggle the configuration variable `mds-wait-until-ready'."
  (interactive)
  (setq mds-wait-until-ready (not mds-wait-until-ready)))

;;}}}

;;{{{ functions

(defun mds-ss-get-embdedded-addr ()
  "Return the (hidden) address of the current procedure."
  (save-excursion
    (goto-char (point-min))
    (if (looking-at mds--addr-procname-re)
	(match-string-no-properties 2))))

(defun mds-ss-get-state ()
  "Return the statement number in the line at point."
  (save-excursion
    (end-of-line)
    (and (re-search-backward "^ *\\([1-9][0-9]*\\)\\([ *?]\\)" nil t)
       (match-string-no-properties 1))))

(defun mds-ss--regexp-for-statement (keyword)
  "Return regexp that matches KEYWORD.
The regexp includes indentation, statement number and decoration,
given by the length of the string in the previous match-group 1."
  (concat "^"
	  "[ 0-9*?]\\{"
	  (number-to-string (length (match-string-no-properties 1)))
	  "\\}"
	  keyword
	  " "
	  ))

(defun mds-ss-beginning-of-statement ()
  "Move to beginning of statement at point and return statement number.
For a multi-part statement (if/elif/else, try/catch), the
beginning is the first keyword, but only when point is at one of the
keywords."
  (beginning-of-line)
  (cond
     ((looking-at "\\( *\\)el\\(if\\|se\\)\\>")
      ;; at 'elif|else': move back to 'if' at same indent
      (re-search-backward (mds-ss--regexp-for-statement "if")))
     ((looking-at "\\( *\\)catch[ :]")
      ;; at catch: move back to 'try' at same indent
      (re-search-backward (mds-ss--regexp-for-statement "try")))
     ((looking-at "\\( *\\)end \\([a-z]+\\)")
      (re-search-backward (mds-ss--regexp-for-statement (match-string-no-properties 2)))))
  (when (looking-at mds-ss-statement-re)
    (goto-char (match-beginning 3))
    (match-string-no-properties 1)))

;;}}}

;;{{{ commands

;; Define the interactive commands bound to keys

;;{{{ (*) Execution

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
	    (and
	     ;; check for reserved words, builtins, etc
	     (let ((release (mds-client-get-maple-release mds-client)))
	       (or (member proc (cdr (assoc release maplev--reserved-words-alist)))
		   (member proc (cdr (assoc release maplev--builtin-functions-alist)))
		   (member proc maplev--special-words)
		   (member proc maplev--initial-variables)))
	     (or (beep) t)))
	(setq proc (read-string (format "procedure [%s]: " (or proc "")) nil nil proc)))
    (message "Stop in procedure %s..." proc)
    (mds-ss-eval-proc-statement (format "_enter %s" proc))))

(defun mds-here (cnt)
  "Skip until the statement at point is reached CNT times."
  (interactive "p")
  (message "Skipping to point...")
  (mds-ss-eval-proc-statement (format "_here %d %s %s"
				      cnt
				      (mds-ss-get-embdedded-addr)
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
  (mds-ss-eval-proc-statement "_skip"))

(defun mds-step ()
  "Send the 'step' command to the debugger."
  (interactive)
  (mds-ss-eval-proc-statement "step" 'save))

(defun mds-cycle-trace ()
  "Cycle through the tracing states:
'nil', 'cont', 'next', 'into', 'level', and 'step'.
If nil is selected, tracing does not occur.  Otherwise, when the
debugger returns control to the server, the selected debugging
command is immediately executed.

To best use the results after tracing, turn off tracing
mode (select nil), then reenter the debugger from the client.
The hyperlinks in the output buffer are then active."
  (interactive)
  (let ((state (mds-client-get-trace mds-client)))
    (mds-client-set-trace mds-client
			  (cond
			   ((null state)           "cont")
			   ((string= state "cont") "next")
			   ((string= state "next") "into")
			   ((string= state "into") "level")
			   ((string= state "level") "step")
			   ((string= state "step") nil)))
    (message (concat "tracing " (or state "disabled")))))

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
					    (mds-client-get-addr mds-client)
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
	      (cond (mds--query-stop-var "stopat-cond" "condition" 'mds-ss-stopwhen-history-list)))
	  (replace-match "?" nil nil nil 2)
	  (mds-ss-eval-debug-code
	   (format "debugopts('stopat'=[pointto(%s),%s,%s])"
		   (mds-client-get-addr mds-client) state cond) 'hide))
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
    ;;    (if (string= var "")
    ;;	(error "stopwhenif requires a variable and a value")
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
	   (format "debugopts('stopat'=[pointto(%s),-%s])" (mds-client-get-addr mds-client) state) 'hide))
      (ding)
      (message "no breakpoint at this state"))))

;;}}}
;;{{{ (*) Evaluation

(defun mds-eval-and-prettyprint ()
  "Query for an expression and pretty-print it in the output buffer.
The default is taken from expression at point.  The Maple
procedure mdc:-Format:-PrettyPrint is used to break the expression into
multiple lines."
  (interactive)
  (let ((expr (mds-expr-at-point-interactive
	       "prettyprint: " "")))
    (mds-ss-eval-expr (format "mdc:-Format:-PrettyPrint(%s)" expr) expr)))

(defun mds-eval-and-prettyprint-prev ()
  "Move backward to previous statement and call `mds-eval-and-prettyprint'."
  (interactive)
  (save-excursion
    (let ((col (current-column)))
      (forward-line -1)
      (mds-ss-beginning-of-statement)
      (while (looking-at "end ")
	(forward-line -1)
	(mds-ss-beginning-of-statement))
      (let ((expr (mds-expr-at-point)))
      (mds-ss-eval-expr (format "mdc:-Format:-PrettyPrint(%s)" expr) expr)))))
				


(defun mds-eval-and-display-expr (expr &optional suffix)
  "Evaluate a Maple expression, EXPR, display result and print optional SUFFIX.
If called interactively, EXPR is queried."
  (interactive (list (mds-expr-at-point-interactive
		      "eval: " "")))
  (if current-prefix-arg (mds-out-clear))
  (mds-ss-eval-expr expr))


(defun mds-eval-and-display-expr-global (expr)
  "Evaluate a Maple expression, EXPR, in a global context.
If called interactively, EXPR is queried.
The result is returned in the message area."
  (interactive (list (mds-expr-at-point-interactive
		      "global eval: " "")))
  (if current-prefix-arg (mds-out-clear))
  (mds-eval-and-display-expr (concat "statement " expr)))

;;}}}
;;{{{ (*) Information

(defun mds-args ()
  "Display the arguments of the current procedure."
  (interactive)
  (mds-ss-eval-expr "args"))

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
Note that the string displayed in the echo area has the current
procedure stripped from it."
  (interactive)
  (mds-ss-eval-expr "showstack"))

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
The optional DEPTH parameter is a positive integer that specifies
the number of activation levels to display."
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
  (mds-ss-eval-proc-statement "_monitor toggle"))

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
	(addr (if all "0" (mds-ss-get-embdedded-addr))))
    (mds-ss-eval-proc-statement (format "_monitor define %s %s" addr expr))))

;;}}}
;;{{{ (*) Short cuts

(defun mds-send-last-command ()
  "Reexecute the last command that executes code."
  (interactive)
  (let ((cmd (mds-client-get-last-cmd mds-client)))
    (if cmd
	(mds-ss-eval-proc-statement cmd)
      (ding)
      (message "no previous command"))))

;;}}}

;;{{{ (*) Miscellaneous

(defun mds-ss-refresh ()
  "Refresh the showstat buffer."
  (interactive)
  (let ((cmd (format "mdc:-Debugger:-ShowstatAddr(%s)" (mds-client-get-addr mds-client))))
    (mds-client-set-addr mds-client "")
    (mds-ss-send-client cmd)
    (message "Refreshed showstat buffer")))

(defun mds-goto-current-state ()
  "Move cursor to the current state in the code buffer."
  (interactive)
  (if (and (mds-client-use-lineinfo-p mds-client)
	   (mds-client-has-source-p mds-client))
      (mds-li-goto-current-state mds-client)
    (pop-to-buffer (mds-client-live-buf mds-client))
    (mds-ss-update (current-buffer)
		   (mds-client-get-addr mds-client)
		   mds-ss-procname
		   mds-ss-state
		   mds-ss-statement)))

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
  "Save current statement as goto point."
  (interactive)
  (message "set goback point")
  (mds-ss-eval-proc-statement (format "_goback_save %s %s"
				      (mds-ss-get-state)
				      (mds-ss-get-embdedded-addr))))

(defun mds-info ()
  "Display the info page for MDS."
  (interactive)
  (info "mds"))

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
	   ("r" . mds-return)
	   ("R" . mds-stoperror)
	   ("s" . mds-step)
	   ("S" . mds-skip)
	   ("t" . mds-cycle-trace)
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
it is displayed in square brackets after the mode name."
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
       "----"
       ["Trace [cycle]" mds-cycle-trace t]
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
       ["Toggle display of arguments"   mds-toggle-show-args t]
       ["Toggle input tracking"         mds-toggle-track-input t]
       ["Toggle mds-wait-until-ready"   mds-toggle-wait-until-ready t]
       ["Toggle mds-stop-trace-at-error-flag"   mds-toggle-stop-trace-at-error-flag t]
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

(define-derived-mode mds-ss-mode maplev-proc-mode "showstat-mode"
  "Major mode for stepping through a debugged Maple procedure.

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
\\[mds-cycle-trace] cycle through trace modes
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
\\[mds-toggle-truncate-lines] toggle whether to fold or truncate long lines
C-u \\[mds-toggle-truncate-lines] toggle truncation in debugger output buffer
\\[mds-toggle-show-args] toggle the displaying of arguments when entering a procedure
\\[mds-patch] patch procedure in the buffer
\\[mds-ss-refresh] refresh procedure in the buffer
"
  :group 'mds

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
