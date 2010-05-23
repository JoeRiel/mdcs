;;; mdb-showstat.el --- mdb-showstat-mode

;; Copyright (C) 2009 Joseph S. Riel, all rights reserved

;; Author:     Joseph S. Riel <jriel@maplesoft.com>
;; Created:    Jan 2009
;; Keywords:   maple, debugger
;;
;;; Commentary:

;; This file contains the source for the showstat 

;;; Code:

(require 'mdb)
(require 'maplev)
(eval-when-compile
  (require 'hl-line))

(add-to-list 'overlay-arrow-variable-list 'mdb-showstat-arrow-position)

;;{{{ showstat buffer creation and update

(defun mdb-showstat-update (procname state &optional save-procname)
  "Update the showstat buffer, `mdb-showstat-procname', and `mdb-showstat-state'.
PROCNAME is the name of the procedure, STATE is the current
state; both are strings.  If `mdb-showstat-buffer' is already
displaying PROCNAME, then move the arrow; otherwise call showstat
to display the new procedure."
  (with-current-buffer mdb-showstat-buffer

    ;; save the active procname
					   
    (if save-procname
	(setq mdb-showstat-procname-active    mdb-showstat-procname
	      mdb-showstat-state-active       mdb-showstat-state
	      mdb-showstat-procname-inactive  procname)
      (setq mdb-showstat-procname-active    procname
	    mdb-showstat-procname-inactive  nil))
      

    (setq mdb-showstat-state state)
    ;; Revert cursor-type to ready status.
    (setq cursor-type mdb-cursor-ready)
    (let ((at-first-state (string= state "1")))
      (if (and (equal procname mdb-showstat-procname)
	       (not at-first-state))
	  
	  ;; procname has not changed.
	  ;;
	  ;; Assume we are in the same procedure (not robust).
	  ;; Move the arrow.
	  (mdb-showstat-display-state)

	;; procname has changed.
	;;
	;; Update the mdb-debugger-output-buffer with procname and, if
	;; entering procname, the values of its arguments.  First
	;; determine whether we just entered procname or are
	;; continuing (this may not be robust).
	
	;; Print procname (just the name) with appropriate face.
	(mdb-display-debugger-output 
	 (format "%s:\n"
		 (propertize procname
			     'face (if at-first-state
				       'mdb-face-procname-entered
				     'mdb-face-procname-cont))))
	;; Display arguments if we just entered the procedure.
	(if (and mdb-show-args-on-entry at-first-state)
	    (mdb-show-args-as-equations)))
      
      ;; Save procname in the global variable,
      ;; then update the showstat buffer
      (setq mdb-showstat-procname procname)
      ;; Send the showstat command to the debugger;
      (tq-enqueue mdb-tq (format "showstat %s\n" procname)
		  mdb--prompt-with-cr-re
		  mdb-showstat-buffer
		  #'mdb-showstat-display-proc
		  'delay)
      )))


(defun mdb-showstat-display-proc (buffer msg)
  "Insert MSG into BUFFER, which should be the showstat buffer.
MSG is expected to have the general form
showstat

proc()
...
end proc

^MDBG>

The preamble \"showstat\" and postamble prompt are elided."
  (with-current-buffer buffer
    (let ((buffer-read-only nil))
      ;; Delete old contents then insert the new.
      (delete-region (point-min) (point-max))
      (insert msg)
      ;; Delete prompt and extra lines at end of buffer.
      (forward-line 0)
      (delete-region (point) (point-max))
      ;; Delete 'showstat' and blank lines at beginning of buffer.
      (goto-char (point-min))
      (forward-line 1)
      (delete-region (point-min) (point))
      ;; Set the state arrow
      (mdb-showstat-display-state)
      ;; Display the buffer.
      (display-buffer buffer))))

(defun mdb-showstat-display-state ()
  "Move the overlay arrow in the showstat buffer to current state
and ensure that the buffer and line are visible.  The current
state is stored in `mdb-showstat-state'.  If the `hl-line'
feature is present in this session, then highlight the line.
POINT is moved to the indentation of the current line."
  (let ((buffer-read-only nil)
	(state mdb-showstat-state))
    ;; Find the location of STATE in the buffer.
    (goto-char (point-min))
    (re-search-forward (concat "^ *" state "[ *?]\\(!\\)?"))
    ;; Remove the bang, which showstat uses to mark the current state.
    (if (match-string 1)
	(replace-match " " nil nil nil 1))
    ;; Move the arrow marker to the left margin of the state.
    (beginning-of-line)
    (or mdb-showstat-arrow-position
	(setq mdb-showstat-arrow-position (make-marker)))
    (set-marker mdb-showstat-arrow-position (point))
    ;; If `hl-line' is enabled, highlight the line.
    (when (featurep 'hl-line)
      (cond
       (global-hl-line-mode
	(global-hl-line-highlight))
       ((and hl-line-mode hl-line-sticky-flag)
	(hl-line-highlight))))
    ;; Move point to indentation of the current line (not including the state number).
    (re-search-forward "^ *[1-9][0-9]*[ *?]? *" nil 'move))
  ;; Ensure marker is visible in buffer.
  (set-window-point (get-buffer-window mdb-showstat-buffer) (point)))

(defun mdb-showstat-get-buffer-create ()
  "Return the `mdb-showstat-buffer' buffer.
If it does not exist, or is killed, then create it."
  (or (and (buffer-live-p mdb-showstat-buffer)
	   mdb-showstat-buffer)
      (progn
	(setq mdb-showstat-buffer (get-buffer-create "*showstat*"))
	(with-current-buffer mdb-showstat-buffer
	  (delete-region (point-min) (point-max))
	  (mdb-showstat-mode))
	mdb-showstat-buffer)))

;;}}}
;;{{{ send strings to maple

;; Each of these functions send a string to the maple engine,
;; using mdb-send-string.  The second argument is a flag;
;; when non-nil it indicates that the string executes a command
;; in the debugged code.

(defun mdb-showstat-send-command (cmd)
  "Send CMD, with appended newline, to the Maple process.
Save CMD in `mdb-last-debug-cmd'.  Change cursor type to
`mdb-cursor-waiting', which indicates we are waiting for a
response from Maple.  This function assumes `mdb-showstat-buffer'
is the current buffer."
  (setq mdb-last-debug-cmd cmd)
  (setq cursor-type mdb-cursor-waiting)
  (forward-char) ;; this indicates 'waiting' in tty Emacs, where cursor doesn't change
  (mdb-send-string (concat cmd "\n") t))

(defun mdb-showstat-eval-expr (expr)
  "Send EXPR, with appended newline, to the Maple process."
  (mdb-send-string (concat expr "\n") nil))

;;}}}

;;{{{ select maple expressions

(defun mdb-ident-around-point-interactive (prompt &optional default complete)
  "Request Maple identifier in minibuffer, using PROMPT.
Default is identifier around point. If it is empty use DEFAULT.
Minibuffer completion is used if COMPLETE is non-nil."
  ;; Suppress error message
  (if (not default) (setq default t))
  (let ((enable-recursive-minibuffers t)
	;; this is really simple.  Need to improve...
        (ident (if (looking-at " *\\(?:if\\|return\\)? +")
		   (save-excursion
		     (goto-char (match-end 0))
		     (maplev--ident-around-point default))
		 (maplev--ident-around-point default)))
        (maplev-completion-release maplev-release)
        choice)
    (setq prompt (concat prompt (unless (string-equal ident "")
                                  (concat " (default " ident ")"))
                         ": ")
          choice (if complete
                     (completing-read prompt 'maplev--completion
                                      nil nil nil maplev-history-list ident)
                   (read-string prompt nil maplev-history-list ident)))
    ;; Are there situations where we want to suppress the error message??
    (if (string-equal choice "")
        (error "Empty choice"))
    choice))

;; this is not used

(defun mdb--select-expression-at-point (prompt &optional default complete)
  (if t ;; (mdb-showstat-bol)
      (cond ((looking-at (concat "for +\\("
				 maplev--symbol-re
				 "\\)"))
	     (match-string-no-properties 1))
	    ((looking-at "if +\\(\\(?:.*?\\)\\(?:\n.*?)*?\\)then[ \t\n]")
	     (match-string-no-properties 1))
	    (t
	     (maplev-ident-around-point-interactive prompt default complete)))
    (maplev-ident-around-point-interactive prompt default complete)))

;;}}}
;;{{{ commands

;; Define the interactive commands bound to keys

;;{{{ (*) Tracing

(defun mdb-cont ()
  "Send the 'cont' (continue) command to the debugger."
  (interactive)
  (mdb-goto-current-state)
  (mdb-showstat-send-command "cont"))

(defun mdb-into ()
  "Send the 'into' command to the debugger."
  (interactive)
  (mdb-goto-current-state)
  (mdb-showstat-send-command "into"))

(defun mdb-next ()
  "Send the 'next' command to the debugger."
  (interactive)
  (mdb-goto-current-state)
  (mdb-showstat-send-command "next"))

(defun mdb-outfrom ()
  "Send the 'outfrom' command to the debugger."
  (interactive)
  (mdb-goto-current-state)
  (mdb-showstat-send-command "outfrom"))

(defun mdb-quit ()
  "Send the 'quit' command to the debugger.
If not debugging, pop to the `mdb-buffer'."
  (interactive)
  (if (not mdb-debugging-flag)
      (mdb-pop-to-mdb-buffer)
    (mdb-goto-current-state)
    (mdb-showstat-send-command "quit")))

(defun mdb-return ()
  "Send the 'return' command to the debugger."
  (interactive)
  (mdb-goto-current-state)
  (mdb-showstat-send-command "return"))

(defun mdb-step ()
  "Send the 'step' command to the debugger."
  (interactive)
  (mdb-goto-current-state)
  (mdb-showstat-send-command "step"))

;;}}}
;;{{{ (*) Stop points

(defvar mdb-showstat-stoperror-history-list '("all" "traperror")
  "History list used by stoperror.")

(defvar mdb-showstat-stopwhen-history-list nil
  "History list used by stopwhen.")

(defun mdb--query-stop-var (cmd type hist)
  "Prompt the user with \"CMD [TYPE]: \", using history list HIST."
  (read-from-minibuffer (format "%s [%s]: " cmd type)
			nil nil nil nil hist))

(defun mdb-breakpoint ()
  "Set a breakpoint at the current/previous state."
  (interactive)
  ;; Assume we are in the showstat buffer
  ;; TODO: An alternative is to move outward from
  ;; the Maple structure.  
  ;; If at an elif or else, then move ...
  (save-excursion
    (end-of-line)
    (if (re-search-backward "^ *\\([1-9][0-9]*\\)\\([ *?]\\)" nil t)
	(let ((state (match-string-no-properties 1))
	      (inhibit-read-only t))
	  ;; FIXME: only replace a space, not a ?
	  (replace-match "*" nil nil nil 2)
	  (mdb-showstat-eval-expr (format "stopat %s %s" mdb-showstat-procname state)))
      (ding)
      (message "no previous state in buffer"))))

(defun mdb-breakpoint-cond ()
  "Set a conditional breakpoint at the current/previous state."
  (interactive)
  ;; Assume we are in the showstat buffer
  (save-excursion
    (end-of-line)
    (if (re-search-backward "^ *\\([1-9][0-9]*\\)\\([ *?]\\)" nil t)
	(let ((state (match-string-no-properties 1))
	       (inhibit-read-only t)
	       (cond (mdb--query-stop-var "stopat-cond" "condition" 'mdb-showstat-stopwhen-history-list)))
	  (replace-match "?" nil nil nil 2)
	  (mdb-showstat-eval-expr (format "debugopts('stopat'=[%s,%s,%s])" (mdb-thisproc) state cond)))
      (ding)
      (message "no previous state in buffer"))))


(defun mdb-stoperror (clear)
  "Query for and set or clear, if CLEAR is non-nil, a watchpoint on an error."
  (interactive "P")
  (let* ((cmd (if clear "unstoperror" "stoperror"))
	 (err (mdb--query-stop-var cmd "errMsg" 'mdb-showstat-stoperror-history-list)))
    (mdb-showstat-eval-expr (format "%s %s" cmd err))))

(defun mdb-stoperror-clear ()
  "Query for and clear a watchpoint on an error."
  (interactive)
  (mdb-stoperror 'clear))


(defun mdb-stopwhen-local (clear)
  "Set or clear, if CLEAR is non-nil, watchpoint on a variable.
Query for local variable, using symbol at point as default."
  (interactive "P")
  (let* ((cmd (if clear "unstopwhen" "stopwhen"))
	 (var (mdb--query-stop-var cmd "var" 'mdb-showstat-stopwhen-history-list)))
    (mdb-showstat-eval-expr (format "%s procname %s" cmd var))))

(defun mdb-stopwhen-global (clear)
  "Set or clear, if CLEAR is non-nil, watchpoint on a variable.
Query for global variable, using symbol at point as default."
  (interactive "P")
  (let* ((cmd (if clear "unstopwhen" "stopwhen"))
	 (var (mdb--query-stop-var cmd "var" 'mdb-showstat-stopwhen-history-list)))
    (mdb-showstat-eval-expr (format "%s %s" cmd var))))

(defun mdb-stopwhenif ()
  "Query and set a conditional watchpoint on a variable."
  (interactive)
  (let* ((cmd "stopwhenif")
	 (var (mdb--query-stop-var cmd "var" 'mdb-showstat-stopwhen-history-list))
	 (val (read-string "value: ")))
    (mdb-showstat-eval-expr (format "%s(%s,%s)" cmd var val))))

(defun mdb-stopwhen-clear ()
  "Query and clear a watchpoint on a variable."
  (interactive)
  (mdb-stopwhen-global 'clear))

(defun mdb-unstopat ()
  "Clear a breakpoint at the current state.
If the state does not have a breakpoint, print a message."
  (interactive)
  (if (eq (current-buffer) mdb-showstat-buffer)
      (save-excursion			; this does no good ...
	(end-of-line)
	(if (and (re-search-backward "^ *\\([1-9][0-9]*\\)\\(\\*?\\)" nil t)
		 (string= (match-string 2) "*"))
	    (let ((state (match-string-no-properties 1))
		  (inhibit-read-only t))
	      (replace-match " " nil nil nil 2)
	      (mdb-showstat-eval-expr (concat "unstopat " state)))
	  (ding)
	  (message "no breakpoint at this state")))
    (ding)
    (message "not in showstat buffer")))

;;}}}
;;{{{ (*) Evaluation

(defun mdb-eval-and-prettyprint (expr)
  "Pretty-print EXPR.  This calls the Maple procedure 
mdb:-PrettyPrint to convert EXPR into a more useful display.
With optional prefix, clear debugger output before displaying."
  (interactive (list (mdb-ident-around-point-interactive
		      "prettyprint: " "")))
  (if current-prefix-arg (mdb-debugger-clear-output))
  (mdb-send-string (format "mdb:-PrettyPrint(%s)\n" expr)
		   nil ; not advancing the debugger
		   (propertize (format "%s:\n" expr)
		   	       'face 'mdb-face-prompt ; FIXME: create appropriate face
		    	       )
		   nil
		   #'mdb-prettify-args-as-equations))

(defun mdb-eval-and-display-expr (expr &optional suffix)
  "Evaluate a Maple expression, EXPR, display result and print optional SUFFIX.
If called interactively, EXPR is queried."
  (interactive (list (mdb-ident-around-point-interactive
		      "eval: " "")))
  (if current-prefix-arg (mdb-debugger-clear-output))
  (mdb-send-string (concat expr "\n")
		   nil
		   (propertize (format "%s:\n" expr)
			       'face 'mdb-face-prompt ; FIXME: create appropriate face
			       )
		   suffix ))


(defun mdb-eval-and-display-expr-global (expr)
  "Evaluate a Maple expression, EXPR, in a global context.  
If called interactively, EXPR is queried.
The result is returned in the message area."
  (interactive (list (mdb-ident-around-point-interactive
		      "global eval: " "")))
  (if current-prefix-arg (mdb-debugger-clear-output))
  (mdb-eval-and-display-expr (concat "statement " expr)))


;;}}}
;;{{{ (*) Information

(defun mdb-args ()
  "Display the arguments of the current procedure."
  (interactive)
  (mdb-showstat-eval-expr "args"))


(defun mdb-show-args-as-equations ()
  "Display the parameters and arguments of the current Maple procedure as equations."
  (interactive)
  (if current-prefix-arg (mdb-debugger-clear-output))
					; We need to use a global variable for the index,
					; one that isn't likely to appear in an expression.
					; Alternatively, a module expoart could be used.
  (mdb-send-string (format "mdb:-ArgsToEqs(%s, [seq([_params[`_|_`]],`_|_`=1.._nparams)],[_rest],[_options])\n"
			   (mdb-thisproc))
		   nil
		   (propertize "args:\n" 'face 'mdb-face-prompt)
		   nil
		   #'mdb-prettify-args-as-equations))

(defconst mdb--flush-left-arg-re "^\\([a-zA-Z%_][a-zA-Z0-9_]*\\??\\) =")

(defun mdb-prettify-args-as-equations (beg end)
  "Font lock the argument names in the region from BEG to END."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward mdb--flush-left-arg-re end t)
      (put-text-property (match-beginning 1) (match-end 1) 'face 'mdb-face-arg))))

(defun mdb-showstack ()
  "Send the 'showstack' command to the debugger.
Note that the string displayed in the echo area has the current
procedure stripped from it."
  (interactive)
  (mdb-showstat-eval-expr "showstack"))

(defun mdb-showstop ()
  "Send the 'showstop' command to the debugger."
  (interactive)
  (mdb-showstat-eval-expr "showstop"))

(defun mdb-showerror ()
  "Send the 'showerror' command to the debugger."
  (interactive)
  (mdb-showstat-eval-expr "showerror"))

(defun mdb-showexception ()
  "Send the 'showexception' command to the debugger."
  (interactive)
  (mdb-showstat-eval-expr "showexception"))

;;}}}
;;{{{ (*) Miscellaneous

(defun mdb-goto-current-state ()
  "Move cursor to the current state in the active showstat buffer."
  (interactive) 
  (if mdb-showstat-procname-inactive
      (mdb-showstat-update mdb-showstat-procname-active mdb-showstat-state-active)
    (mdb-goto-state mdb-showstat-state)))

(defun mdb-goto-state (state)
  "Move POINT to STATE."
  ;; Assume we are in the showstat buffer.
  (goto-char (point-min))
  (unless (re-search-forward (concat "^ *" state "[ *?]\\s-*") nil t)
    (ding)
    (message "cannot find state %s" state)))

(defun mdb-toggle-truncate-lines (output-buffer)
  "Toggle the truncation of long lines.  If OUTPUT-BUFFER is
non-nil, do so in the `mdb-debugger-output-buffer', otherwise do so in 
the `mdb-showstat-buffer'."
  (interactive "P")
  (with-current-buffer
      (if output-buffer
	  mdb-debugger-output-buffer
	mdb-showstat-buffer)
  (toggle-truncate-lines)))
    

(defun mdb-where (&optional depth)
  "Send the 'where' command to the debugger.
The optional DEPTH parameter is a positive integer that specifies
the number of activation levels to display."
  (interactive "P")
  (let ((cmd (if depth
		 (format "where %d\n" depth)
	       "where\n")))
    (mdb-send-string cmd nil nil nil
		     #'mdb-highlight-where-output)))

(defun mdb-highlight-where-output (beg end)
  "Font lock the names of called functions in the region from BEG to END,
which is the output of `mdb-where'."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "^[^ \t\n]+: " end t)
      (put-text-property (match-beginning 0) (match-end 0) 'face 'font-lock-function-name-face))))


(defun mdb-pop-to-mdb-buffer ()
  "Pop to the Maple debugger buffer."
  (interactive)
  (pop-to-buffer mdb-buffer))

(defun mdb-help-debugger ()
  (interactive)
  (maplev-help-show-topic "debugger"))

(defun mdb-info ()
  "Display the info page for Mdb."
  (interactive)
  (info "mdb"))

;;}}}

;;}}}
;;{{{ mode map

(defvar mdb-showstat-mode-map
  (let ((map (make-sparse-keymap))
	(bindings
	 '((" " . mdb-send-last-command)
	   ("A" . mdb-show-args-as-equations)
	   ("a" . mdb-args)
	   ("b" . mdb-breakpoint)
	   ("B" . mdb-breakpoint-cond)
	   ("c" . mdb-cont)
	   ("C" . mdb-debugger-clear-output)
	   ("d" . self-insert-command)
	   ("e" . mdb-eval-and-display-expr)
	   ("E" . mdb-eval-and-display-expr-global)
	   ("f" . self-insert-command)
	   ("h" . mdb-help-debugger)
	   ("H" . mdb-info)
	   ("i" . mdb-into)
	   ("I" . mdb-stopwhenif)
	   ("k" . mdb-showstack)
	   ("K" . mdb-where)
	   ("l" . mdb-goto-current-state)
	   ("n" . mdb-next)
	   ("o" . mdb-outfrom)
	   ("p" . mdb-showstop)
	   ("q" . mdb-quit)
	   ("r" . mdb-return)
	   ("R" . mdb-stoperror)
	   ("s" . mdb-step)
	   ("T" . mdb-toggle-truncate-lines)
	   ("u" . mdb-unstopat)
	   ("w" . mdb-stopwhen-local)
	   ("W" . mdb-stopwhen-global)
	   ("x" . mdb-showexception)
	   ("X" . mdb-showerror)
	   ("." . mdb-eval-and-prettyprint)
	   ("\C-c\C-c" . mdb-kill-maple)
	   ("\C-c\C-o" . mdb-pop-to-mdb-buffer)
	   )))
    (mapc (lambda (binding) (define-key map (car binding) (cdr binding)))
	  bindings)
    map))

;;}}}

;;{{{ menu

(defvar mdb-showstat-menu nil)
(unless mdb-showstat-menu
  (easy-menu-define
    mdb-showstat-menu mdb-showstat-mode-map
    "Menu for Mdb showstat mode"
    `("Showstat"

      ("Tracing"
       ["Continue"	mdb-cont t]
       ["Next"		mdb-next t]
       ["Into"		mdb-into t]
       ["Outfrom"	mdb-outfrom t]
       ["Step"		mdb-step t]
       ["Return"	mdb-return t]
       ["Quit"		mdb-step t]
       ["Kill"		mdb-kill-maple t])

      ("Stop points"
       ["Set breakpoint at point"    mdb-breakpoint t]
       ["Clear breakpoint at point"  mdb-unstopat t]
       ["Clear conditional breakpoint at point"  mdb-breakpoint-cond t]
       "----"
       ["Set global watchpoint"      mdb-stopwhen-global t]
       ["Set local watchpoint"       mdb-stopwhen-local t]
       ["Set conditional watchpoint" mdb-stopwhenif t] 
       ["Clear watchpoint"           mdb-stopwhen-clear :keys "C-u w"]
       "----"
       ["Set watchpoint on error"    mdb-stoperror t]
       ["Clear watchpoint on error"  mdb-stoperror-clear :keys "C-u R"]
       "----"
       ["Show all stop points"       mdb-showstop t]
       )

      ("Evaluation"
       ["Evaluate expression"			mdb-eval-and-display-expr t]
       ["Evaluate expression in global context" mdb-eval-and-display-expr-global t]
       ["Evaluate and prettyprint expression"	mdb-eval-and-prettyprint t] )

      ("Information"
       ["Display parameters and values" mdb-show-args-as-equations t]
       ["Show stack"			mdb-showstack t]
       ["Show stack with arguments"	mdb-where t]
       ["Show error"			mdb-showerror t]
       ["Show exception"		mdb-showexception t])

      ("Miscellaneous"
       ["Pop to Mdb buffer"        mdb-pop-to-mdb-buffer t]
       ["Clear debugger output"    mdb-debugger-clear-output t]
       ["Toggle truncate lines"    mdb-toggle-truncate-lines t])

      ("Help"
       ["Help Maple debugger"      mdb-help-debugger t]
       ["Info for Mdb mode"        mdb-info t])

       )))

;;}}}

;;{{{ showstat-mode

(define-derived-mode mdb-showstat-mode maplev-proc-mode "showstat-mode"
  "Major mode for stepping through a debugged Maple procedure.
This mode is automatically applied to the `mdb-showstat-buffer' generated by `mdb'.

Tracing
-------
\\[mdb-send-last-command] repeat the last tracing command
\\[mdb-cont] (cont) continue execution until next stop point
\\[mdb-next] (next) execute next statement at current nesting level
\\[mdb-into] (into) execute next statement at any level in current procedure
\\[mdb-outfrom] (outfrom) execute current statement sequence or until stop point
\\[mdb-step] (step) execute next statement at any level
\\[mdb-return] (return) continue executing until current procedure returns
\\[mdb-quit] (quit) terminate debugging, return to mdb buffer
\\[mdb-kill-maple] kill and restart the Maple process

Stop points
-----------
\\[mdb-breakpoint] (stopat) set breakpoint at cursor
\\[mdb-unstopat] (unstopat) clear breakpoint at cursor
\\[mdb-breakpoint-cond] (stopat-cond) set conditional breakpoint at cursor

\\[mdb-stopwhenif] (stopwhenif) set watchpoint on variable = value

\\[mdb-stopwhen-local] (stopwhen) set watchpoint on local variable
C-u \\[mdb-stopwhen-local] (stopwhen) clear watchpoint on local variable
\\[mdb-stopwhen-global] (stopwhen) set watchpoint on global variable
C-u \\[mdb-stopwhen-global] (stopwhen) clear watchpoint on global variable

\\[mdb-showstop] (showstop) display all breakpoints

Information
-----------
\\[mdb-show-args-as-equations] display the parameter names and values
\\[mdb-args] display the arguments of the current procedure
\\[mdb-help-debugger] display Maple debugger help page
\\[mdb-showstack] (showstack) display abbreviated stack
\\[mdb-where] (where) display stack of procedure calls
\\[mdb-showerror] display the last error
\\[mdb-showexception] display the last exception

Evaluation
----------
\\[mdb-eval-and-display-expr] evalute a Maple expression
C-u \\[mdb-eval-and-display-expr] clear output then evalute a Maple expression
\\[mdb-eval-and-display-expr-global] evalute a Maple expression in a global context
\\[mdb-eval-and-prettyprint] evaluate and prettyprint a Maple expression
C-u \\[mdb-eval-and-prettyprint] clear output then evaluate and prettyprint a Maple expression

Miscellaneous
-------------
\\[mdb-pop-to-mdb-buffer] pop to the mdb buffer
\\[mdb-goto-current-state] move (return) cursor to current state
\\[mdb-debugger-clear-output] clear the debugger output buffer
\\[mdb-help-debugger] display help page for the Maple debugger
\\[mdb-info] display info pages for the Maple debugger
\\[maplev-help-at-point] display a Maple help page
\\[maplev-proc-at-point] display a Maple procedure
\\[mdb-toggle-truncate-lines] toggle whether to fold or truncate long lines
C-u \\[mdb-toggle-truncate-lines] toggle truncation in debugger output buffer
"
  :group 'mdb
  (setq mdb-showstat-procname ""
	mdb-showstat-state ""
	mdb-showstat-arrow-position nil)

  (and mdb-showstat-menu (easy-menu-add mdb-showstat-menu))

  (add-hook 'kill-buffer-hook '(lambda () (setq mdb-update-showstat-p t)) nil 'local))

;;}}}

(provide 'mdb-showstat)

