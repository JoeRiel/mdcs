;;; mdcs-showstat.el --- mdcs-showstat-mode
;;; -*- mode emacs-lisp; mode: folding -*-

;; Copyright (C) 2009 Joseph S. Riel, all rights reserved

;; Author:     Joseph S. Riel <jriel@maplesoft.com>
;; Created:    Jan 2009
;; Keywords:   maple, debugger
;;
;;; Commentary:

;; This file contains the source for the showstat functions.

;;; Code:

(require 'maplev)
(eval-when-compile
  (require 'hl-line))

;;{{{ variables

(defvar mdcs-showstat-arrow-position	nil "Marker for state arrow.")
(defvar mdcs-showstat-debugging-flag nil "Non-nil when debugging.")
(defvar mdcs-showstat-last-debug-cmd "" "Stores the last debugger command.")
(defvar mdcs-showstat-output-buffer	nil "Buffer that displays output")
(defvar mdcs-showstat-procname		""  "Name of current showstat procedure.")
(defvar mdcs-showstat-procname-active	""  "Name of active showstat procedure.")
(defvar mdcs-showstat-procname-inactive	nil "Name of inactive showstat procedure.")
(defvar mdcs-showstat-server-proc	nil "Server associated with this buffer")
(defvar mdcs-showstat-state              "1" "Current state of procedure.")
(defvar mdcs-showstat-state-active	nil "Store current state of active procedure")
(defvar mdcs-watch-alist nil  "Alist for storing watch variables.  The keys are procedure names,the values are additional alists.")

;; Make variables buffer-local
(mapc #'make-variable-buffer-local
      '(
	mdcs-showstat-arrow-position
	mdcs-showstat-debugging-flag
	mdcs-showstat-last-debug-cmd
	mdcs-showstat-output-buffer
	mdcs-showstat-procname
	mdcs-showstat-procname-active
	mdcs-showstat-procname-inactive
	mdcs-showstat-server-proc
	mdcs-showstat-state
	mdcs-showstat-state-active
	mdcs-showstat-watch-alist
	))

(add-to-list 'overlay-arrow-variable-list 'mdcs-showstat-arrow-position)

;;}}}

;;{{{ send strings to maple

(defun mdcs-showstat-send (msg &rest rest)
  "Send MSG to the associated Maple process.
REST arguments are ignored; see `mdcs-send-string' for
what was expected."
  (mdcs-server-send-client mdcs-showstat-server-proc msg))

;; Each of these functions send a string to the maple engine,
;; using mdcs-showstat-send.  The second argument is a flag;
;; when non-nil it indicates that the string executes a command
;; in the debugged code.

(defun mdcs-showstat-send-command (cmd)
  "Send CMD, with appended newline, to the Maple process.
Save CMD in `mdcs-showstat-last-debug-cmd'.  Change cursor type to
`mdcs-cursor-waiting', which indicates we are waiting for a
response from Maple.  This function assumes we are in 
the appropriate `mdcs-showstat-buffer'."
  (setq mdcs-showstat-last-debug-cmd cmd)
  (setq cursor-type mdcs-cursor-waiting)
  (forward-char) ;; this indicates 'waiting' in tty Emacs, where cursor doesn't change
  (mdcs-showstat-send (concat cmd "\n") t))

(defun mdcs-showstat-eval-expr (expr)
  "Send EXPR, with appended newline, to the Maple process."
  (mdcs-showstat-send (concat expr "\n") nil))

;;}}}

;;{{{ showstat buffer creation and update

(defun mdcs-showstat-update (procname state)
  "Update the showstat buffer and the buffer-local variables
`mdcs-showstat-procname', and `mdcs-showstat-state'.  PROCNAME is
the name of the procedure, STATE is the current state; both are
strings.  If the buffer is already displaying PROCNAME, then move
the arrow; otherwise call (maple) showstat to display the new
procedure."
  ;;  (with-current-buffer mdcs-showstat-buffer

  ;; save the active procname, clear the inactive, and set the state
  (setq mdcs-showstat-procname-active    procname
	mdcs-showstat-procname-inactive  nil
	mdcs-showstat-state              state)

  ;; Revert cursor-type to ready status.
  (setq cursor-type mdcs-cursor-ready)
  (let ((at-first-state (string= state "1")))
    (if (and (equal procname mdcs-showstat-procname)
	     (not at-first-state))
	
	;; procname has not changed.
	;;
	;; Assume we are in the same procedure (not robust).
	;; Move the arrow.
	(mdcs-showstat-display-state)

      ;; procname has changed.
      ;;
      ;; Update the `mdcs-showstat-output-buffer' with procname and, if
      ;; entering procname, the values of its arguments.  First
      ;; determine whether we just entered procname or are
      ;; continuing (this may not be robust).
      
      ;; Print procname (just the name) with appropriate face.
      (mdcs-showstat-display-debugger-output 
       (format "%s:\n"
	       (propertize procname
			   'face (if at-first-state
				     'mdcs-face-procname-entered
				   'mdcs-face-procname-cont))))
      ;; Display arguments if we just entered the procedure.
      ;;(if (and mdcs-show-args-on-entry at-first-state)
      ;;      (mdcs-show-args-as-equations))
      )
    
    ;; Save procname in the global variable,
    ;; then update the showstat buffer
    (setq mdcs-showstat-procname procname)
    ;; Send the showstat command to the debugger;
    ;; (tq-enqueue mdcs-tq (format "showstat\n")
    ;; 		mdcs--prompt-with-cr-re
    ;; 		(cons mdcs-showstat-buffer nil)
    ;; 		#'mdcs-showstat-display-proc
    ;; 		'delay)
    
    (mdcs-showstat-send "showstat")))

(defun mdcs-showstat-display-inactive (procname statement)
  "Update the showstat buffer, `mdcs-showstat-procname', and `mdcs-showstat-state'.
PROCNAME is the name of the procedure, STATE is the current
state; both are strings.  If `mdcs-showstat-buffer' is already
displaying PROCNAME, then move the arrow; otherwise call showstat
to display the new procedure."
;;  (with-current-buffer mdcs-showstat-buffer

    (if (not mdcs-showstat-procname-inactive)
	;; save the active procname and state
	(setq mdcs-showstat-procname-active    mdcs-showstat-procname
	      mdcs-showstat-state-active       mdcs-showstat-state))

    (setq mdcs-showstat-procname-inactive  procname
	  mdcs-showstat-procname           procname)

    ;; Send the showstat command to the debugger;
    ;; (tq-enqueue mdcs-tq (format "mdb:-showstat(\"%s\")\n" procname)
    ;; 		mdcs--prompt-with-cr-re
    ;; 		(cons mdcs-showstat-buffer statement)
    ;; 		#'mdcs-showstat-display-proc
    ;; 		'delay)
    (mdcs-showstat-send (format "mdb:-showstat(\"%s\")" procname)))


(defun mdcs-showstat-display-proc (msg)
  "Insert MSG into the showstat buffer.
The preamble \"showstat\" and postamble prompt are elided."
  (let ((buffer-read-only nil))
    ;; Delete old contents then insert the new.
    (delete-region (point-min) (point-max))
    (insert msg)
    ;; Goto current state
    ;; (when statement
    ;;   (search-forward (concat " " statement) nil t)
    ;;   (setq mdcs-showstat-state (mdcs-showstat-get-state)))
    ;; Set the state arrow
    (mdcs-showstat-display-state)
    ;; Display the buffer.
    (display-buffer (current-buffer))))

(defun mdcs-showstat-display-state ()
  "Move the overlay arrow in the showstat buffer to current state
and ensure that the buffer and line are visible.  The current
state is stored in `mdcs-showstat-state'.  If the `hl-line'
feature is present in this session, then highlight the line.
POINT is moved to the indentation of the current line."
  (let ((buffer-read-only nil)
	(state mdcs-showstat-state))
    ;; Find the location of STATE in the buffer.
    (goto-char (point-min))
    (re-search-forward (concat "^ *" state "[ *?]\\(!\\)?"))
    ;; Remove the bang, which showstat uses to mark the current state.
    (if (match-string 1)
	(replace-match " " nil nil nil 1))
    ;; Move the arrow marker to the left margin of the state.
    (beginning-of-line)
    (or mdcs-showstat-arrow-position
	(setq mdcs-showstat-arrow-position (make-marker)))
    (set-marker mdcs-showstat-arrow-position (point))
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
  (set-window-point (get-buffer-window) (point)))

(defun mdcs-showstat-generate-buffer (proc)
  "Generate and return a new `mdcs-showstat-buffer' buffer
and an `mdcs-showstat-output-buffer'."
  (let ((buf (generate-new-buffer "*mdcs-showstat*")))
    (with-current-buffer buf
      (mdcs-showstat-mode)
      (setq mdcs-showstat-arrow-position nil
	    mdcs-showstat-output-buffer (generate-new-buffer "*mdcs-output*")
	    mdcs-showstat-procname ""
	    mdcs-showstat-procname-active ""
	    mdcs-showstat-procname-inactive nil
	    mdcs-showstat-state "1"
	    mdcs-showstat-state-active nil
	    mdcs-showstat-server-proc proc))
    buf))

;;}}}


(defun mdcs-showstat-kill-buffer (buf)
  (when (bufferp buf)
    (with-current-buffer buf
      (kill-buffer mdcs-showstat-output-buffer))
    (kill-buffer buf)))


(defun mdcs-showstat-display-debugger-output (msg &optional func)
  "Display MSG in `mdcs-debugger-output-buffer'."
  (unless (string= msg "")
    (let ((buf mdcs-showstat-output-buffer))
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

			    

(defun mdcs-showstat-set-debugging-flag (debugging)
  "Compare DEBUGGING with `mdcs-showstat-debugging-flag'.
A difference indicates that debugging has started/stopped.  Reassign
`mdcs-debugging-flag' and run either `mdcs-showstat-start-debugging' or
`mdcs-showstat-finish-debugging'."
  (if mdcs-showstat-debugging-flag
      (unless debugging
	;; turn-off debugging.
	(setq mdcs-showstat-debugging-flag nil)
	(mdcs-showstat-finish-debugging))
    (when debugging
      ;; turn-on debugging
      (mdcs-showstat-start-debugging)
      (setq mdcs-showstat-debugging-flag t))))

(defun mdcs-showstat-start-debugging ()
  "Called when the debugger starts."
  (mdcs-showstat-display-debugger-output
   (propertize mdcs-debugger-break
	       'face 'mdcs-face-prompt
	       'rear-nonsticky t)))

(defun mdcs-showstat-finish-debugging ()
  "Called when the debugger finishes."
  (ding)
  (message "finished debugging")
  ;; Clear overlay in showstat buffer.
  ;; Does this handle hl-line?
  (set-marker mdcs-showstat-arrow-position nil)
  ;; Reset the showstat variables.
  (setq mdcs-showstat-procname ""
	mdcs-showstat-state "1"))


(defun mdcs-thisproc ()
  "Return string corresponding to current procedure."
  (if mdcs-pre-Maple-14
      "procname"
    "thisproc"))


;;{{{ select maple expressions

(defun mdcs-ident-around-point-interactive (prompt &optional default complete)
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

(defun mdcs--select-expression-at-point (prompt &optional default complete)
  (if t ;; (mdcs-showstat-bol)
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

(defun mdcs-cont ()
  "Send the 'cont' (continue) command to the debugger."
  (interactive)
  (mdcs-goto-current-state)
  (mdcs-showstat-send-command "cont"))

(defun mdcs-into ()
  "Send the 'into' command to the debugger."
  (interactive)
  (mdcs-goto-current-state)
  (mdcs-showstat-send-command "into"))

(defun mdcs-next ()
  "Send the 'next' command to the debugger."
  (interactive)
  (mdcs-goto-current-state)
  (mdcs-showstat-send-command "next"))

(defun mdcs-outfrom ()
  "Send the 'outfrom' command to the debugger."
  (interactive)
  (mdcs-goto-current-state)
  (mdcs-showstat-send-command "outfrom"))

(defun mdcs-quit ()
  "Send the 'quit' command to the debugger.
If not debugging, pop to the `mdcs-buffer'."
  (interactive)
  (if (not mdcs-showstat-debugging-flag)
      (mdcs-pop-to-mdcs-buffer)
    (mdcs-goto-current-state)
    (mdcs-showstat-send-command "quit")))

(defun mdcs-return ()
  "Send the 'return' command to the debugger."
  (interactive)
  (mdcs-goto-current-state)
  (mdcs-showstat-send-command "return"))

(defun mdcs-step ()
  "Send the 'step' command to the debugger."
  (interactive)
  (mdcs-goto-current-state)
  (mdcs-showstat-send-command "step"))

;;}}}
;;{{{ (*) Stop points

(defun mdcs-showstat-get-state ()
  (and (re-search-backward "^ *\\([1-9][0-9]*\\)\\([ *?]\\)" nil t)
       (match-string-no-properties 1)))

(defvar mdcs-showstat-stoperror-history-list '("all" "traperror")
  "History list used by stoperror.")

(defvar mdcs-showstat-stopwhen-history-list nil
  "History list used by stopwhen.")

(defun mdcs--query-stop-var (cmd type hist)
  "Prompt the user with \"CMD [TYPE]: \", using history list HIST."
  (read-from-minibuffer (format "%s [%s]: " cmd type)
			nil nil nil nil hist))

(defun mdcs-breakpoint ()
  "Set a breakpoint at the current/previous state."
  (interactive)
  ;; Assume we are in the showstat buffer
  ;; TODO: An alternative is to move outward from
  ;; the Maple structure.  
  ;; If at an elif or else, then move ...
  (save-excursion
    (end-of-line)
    (let ((state (mdcs-showstat-get-state))
	  (inhibit-read-only t))
      (if state
	  (progn
	    ;; FIXME: only replace a space, not a ?
	    (replace-match "*" nil nil nil 2)
	    (mdcs-showstat-eval-expr (format "mdb:-stopat(\"%s\",%s)" mdcs-showstat-procname state)))
	(ding)
	(message "no previous state in buffer")))))

(defun mdcs-breakpoint-cond ()
  "Set a conditional breakpoint at the current/previous state."
  (interactive)
  ;; Assume we are in the showstat buffer
  (save-excursion
    (end-of-line)
    (if (re-search-backward "^ *\\([1-9][0-9]*\\)\\([ *?]\\)" nil t)
	(let ((state (match-string-no-properties 1))
	      (inhibit-read-only t)
	      (cond (mdcs--query-stop-var "stopat-cond" "condition" 'mdcs-showstat-stopwhen-history-list)))
	  (replace-match "?" nil nil nil 2)
	  (mdcs-showstat-eval-expr (format "debugopts('stopat'=[%s,%s,%s])" (mdcs-thisproc) state cond)))
      (ding)
      (message "no previous state in buffer"))))


(defun mdcs-stoperror (clear)
  "Query for and set or clear, if CLEAR is non-nil, a watchpoint on an error."
  (interactive "P")
  (let* ((cmd (if clear "unstoperror" "stoperror"))
	 (err (mdcs--query-stop-var cmd "errMsg" 'mdcs-showstat-stoperror-history-list)))
    (mdcs-showstat-eval-expr (format "%s %s" cmd err))))

(defun mdcs-stoperror-clear ()
  "Query for and clear a watchpoint on an error."
  (interactive)
  (mdcs-stoperror 'clear))


(defun mdcs-stopwhen-local (clear)
  "Set or clear, if CLEAR is non-nil, watchpoint on a variable.
Query for local variable, using symbol at point as default."
  (interactive "P")
  (let* ((cmd (if clear "unstopwhen" "stopwhen"))
	 (var (mdcs--query-stop-var cmd "var" 'mdcs-showstat-stopwhen-history-list)))
    (mdcs-showstat-eval-expr (format "%s procname %s" cmd var))))

(defun mdcs-stopwhen-global (clear)
  "Set or clear, if CLEAR is non-nil, watchpoint on a variable.
Query for global variable, using symbol at point as default."
  (interactive "P")
  (let* ((cmd (if clear "unstopwhen" "stopwhen"))
	 (var (mdcs--query-stop-var cmd "var" 'mdcs-showstat-stopwhen-history-list)))
    (mdcs-showstat-eval-expr (format "%s %s" cmd var))))

(defun mdcs-stopwhenif ()
  "Query and set a conditional watchpoint on a variable."
  (interactive)
  (let* ((cmd "stopwhenif")
	 (var (mdcs--query-stop-var cmd "var" 'mdcs-showstat-stopwhen-history-list))
	 (val (read-string "value: ")))
    (mdcs-showstat-eval-expr (format "%s(%s,%s)" cmd var val))))

(defun mdcs-stopwhen-clear ()
  "Query and clear a watchpoint on a variable."
  (interactive)
  (mdcs-stopwhen-global 'clear))

(defun mdcs-unstopat ()
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
	  (mdcs-showstat-eval-expr (concat "unstopat " state)))
      (ding)
      (message "no breakpoint at this state"))))

;;}}}
;;{{{ (*) Evaluation

(defun mdcs-eval-and-prettyprint (expr)
  "Pretty-print EXPR.  This calls the Maple procedure 
mdb:-PrettyPrint to convert EXPR into a more useful display.
With optional prefix, clear debugger output before displaying."
  (interactive (list (mdcs-ident-around-point-interactive
		      "prettyprint: " "")))
  (if current-prefix-arg (mdcs-debugger-clear-output))
  (mdcs-showstat-send (format "mdb:-PrettyPrint(%s)\n" expr)
		     nil ; not advancing the debugger
		     (propertize (format "%s:\n" expr)
				 'face 'mdcs-face-prompt ; FIXME: create appropriate face
				 )
		     nil
		     #'mdcs-prettify-args-as-equations))

(defun mdcs-eval-and-display-expr (expr &optional suffix)
  "Evaluate a Maple expression, EXPR, display result and print optional SUFFIX.
If called interactively, EXPR is queried."
  (interactive (list (mdcs-ident-around-point-interactive
		      "eval: " "")))
  (if current-prefix-arg (mdcs-debugger-clear-output))
  (mdcs-showstat-send (concat expr "\n")
		     nil
		     (propertize (format "%s:\n" expr)
				 'face 'mdcs-face-prompt ; FIXME: create appropriate face
				 )
		     suffix ))


(defun mdcs-eval-and-display-expr-global (expr)
  "Evaluate a Maple expression, EXPR, in a global context.  
If called interactively, EXPR is queried.
The result is returned in the message area."
  (interactive (list (mdcs-ident-around-point-interactive
		      "global eval: " "")))
  (if current-prefix-arg (mdcs-debugger-clear-output))
  (mdcs-eval-and-display-expr (concat "statement " expr)))

;;}}}
;;{{{ (*) Information

(defun mdcs-args ()
  "Display the arguments of the current procedure."
  (interactive)
  (mdcs-showstat-eval-expr "args"))


(defun mdcs-show-args-as-equations ()
  "Display the parameters and arguments of the current Maple procedure as equations."
  (interactive)
  (if current-prefix-arg (mdcs-debugger-clear-output))
					; We need to use a global variable for the index,
					; one that isn't likely to appear in an expression.
					; Alternatively, a module export could be used.
  (mdcs-showstat-send (format "mdb:-ArgsToEqs(%s, [seq([_params[`_|_`]],`_|_`=1.._nparams)],[_rest],[_options])\n"
			     (mdcs-thisproc))
		     nil
		     (propertize "args:\n" 'face 'mdcs-face-prompt)
		     nil
		     #'mdcs-prettify-args-as-equations))

(defconst mdcs--flush-left-arg-re "^\\([a-zA-Z%_][a-zA-Z0-9_]*\\??\\) =")

(defun mdcs-prettify-args-as-equations (beg end)
  "Font lock the argument names in the region from BEG to END."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward mdcs--flush-left-arg-re end t)
      (put-text-property (match-beginning 1) (match-end 1) 'face 'mdcs-face-arg))))

(defun mdcs-showstack ()
  "Send the 'showstack' command to the debugger.
Note that the string displayed in the echo area has the current
procedure stripped from it."
  (interactive)
  (mdcs-showstat-eval-expr "showstack"))

(defun mdcs-showstop ()
  "Send the 'showstop' command to the debugger."
  (interactive)
  (mdcs-showstat-eval-expr "showstop"))

(defun mdcs-showerror (fmt)
  "Send the 'showerror' command to the debugger.
If FMT (prefix arg) is non-nil, display the formatted message,
otherwise hyperlink the raw message."
  (interactive "P")
  (if fmt
      (mdcs-showstat-eval-expr "printf(\"%s\\n\",StringTools:-FormatMessage(debugopts('lasterror')))")
    ;;(mdcs-showstat-eval-expr "showerror")
    (mdcs-showstat-send "showerror\n" nil nil nil #'mdcs-showerror-link)
    ))

(defconst mdcs-link-error-re "^\\[\\([^\"].*?\\), ")

(defun mdcs-showerror-link (beg end)
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (if (looking-at mdcs-link-error-re)
	(make-text-button (match-beginning 1) (match-end 1) :type 'mdcs-showstat-open-button))))

(define-button-type 'mdcs-showerror-open-button
  'help-echo "Open procedure"
  'action 'mdcs-showerror-open-procedure
  'follow-link t
  'face 'link)

(defun mdcs-showerror-open-procedure (button)
  (save-excursion
    (beginning-of-line)
    (if (looking-at mdcs-link-error-re)
	(mdcs-showstat-display-inactive (match-string-no-properties 1) nil))))

(defun mdcs-showexception (raw)
  "Send the 'showexception' command to the debugger.
If RAW (prefix arg) is non-nil, display the raw output, 
otherwise run through StringTools:-FormatMessage."
  (interactive "P")
  (if raw
      (mdcs-showstat-send "showexception\n" nil nil nil #'mdcs-showerror-link)
    ;;(mdcs-showstat-eval-expr "showexception")
    (mdcs-showstat-eval-expr "printf(\"%s\\n\",StringTools:-FormatMessage(debugopts('lastexception')[2..]))")))

;;}}}
;;{{{ (*) Miscellaneous

(defun mdcs-goto-current-state ()
  "Move cursor to the current state in the active showstat buffer."
  (interactive) 
  (if mdcs-showstat-procname-inactive
      (mdcs-showstat-update mdcs-showstat-procname-active mdcs-showstat-state-active)
    (mdcs-goto-state mdcs-showstat-state)))

(defun mdcs-goto-state (state)
  "Move POINT to STATE.
STATE is a string corresponding to an integer."
  ;; Assume we are in the showstat buffer.
  (goto-char (point-min))
  (unless (re-search-forward (concat "^ *" state "[ *?]\\s-*") nil t)
    (ding)
    (message "cannot find state %s" state)))

(defun mdcs-toggle-truncate-lines (output-buffer)
  "Toggle the truncation of long lines.  If OUTPUT-BUFFER is
non-nil, do so in the `mdcs-showstat-output-buffer', otherwise do so in 
the `mdcs-showstat-buffer'."
  (interactive "P")
  (if output-buffer
      (with-current-buffer mdcs-showstat-output-buffer
	(toggle-truncate-lines))
    (toggle-truncate-lines)))

(defun mdcs-where (&optional depth)
  "Send the 'where' command to the debugger.
The optional DEPTH parameter is a positive integer that specifies
the number of activation levels to display."
  (interactive "P")
  (let ((cmd (if depth
		 (format "where %d\n" depth)
	       "where:\n")))
    (mdcs-showstat-send cmd nil nil nil
		       #'mdcs-highlight-where-output)))

(defconst mdcs-showstat-procname-re "^\\([^ \t\n]+\\): ")

(defun mdcs-highlight-where-output (beg end)
  "Font lock the names of called functions in the region from BEG to END,
which is the output of `mdcs-where'."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward mdcs-showstat-procname-re end t)
      (make-text-button (match-beginning 1) (match-end 1) :type 'mdcs-showstat-open-button))))

(define-button-type 'mdcs-showstat-open-button
  'help-echo "Open procedure"
  'action 'mdcs-showstat-open-procedure
  'follow-link t
  'face 'link)

(defun mdcs-showstat-open-procedure (button)
  (save-excursion
    (beginning-of-line)
    (unless (looking-at "TopLevel")
      (looking-at mdcs-showstat-procname-re)
      (let ((procname (match-string-no-properties 1))
	    (statement (buffer-substring-no-properties
			(match-end 0) (line-end-position))))
	(mdcs-showstat-display-inactive procname statement)))))


(defun mdcs-pop-to-mdcs-buffer ()
  "Pop to the Maple debugger buffer."
  (interactive)
  (pop-to-buffer mdcs-buffer))

(defun mdcs-help-debugger ()
  (interactive)
  (maplev-help-show-topic "debugger"))

(defun mdcs-info ()
  "Display the info page for Mdb."
  (interactive)
  (info "mdb"))

;;}}}

;;}}}
;;{{{ mode map

(defvar mdcs-showstat-mode-map
  (let ((map (make-sparse-keymap))
	(bindings
	 '((" " . mdcs-send-last-command)
	   ("A" . mdcs-show-args-as-equations)
	   ("a" . mdcs-args)
	   ("b" . mdcs-breakpoint)
	   ("B" . mdcs-breakpoint-cond)
	   ("c" . mdcs-cont)
	   ("C" . mdcs-debugger-clear-output)
	   ("d" . self-insert-command)
	   ("e" . mdcs-eval-and-display-expr)
	   ("E" . mdcs-eval-and-display-expr-global)
	   ("f" . self-insert-command)
	   ("h" . mdcs-help-debugger)
	   ("H" . mdcs-info)
	   ("i" . mdcs-into)
	   ("I" . mdcs-stopwhenif)
	   ("k" . mdcs-showstack)
	   ("K" . mdcs-where)
	   ("l" . mdcs-goto-current-state)
	   ("n" . mdcs-next)
	   ("o" . mdcs-outfrom)
	   ("p" . mdcs-showstop)
	   ("q" . mdcs-quit)
	   ("r" . mdcs-return)
	   ("R" . mdcs-stoperror)
	   ("s" . mdcs-step)
	   ("T" . mdcs-toggle-truncate-lines)
	   ("u" . mdcs-unstopat)
	   ("w" . mdcs-stopwhen-local)
	   ("W" . mdcs-stopwhen-global)
	   ("x" . mdcs-showexception)
	   ("X" . mdcs-showerror)
	   ("." . mdcs-eval-and-prettyprint)
	   ("\C-c\C-c" . mdcs-kill-maple)
	   ("\C-c\C-o" . mdcs-pop-to-mdcs-buffer)
	   )))
    (mapc (lambda (binding) (define-key map (car binding) (cdr binding)))
	  bindings)
    map))

;;}}}

;;{{{ menu

(defvar mdcs-showstat-menu nil)
(unless mdcs-showstat-menu
  (easy-menu-define
    mdcs-showstat-menu mdcs-showstat-mode-map
    "Menu for Mdb showstat mode"
    `("Showstat"

      ("Tracing"
       ["Continue"	mdcs-cont t]
       ["Next"		mdcs-next t]
       ["Into"		mdcs-into t]
       ["Outfrom"	mdcs-outfrom t]
       ["Step"		mdcs-step t]
       ["Return"	mdcs-return t]
       ["Quit"		mdcs-step t]
       ["Kill"		mdcs-kill-maple t])

      ("Stop points"
       ["Set breakpoint at point"    mdcs-breakpoint t]
       ["Clear breakpoint at point"  mdcs-unstopat t]
       ["Clear conditional breakpoint at point"  mdcs-breakpoint-cond t]
       "----"
       ["Set global watchpoint"      mdcs-stopwhen-global t]
       ["Set local watchpoint"       mdcs-stopwhen-local t]
       ["Set conditional watchpoint" mdcs-stopwhenif t] 
       ["Clear watchpoint"           mdcs-stopwhen-clear :keys "C-u w"]
       "----"
       ["Set watchpoint on error"    mdcs-stoperror t]
       ["Clear watchpoint on error"  mdcs-stoperror-clear :keys "C-u R"]
       "----"
       ["Show all stop points"       mdcs-showstop t]
       )

      ("Evaluation"
       ["Evaluate expression"			mdcs-eval-and-display-expr t]
       ["Evaluate expression in global context" mdcs-eval-and-display-expr-global t]
       ["Evaluate and prettyprint expression"	mdcs-eval-and-prettyprint t] )

      ("Information"
       ["Display parameters and values" mdcs-show-args-as-equations t]
       ["Show stack"			mdcs-showstack t]
       ["Show stack with arguments"	mdcs-where t]
       ["Show error"			mdcs-showerror t]
       ["Show error raw"		(mdcs-showerror t) t]
       ["Show exception"		mdcs-showexception t]
       ["Show exception raw"		(mdcs-showexception t) t] )

      ("Miscellaneous"
       ["Pop to Mdb buffer"        mdcs-pop-to-mdcs-buffer t]
       ["Clear debugger output"    mdcs-debugger-clear-output t]
       ["Toggle truncate lines"    mdcs-toggle-truncate-lines t]
       ["Toggle display of arguments"   mdcs-toggle-show-args t] )
      

      ("Help"
       ["Help Maple debugger"      mdcs-help-debugger t]
       ["Info for Mdb mode"        mdcs-info t])

      )))

;;}}}

;;{{{ showstat-mode

(define-derived-mode mdcs-showstat-mode maplev-proc-mode "showstat-mode"
  "Major mode for stepping through a debugged Maple procedure.

Tracing
-------
\\[mdcs-send-last-command] repeat the last tracing command
\\[mdcs-cont] (cont) continue execution until next stop point
\\[mdcs-next] (next) execute next statement at current nesting level
\\[mdcs-into] (into) execute next statement at any level in current procedure
\\[mdcs-outfrom] (outfrom) execute current statement sequence or until stop point
\\[mdcs-step] (step) execute next statement at any level
\\[mdcs-return] (return) continue executing until current procedure returns
\\[mdcs-quit] (quit) terminate debugging, return to mdb buffer
\\[mdcs-kill-maple] kill and restart the Maple process

Stop points
-----------
\\[mdcs-breakpoint] (stopat) set breakpoint at cursor
\\[mdcs-unstopat] (unstopat) clear breakpoint at cursor
\\[mdcs-breakpoint-cond] (stopat-cond) set conditional breakpoint at cursor

\\[mdcs-stopwhenif] (stopwhenif) set watchpoint on variable = value

\\[mdcs-stopwhen-local] (stopwhen) set watchpoint on local variable
C-u \\[mdcs-stopwhen-local] (stopwhen) clear watchpoint on local variable
\\[mdcs-stopwhen-global] (stopwhen) set watchpoint on global variable
C-u \\[mdcs-stopwhen-global] (stopwhen) clear watchpoint on global variable

\\[mdcs-showstop] (showstop) display all breakpoints

Information
-----------
\\[mdcs-show-args-as-equations] display the parameter names and values
\\[mdcs-args] display the arguments of the current procedure
\\[mdcs-help-debugger] display Maple debugger help page
\\[mdcs-showstack] (showstack) display abbreviated stack
\\[mdcs-where] (where) display stack of procedure calls
\\[mdcs-showerror] display the last error
C-u \\[mdcs-showerror] display the last error (raw)
\\[mdcs-showexception] display the last exception
C-u \\[mdcs-showexception] display the last exception (raw)

Evaluation
----------
\\[mdcs-eval-and-display-expr] evalute a Maple expression
C-u \\[mdcs-eval-and-display-expr] clear output then evalute a Maple expression
\\[mdcs-eval-and-display-expr-global] evalute a Maple expression in a global context
\\[mdcs-eval-and-prettyprint] evaluate and prettyprint a Maple expression
C-u \\[mdcs-eval-and-prettyprint] clear output then evaluate and prettyprint a Maple expression

Miscellaneous
-------------
\\[mdcs-pop-to-mdcs-buffer] pop to the mdb buffer
\\[mdcs-goto-current-state] move (return) cursor to current state
\\[mdcs-debugger-clear-output] clear the debugger output buffer
\\[mdcs-help-debugger] display help page for the Maple debugger
\\[mdcs-info] display info pages for the Maple debugger
\\[maplev-help-at-point] display a Maple help page
\\[maplev-proc-at-point] display a Maple procedure
\\[mdcs-toggle-truncate-lines] toggle whether to fold or truncate long lines
C-u \\[mdcs-toggle-truncate-lines] toggle truncation in debugger output buffer
"
  :group 'mdb


  (setq mdcs-showstat-procname ""
	mdcs-showstat-state ""
	mdcs-showstat-arrow-position nil)

  (and mdcs-showstat-menu (easy-menu-add mdcs-showstat-menu))

  (add-hook 'kill-buffer-hook '(lambda () (setq mdcs-update-showstat-p t)) nil 'local))

;;}}}

(provide 'mdcs-showstat)
