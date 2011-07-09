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
  (require 'mds-out)
  (require 'mds-re)
  (require 'mds-wm))

;;{{{ declarations

;; avoid compiler warnings

(declare-function mds-client-out-buf "mds")
(declare-function mds-client-live-buf "mds")
(declare-function mds-client-dead-buf "mds")
(declare-function mds-client-send "mds")

;;}}}

;;{{{ customization

(defgroup mds nil
  "Maple Debugger Server."
  :group 'tools)

(defcustom mds-truncate-lines t
  "When non-nil, lines in showstat buffer are initially truncated."
  :group 'boolean
  :group 'mds)

;;{{{ (*) cursors

(defcustom mds-cursor-waiting 'hollow
  "Cursor used in showstat buffer when waiting for Maple to respond."
  :type 'symbol
  :group 'mds)

(defcustom mds-cursor-ready 'box
  "Cursor used in showstat buffer when ready for a user input."
  :type 'symbol
  :group 'mds)

;;}}}

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

(defvar mds-thisproc "thisproc" "Set to procname if Maple version < 14")

;; Removed because of timing issues that killed debugger
;; (defvar mds-show-args-on-entry t  "Non-nil means print the arguments to a procedure when entering it." )

(defvar mds-ss-arrow-position nil "Marker for state arrow.")
(defvar mds-ss-addr           nil "Address of displayed showstat procedure.")
(defvar mds-ss-last-debug-cmd nil "The previous debugger command.")
(defvar mds-ss-live	      nil "Store current state of active procedure")
(defvar mds-ss-procname       nil "Name of displayed showstat procedure.")
(defvar mds-ss-state          "1" "Current state of procedure.")
(defvar mds-ss-statement      ""  "String matching a statement; used by dead buffer")
(defvar mds-ss-trace          nil "Valid values are nil, cont, into, and step")
(defvar mds-ss-watch-alist    nil  "Alist for storing watch variables.  The keys are procedure names,the values are additional alists.")

;; Make variables buffer-local
(mapc #'make-variable-buffer-local
      '(mds-client
	mds-ss-addr
	mds-ss-arrow-position
	mds-ss-last-debug-cmd
	mds-ss-live
	mds-ss-procname
	mds-ss-state
	mds-ss-statement
	mds-ss-trace
	mds-ss-watch-alist
	mds-this-proc
	))

(add-to-list 'overlay-arrow-variable-list 'mds-ss-arrow-position)

;;}}}

;;{{{ send strings to maple client

;; Each of these functions send a string to the maple engine,
;; using mds-ss-send.  The second argument is a flag;
;; when non-nil it indicates that the string executes a command
;; in the debugged code.

(defun mds-ss-send-client (msg)
  (mds-client-send mds-client msg))

(defun mds-ss-eval-debug-code (cmd &optional hide)
  "Send CMD, with appended newline, to the Maple process and to the output buffer.
Echo the command to the output buffer unless HIDE is non-nil."
  (unless hide
    (mds-out-append-input (mds-client-out-buf mds-client) cmd 'mds-debugger-cmd-face))
  (mds-ss-send-client (concat cmd "\n")))

(defun mds-ss-eval-expr (expr)
  "Send EXPR, with appended newline, to the Maple process and to the output buffer.
This function is intended to be used for evaluating Maple expressions."
  (mds-out-append-input (mds-client-out-buf mds-client) expr 'mds-user-input-face)
  (mds-ss-send-client (concat expr "\n")))

(defun mds-ss-eval-proc-statement (cmd &optional save)
  "Send CMD, with appended newline, to the Maple process and to
the output buffer, tagged as 'cmd.  If SAVE is non-nil, the save
it as the last command `mds-ss-last-debug-cmd'.  Change
cursor type to `mds-cursor-waiting', which indicates we are
waiting for a response from Maple.  This function assumes we are
in the appropriate `mds-ss-buffer'.  This function is
to be used with commands that cause Maple to execute procedural code."
  (if save (setq mds-ss-last-debug-cmd cmd))
  (setq cursor-type mds-cursor-waiting)
  (unless (eobp) (forward-char)) ;; this indicates 'waiting' in tty Emacs, where cursor doesn't change
  (mds-out-display (mds-client-out-buf mds-client) cmd 'cmd)
  (mds-ss-send-client cmd))

;;}}}

;;{{{ buffer creation and update

;;{{{ (*) mds-ss-create-buffer

(defun mds-ss-create-buffer (client &optional alive)
  "Create and return a `mds-ss-buffer' buffer for CLIENT.
If ALIVE is non-nil, create a live buffer."
  (let ((buf (generate-new-buffer (if alive
				      "*mds-ss-live*"
				    "*mds-ss-dead*"))))
    (with-current-buffer buf
      (mds-ss-mode)
      (setq mds-client client
	    mds-ss-addr ""
	    mds-ss-arrow-position nil
	    mds-ss-live alive
	    mds-ss-procname ""
	    mds-ss-state "1"
	    mds-ss-trace nil
	    buffer-read-only 't)
      (if mds-truncate-lines
	  (toggle-truncate-lines 1)))
    buf))

;;}}}
;;{{{ (*) mds-ss-update

(defun mds-ss-update (buf addr procname state &optional statement)
  "Update the showstat buffer and the buffer local variables
`mds-ss-addr', `mds-ss-procname', and
`mds-ss-state'.  ADDR is the address of PROCNAME, which is
the name of the procedure, STATE is the current state; all are
strings.  If the buffer is already displaying PROCNAME, then just
move the arrow; otherwise call (maple) showstat to display the
new procedure."

  (with-current-buffer buf

    (unless mds-ss-trace
      ;; Revert cursor-type to ready status.
      (setq cursor-type mds-cursor-ready))

    (if (string= addr mds-ss-addr)
	;; procname has not changed.
	;; move the arrow
	(unless mds-ss-trace
	  (mds-ss-display-state state))

      ;; New procedure; send procname to the output buffer.
      (mds-out-display (mds-client-out-buf mds-client)
		       (format "<%s>\n%s" addr procname)
		       'addr-procname)

      (unless mds-ss-trace
	;; Call Maple showstat routine to update the showstat buffer.
	(mds-ss-send-client (format "mdc:-Debugger:-ShowstatAddr(%s)" addr))))
    
    ;; Update the buffer-local status
    (setq mds-ss-addr     addr
	  mds-ss-procname procname
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

      ;; Search technique suitable for multiline `statement'; that
      ;; typically occurs when the procedure on the stack occurs in
      ;; the controlling part (condition of conditional, etc) of the
      ;; statement.  Create str by catenating statements of procedure,
      ;; removing statement numbers, etc.  Store the position of the
      ;; beginning of each line in bols.

      (let (str bols line)
	(while (re-search-forward mds-ss-statement-re nil 'move)
	  (setq	bols (cons (length str) bols)
		line (match-string-no-properties 2)
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

;;{{{ mds-ss-view-dead-proc

(defun mds-ss-view-dead-proc (addr procname statement &optional state)
  "View procedure with name PROCNAME and address ADDR in the dead buffer.
If the optional string STATE is provided, use that as
the state number to display.  Otherwise, find the statement
number from STATEMENT."
  (with-current-buffer (mds-client-dead-buf mds-client)
    (unless (string= procname "")
      (if (string= procname mds-ss-procname)
	  ;; Already displaying the procedure; just update the arrow.
	  (mds-ss-display-state (or state
				    (mds-ss-determine-state statement)))

	;; Need to fetch from Maple.
	;; Set the buffer locals state info.
	(setq mds-ss-procname   procname
	      mds-ss-statement  statement)
   	(if state (setq mds-ss-state state))
	
	;; Update the dead buffer.
	(mds-ss-send-client (format "mdc:-Debugger:-ShowstatAddr(%s,'dead')" addr))))))

;;}}}

;;{{{ (*) mds-ss-send-showstat

(defun mds-ss-send-showstat (procname statement &optional state)
  "Query the client to send the showstat information for PROCNAME.
The output will be displayed in the dead showstat buffer.
Set the buffer-local variables `mds-ss-procname' and `mds-ss-statement'."
  (with-current-buffer (mds-client-dead-buf mds-client)
    (setq mds-ss-procname procname
	  mds-ss-statement statement)
    (if state
	(setq mds-ss-state state)))
  (mds-ss-send-client (format "mdc:-Format:-showstat(\"%s\")" procname)))

;;}}}
;;{{{ (*) mds-ss-display

(defun mds-ss-display (buf proc)
  "Insert Maple procedure PROC into the showstat buffer.
PROC is the output of a call to showstat.  Use and update
the buffer-local variables `mds-ss-state' and `mds-ss-statement'."
  (with-current-buffer buf

    (let ((buffer-read-only nil))
      ;; Delete old contents then insert the new.
      (erase-buffer)
      (insert proc)

      ;; Hide the address and assign `mds-ss-addr'
      (goto-char (point-min))
      (let ((addr-procname (mds-activate-addr-procname)))
	(setq mds-ss-addr (car addr-procname)
	      mds-ss-procname (cdr addr-procname)))

      ;; Update the mode-line; this adds the procname to the mode-line
      (mds-ss-set-mode-line mds-ss-procname)

      (cond
       (mds-ss-live
	;; Move the state arrow
	;; FIXME: only do if necessary
	(mds-ss-display-state mds-ss-state))

       ;; From here down, we are in the dead ss-buf
       ((string= "" mds-ss-statement)
	(setq mds-ss-state "1")
	(mds-ss-display-state "1"))

       ((string= "0" mds-ss-statement)
	(mds-ss-display-state mds-ss-state))

       ('t
	(let ((state (mds-ss-determine-state mds-ss-statement)))
	  (when (null state)
	    (ding)
	    (message "cannot find statement in procedure body"))
	  ;; save state and clear statement
	  (setq mds-ss-state state)
	  mds-ss-statement "")
	;; Move the state arrow
	(mds-ss-display-state mds-ss-state))))
    
    ;; Make buffer visible
    (if mds-ss-live
	(display-buffer buf)
      (mds-wm-display-dead mds-client))))

;;}}}
;;{{{ (*) mds-ss-display-state

(defun mds-ss-display-state (state)
  "Move the overlay arrow in the showstat buffer to STATE and
ensure that the buffer and line are visible.  If the `hl-line'
feature is present in this session, then highlight the line.
POINT is moved to the indentation of the current line."
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
	  (re-search-forward "^ *[1-9][0-9]*[ *?]? *" nil 'move))
	;; Ensure marker is visible in buffer.
	(set-window-point (get-buffer-window) (point))
	;; Ensure live-ss-buf is displayed.
	(mds-wm-display-live-buf))))


;;}}}

;;}}}

;;{{{ select maple expressions

(defun mds-ident-around-point-interactive (prompt &optional default complete)
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

;;}}}

;;{{{ commands

;; Define the interactive commands bound to keys

;;{{{ (*) Tracing

(defun mds-cont ()
  "Send the 'cont' (continue) command to the debugger."
  (interactive)
  (mds-ss-eval-proc-statement "cont" 'save))

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
  (if mds-ss-live
      (mds-ss-eval-expr "quit")
    (delete-window (get-buffer-window (mds-client-dead-buf mds-client)))))

(defun mds-return ()
  "Send the 'return' command to the debugger."
  (interactive)
  (mds-ss-eval-proc-statement "return" 'save))

(defun mds-step ()
  "Send the 'step' command to the debugger."
  (interactive)
  (mds-ss-eval-proc-statement "step" 'save))

(defun mds-cycle-trace ()
  "Cycle through the four tracing states: 'nil', 'into', 'step', and 'cont'.
If nil is selected, tracing does not occur.  If into is selected,
then only those procedures that have been instrumented are traced.
If 'step' is selected, then all procedures are traced.

To best use the results after tracing, turn off tracing mode (select nil),
then reenter the debugger from the client.  The hyperlinks in the 
output buffer are then active."
  (interactive)
  (setq mds-ss-trace
	(cond
	 ((null mds-ss-trace)           "cont")
	 ((string= mds-ss-trace "cont") "into")
	 ((string= mds-ss-trace "into") "step")
	 ((string= mds-ss-trace "step") nil)))
  (message (concat "tracing " (or mds-ss-trace "disabled"))))

;;}}}
;;{{{ (*) Stop points

(defun mds-ss-get-state ()
  (and (re-search-backward "^ *\\([1-9][0-9]*\\)\\([ *?]\\)" nil t)
       (match-string-no-properties 1)))

(defvar mds-ss-stoperror-history-list '("all" "traperror")
  "History list used by stoperror.")

(defvar mds-ss-stopwhen-history-list nil
  "History list used by stopwhen.")

(defun mds--query-stop-var (cmd type hist)
  "Prompt the user with \"CMD [TYPE]: \", using history list HIST."
  (read-from-minibuffer (format "%s [%s]: " cmd type)
			nil nil nil nil hist))

(defun mds-breakpoint ()
  "Set a breakpoint at the current/previous state."
  (interactive)
  (save-excursion
    (end-of-line)
    (let ((state (mds-ss-get-state))
	  (inhibit-read-only t))
      (if state
	  (progn
	    ;; FIXME: only replace a space, not a ?
	    (replace-match "*" nil nil nil 2)
	    (mds-ss-eval-debug-code
	     (format "debugopts('stopat'=[pointto(%s),%s])" mds-ss-addr state) 'hide))
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
		   mds-ss-addr state cond) 'hide))
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
				      mds-ss-addr var) 'hide))))

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
	   (format "debugopts('stopat'=[pointto(%s),-%s])" mds-ss-addr state) 'hide))
      (ding)
      (message "no breakpoint at this state"))))

;;}}}
;;{{{ (*) Evaluation

(defun mds-eval-and-prettyprint (expr)
  "Pretty-print EXPR.  This calls the Maple procedure 
mdc:-Format:-PrettyPrint to convert EXPR into a more useful display.
With optional prefix, clear debugger output before displaying."
  (interactive (list (mds-ident-around-point-interactive
		      "prettyprint: " "")))
  (if current-prefix-arg (mds-out-clear))
  (mds-ss-eval-expr (format "mdc:-Format:-PrettyPrint(%s)" expr)))

(defun mds-eval-and-display-expr (expr &optional suffix)
  "Evaluate a Maple expression, EXPR, display result and print optional SUFFIX.
If called interactively, EXPR is queried."
  (interactive (list (mds-ident-around-point-interactive
		      "eval: " "")))
  (if current-prefix-arg (mds-out-clear))
  (mds-ss-eval-expr expr))


(defun mds-eval-and-display-expr-global (expr)
  "Evaluate a Maple expression, EXPR, in a global context.  
If called interactively, EXPR is queried.
The result is returned in the message area."
  (interactive (list (mds-ident-around-point-interactive
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
  (if current-prefix-arg (mds-out-clear))
  (mds-out-append-input (mds-client-out-buf mds-client) "Args:" 'mds-args-face)
					; We need to use a global variable for the index,
					; one that isn't likely to appear in an expression.
					; Alternatively, a module export could be used.
  (mds-ss-send-client (format "mdc:-Format:-ArgsToEqs(%s, [seq([_params[`_|_`]],`_|_`=1.._nparams)],[_rest],[_options])\n"
			      mds-thisproc)))

(defconst mds--flush-left-arg-re "^\\([a-zA-Z%_][a-zA-Z0-9_]*\\??\\) =")

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
    (mds-ss-send-client "showerror\n")
    ))

(defconst mds-link-error-re "^\\[\\([^\"].*?\\), ")

(defun mds-showerror-link (beg end)
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (if (looking-at mds-link-error-re)
	(make-text-button (match-beginning 1) (match-end 1) 
			  :type 'mds-showerror-view-proc-button))))

(define-button-type 'mds-showerror-view-procbutton
  'help-echo "Open procedure"
  'action 'mds-showerror-view-procedure
  'follow-link t
  'face 'link)

(defun mds-showerror-view-procedure (button)
  (save-excursion
    (beginning-of-line)
    (if (looking-at mds-link-error-re)
	(mds-ss-send-showstat (match-string-no-properties 1) nil))))

(defun mds-showexception (raw)
  "Send the 'showexception' command to the debugger.
If RAW (prefix arg) is non-nil, display the raw output, 
otherwise run through StringTools:-FormatMessage."
  (interactive "P")
  (if raw
      (mds-ss-send-client "showexception\n")
    (mds-ss-eval-debug-code "mdc:-Debugger:-ShowException()")))

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
;;{{{ (*) Short cuts

(defun mds-send-last-command ()
  (interactive)
  ;; This uses the debugger history and only works when we haven't
  ;; done anything behind the scenes.  Need to save last command.
  (if mds-ss-last-debug-cmd
      (mds-ss-eval-proc-statement mds-ss-last-debug-cmd)
    (beep)
    (message "no previous command")))


;;}}}
;;{{{ (*) View

(defun mds-view ()
  (interactive)
  (ding)
  (message "viewing currently not supported"))

;;}}}

;;{{{ (*) Miscellaneous

(defun mds-goto-current-state ()
  (interactive)
  "Move cursor to the current state in the showstat buffer."
  (pop-to-buffer (mds-client-live-buf mds-client))
  (mds-ss-update (current-buffer)
		 mds-ss-addr
		 mds-ss-procname
		 mds-ss-state))


(defun mds-goto-state (state)
  "Move POINT to STATE.
STATE is a string corresponding to an integer."
  ;; Assume we are in the showstat buffer.
  (goto-char (point-min))
  (unless (re-search-forward (concat "^ *" state "[ *?]\\s-*") nil t)
    (ding)
    (message "cannot find state %s" state)))

(defun mds-toggle-truncate-lines (output-buffer)
  "Toggle the truncation of long lines.  If OUTPUT-BUFFER is
non-nil, do so in the `mds-out-buffer', otherwise do so in 
the `mds-ss-buffer'."
  (interactive "P")
  (if output-buffer
      (with-current-buffer (mds-client-out-buf mds-client)
	(toggle-truncate-lines))
    (toggle-truncate-lines)))

(defun mds-activate-procname-at-point ()
  (if (looking-at mds-ss-where-procname-re)
      (make-text-button (match-beginning 1) (match-end 1) 
			:type 'mds-ss-open-button)))

(defun mds-help-debugger ()
  "Display the Maple help page for the tty debugger."
  (interactive)
  (maplev-help-show-topic "debugger"))

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
	   ("h" . mds-help-debugger)
	   ("H" . mds-info)
	   ("i" . mds-into)
	   ("I" . mds-stopwhenif)
	   ("k" . mds-showstack)
	   ("K" . mds-where)
	   ("l" . mds-goto-current-state)
	   ("n" . mds-next)
	   ("o" . mds-outfrom)
	   ("p" . mds-showstop)
	   ("q" . mds-quit)
	   ("r" . mds-return)
	   ("R" . mds-stoperror)
	   ("s" . mds-step)
	   ("t" . mds-cycle-trace)
	   ("T" . mds-toggle-truncate-lines)
	   ("u" . mds-unstopat)
	   ("v" . mds-view)
	   ("w" . mds-stopwhen-local)
	   ("W" . mds-stopwhen-global)
	   ("x" . mds-showexception)
	   ("X" . mds-showerror)
	   ("." . mds-eval-and-prettyprint)
	   )))
    (mapc (lambda (binding) (define-key map (car binding) (cdr binding)))
	  bindings)
    map))

;;}}}

;;{{{ mode-line

(defun mds-ss-set-mode-line (proc)
  "Set the mode-line of an mds-ss buffer.
PROC is a string corresponding to the displayed procedure, 
it is displayed in bold after the mode name."
  (setq mode-line-format
	(list
	 mode-line-buffer-identification
	 "   "
	 mode-line-modes
	 "---"
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
    (setq face-remapping-alist (if off
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
    "Menu for Mds showstat mode"
    `("Showstat"

      ("Tracing"
       ["Continue"	mds-cont t]
       ["Next"		mds-next t]
       ["Into"		mds-into t]
       ["Outfrom"	mds-outfrom t]
       ["Step"		mds-step t]
       ["Return"	mds-return t]
       ["Trace"         mds-cycle-trace t]
       ["Quit"		mds-step t])

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
       )

      ("Evaluation"
       ["Evaluate expression"			mds-eval-and-display-expr t]
       ["Evaluate expression in global context" mds-eval-and-display-expr-global t]
       ["Evaluate and prettyprint expression"	mds-eval-and-prettyprint t] 
       ["Edit procedure"                        mds-edit-ss-to-proc t] )

      ("Information"
       ["Display parameters and values" mds-show-args-as-equations t]
       ["Show stack"			mds-showstack t]
       ["Show stack with arguments"	mds-where t]
       ["Show error"			mds-showerror t]
       ["Show error raw"		(mds-showerror t) t]
       ["Show exception"		mds-showexception t]
       ["Show exception raw"		(mds-showexception t) t] )

      ("Miscellaneous"
       ["Clear debugger output"         mds-out-clear t]
       ["Toggle truncate lines"         mds-toggle-truncate-lines t]
       ["Toggle display of arguments"   mds-toggle-show-args t]

       )

      ("Help"
       ["Help Maple debugger"      mds-help-debugger t]
       ["Info for Mds mode"        mds-info t])
      )))

;;}}}

;;{{{ showstat-mode

(define-derived-mode mds-ss-mode maplev-proc-mode "showstat-mode"
  "Major mode for stepping through a debugged Maple procedure.

Tracing
-------
\\[mds-send-last-command] repeat the last tracing command
\\[mds-cont] (cont) continue execution until next stop point
\\[mds-next] (next) execute next statement at current nesting level
\\[mds-into] (into) execute next statement at any level in current procedure
\\[mds-outfrom] (outfrom) execute current statement sequence or until stop point
\\[mds-step] (step) execute next statement at any level
\\[mds-return] (return) continue executing until current procedure returns
\\[mds-cycle-trace] select auto-trace mode
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
\\[mds-eval-and-display-expr] evalute a Maple expression
C-u \\[mds-eval-and-display-expr] clear output then evalute a Maple expression
\\[mds-eval-and-display-expr-global] evalute a Maple expression in a global context
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
\\[mds-toggle-truncate-lines] toggle whether to fold or truncate long lines
C-u \\[mds-toggle-truncate-lines] toggle truncation in debugger output buffer
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

;; mds-ss.el ends here

