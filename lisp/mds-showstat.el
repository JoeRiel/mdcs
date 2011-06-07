;;; mds-showstat.el --- mds-showstat-mode

;; Copyright (C) 2011 Joseph S. Riel, all rights reserved

;; Author:     Joseph S. Riel <jriel@maplesoft.com>
;; Created:    May 2011
;; Keywords:   maple, debugger
;;
;;; Commentary:

;; This file contains the source for the showstat functions.

;;; Code:

(require 'maplev)
(require 'mds-output)
(require 'mds-windows)
(eval-when-compile
  (require 'hl-line))

;;{{{ declarations

;; avoid compiler warnings

(declare-function mds--get-client-out-buf "mds")
(declare-function mds--get-client-live-buf "mds")
(declare-function mds--get-client-dead-buf "mds")
(declare-function mds-send-client "mds")

;;}}}

;;{{{ customization

(defgroup mds nil
  "Maple Debugger Server."
  :group 'tools)

(defcustom mds-truncate-lines 't
  "When non-nil, lines in showstat buffer are initially truncated."
  :group 'mds)


;;{{{ (*) faces

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
(defconst mds-showstat-where-procname-re "^\\([^ \t\n]+\\): "
  "Match the procname printed by the where command.
It is flush-left, presumably contains no spaces, and is terminated
with a colon.  The colon is omitted from the group-one match.
A more precise regular expression would allow spaces inside backquotes,
however, such an abomination should break something.")

(defconst mds-showstat-procname-assignment-re "^\\([^ \t\n]+\\) :="
  "Match an assignment to a procedure in the showstat buffer.
The procname is flush left.  See diatribe in `mds-showstat-where-procname-re'.")

;;}}}
;;{{{ variables

(defvar mds-thisproc "thisproc" "Set to procname if Maple version < 14")

(defvar mds-show-args-on-entry t  "Non-nil means print the arguments to a procedure when entering it." )

(defvar mds-showstat-arrow-position nil "Marker for state arrow.")
(defvar mds-client                  nil "Client structure associated with buffer.")
(defvar mds-showstat-last-debug-cmd ""  "The previous debugger command.")
(defvar mds-showstat-live	    nil "Store current state of active procedure")
(defvar mds-showstat-procname       nil "Name of displayed showstat procedure.")
(defvar mds-showstat-state          "1" "Current state of procedure.")
(defvar mds-showstat-statement      ""  "String matching a statement; used by dead buffer")
(defvar mds-showstat-watch-alist    nil  "Alist for storing watch variables.  The keys are procedure names,the values are additional alists.")

;; Make variables buffer-local
(mapc #'make-variable-buffer-local
      '(mds-client
	mds-showstat-arrow-position
	mds-showstat-last-debug-cmd
	mds-showstat-live
	mds-showstat-procname
	mds-showstat-state
	mds-showstat-statement
	mds-showstat-watch-alist
	mds-this-proc
	))

(add-to-list 'overlay-arrow-variable-list 'mds-showstat-arrow-position)

;;}}}

;;{{{ send strings to maple client

;; Each of these functions send a string to the maple engine,
;; using mds-showstat-send.  The second argument is a flag;
;; when non-nil it indicates that the string executes a command
;; in the debugged code.

(defun mds-showstat-send-client (msg)
  (mds-send-client mds-client msg))

(defun mds-showstat-send-command (cmd)
  "Send CMD, with appended newline, to the Maple process.
Save CMD in `mds-showstat-last-debug-cmd'.  Change cursor type to
`mds-cursor-waiting', which indicates we are waiting for a
response from Maple.  This function assumes we are in the
appropriate `mds-showstat-buffer'."
  (setq mds-showstat-last-debug-cmd cmd)
  (mds-output-display (mds--get-client-out-buf mds-client) cmd 'cmd)
  ;;(sleep-for 0.1)
  (setq cursor-type mds-cursor-waiting)
  (forward-char) ;; this indicates 'waiting' in tty Emacs, where cursor doesn't change
  (mds-showstat-send-client cmd))

(defun mds-showstat-eval-expr (expr)
  "Send EXPR, with appended newline, to the Maple process."
  (mds-showstat-send-client (concat expr "\n")))

;;}}}

;;{{{ Buffer creation and update

;;{{{ (*) mds-showstat-create-buffer

(defun mds-showstat-create-buffer (client &optional alive)
  "Create and return a `mds-showstat-buffer' buffer for CLIENT.
If ALIVE is non-nil, create a live buffer."
  (let ((buf (generate-new-buffer (if alive
				      "*mds-showstat-live*"
				    "*mds-showstat-dead*"))))
    (with-current-buffer buf
      (mds-showstat-mode)
      (setq mds-client client
	    mds-showstat-arrow-position nil
	    mds-showstat-live alive
	    mds-showstat-procname ""
	    mds-showstat-state "1"
	    buffer-read-only 't)
      (if mds-truncate-lines
	  (toggle-truncate-lines 1)))
    buf))

;;}}}
;;{{{ (*) mds-showstat-update

(defun mds-showstat-update (buf procname state)
  "Update the showstat buffer, `mds-showstat-procname', and
`mds-showstat-state'.  PROCNAME is the name of the procedure,
STATE is the current state; both are strings.  If the buffer is
already displaying PROCNAME, then just move the arrow; otherwise
call (maple) showstat to display the new procedure."

  (with-current-buffer buf

    ;; save the procname and the state
    (setq mds-showstat-state state)

    ;; Revert cursor-type to ready status.
    (setq cursor-type mds-cursor-ready)
    (let ((at-first-state (string= state "1")))
      (if (and (equal procname mds-showstat-procname)
	       (not at-first-state))
	  
	  ;; procname has not changed.
	  ;; Move the arrow
	  (mds-showstat-display-state state)

	;; procname has changed (or we entered it recursively).
	;; Update the variable and set the mode-line
	(setq mds-showstat-procname procname)
	(mds-showstat-set-mode-line procname)

	;; Send procname to the output buffer
	(mds-output-display
	 (mds--get-client-out-buf mds-client)
	 procname
	 'procname
	 )

	;; Update the output buffer with procname.
	;; (propertize procname
	;; 		   'face (if at-first-state
	;; 			     'mds-face-procname-entered
	;; 			   'mds-face-procname-cont))))
	;; Display arguments if we just entered the procedure.
	;;(if (and mds-show-args-on-entry at-first-state)
	;;      (mds-show-args-as-equations))
	
	;; Save procname, then update the showstat buffer.
	(setq mds-showstat-procname procname)
	(mds-showstat-send-client "showstat")))))
;;}}}


(defun mds-showstat-determine-state (statement)
  "Search buffer for STATEMENT and return the statement number."
  (goto-char (point-min))
  (if (not (search-forward (concat " " mds-showstat-statement) nil t))
      (error "cannot find statement in procedure body")
    ;; clear statement
    (setq mds-showstat-statement "")
    (mds-showstat-get-state)))

(defun mds-showstat-view-dead-proc (procname statement &optional state)
  (with-current-buffer (mds--get-client-dead-buf mds-client)
    (if (and (string= procname mds-showstat-procname)
	     (not (string= procname "")))
	;; Already displaying the procedure
	(if state
	    (mds-showstat-display-state state)
	  (mds-showstat-display-state (mds-showstat-determine-state statement)))
      
      ;; Need to fetch from Maple
      ;; Set these buffer locals so they can be used by ...
      (setq mds-showstat-procname procname
	    mds-showstat-statement statement)
      (if state
	  (setq mds-showstat-state state)))
    (mds-showstat-send-client (format "mdc:-Format:-showstat(\"%s\")" procname))))

;;{{{ (*) mds-showstat-send-showstat

(defun mds-showstat-send-showstat (procname statement &optional state)
  "Query the client to send the showstat information for PROCNAME.
The output will be displayed in the dead showstat buffer.
Set the buffer-local variables `mds-showstat-procname' and `mds-showstat-statement'."
  (with-current-buffer (mds--get-client-dead-buf mds-client)
    (setq mds-showstat-procname procname
	  mds-showstat-statement statement)
    (if state
	(setq mds-showstat-state state)))
  (mds-showstat-send-client (format "mdc:-Format:-showstat(\"%s\")" procname)))

;;}}}
;;{{{ (*) mds-showstat-display

(defun mds-showstat-display (buf proc)
  "Insert Maple procedure PROC into the showstat buffer.
PROC is the output of a call to showstat.  Use and update
the buffer-local variables `mds-showstat-state' and `mds-showstat-statement'."
  (with-current-buffer buf

    (let ((buffer-read-only nil))
      ;; Delete old contents then insert the new.
      (delete-region (point-min) (point-max))
      (insert proc)
      ;; Delete first char (\n)
      (goto-char (point-min))
      (if (looking-at "\n")
	  (delete-char 1))

      ;; Update `mds-showstat-procname' by extracting the value from
      ;; the inserted text. It is frequently already correct, because
      ;; was assigned in mds-showstat-send-showstat, however, it
      ;; will not be assigned if the update was via
      ;; `mds-eval-and-prettyprint'.

      (if (looking-at mds-showstat-procname-assignment-re)
	  (setq mds-showstat-procname (match-string 1)))

      ;; Update the mode-line; this adds the procname to the mode-line
      (mds-showstat-set-mode-line mds-showstat-procname)

      ;; Update state information and, if appropriate, move the arrow.
      (cond
       (mds-showstat-live
	;; Move the state arrow
	(mds-showstat-display-state mds-showstat-state))

       ((string= "" mds-showstat-statement)
	(setq mds-showstat-state 0))  ;; illegal state

       ((string= "0" mds-showstat-statement)
	(mds-showstat-display-state mds-showstat-state))

       ('t
	(if (not (search-forward (concat " " mds-showstat-statement) nil t))
	    ;; this has never occurred.
	    (error "cannot find statement in procedure body")
	  ;; save state and clear statement
	  (setq mds-showstat-state (mds-showstat-get-state)
		mds-showstat-statement "")
	  ;; Move the state arrow
	  (mds-showstat-display-state mds-showstat-state))))
      
      ;; Make buffer visible
      (if mds-showstat-live
	  (display-buffer buf)
	(mds-windows-display-dead mds-client)))))

;;}}}
;;{{{ (*) mds-showstat-display-state

(defun mds-showstat-display-state (state)
  "Move the overlay arrow in the showstat buffer to STATE and
ensure that the buffer and line are visible.  If the `hl-line'
feature is present in this session, then highlight the line.
POINT is moved to the indentation of the current line."
  (let ((buffer-read-only nil))
    ;; Find the location of STATE in the buffer.
    (goto-char (point-min))
    (when (re-search-forward (concat "^ *" state "[ *?]\\(!\\)?") nil 't)
      ;; Remove the bang, which showstat uses to mark the current state.
      (if (match-string 1)
	  (replace-match " " nil nil nil 1))
      ;; Move the arrow marker to the left margin of the state.
      (beginning-of-line)
      (or mds-showstat-arrow-position
	  (setq mds-showstat-arrow-position (make-marker)))
      (set-marker mds-showstat-arrow-position (point))
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
    (set-window-point (get-buffer-window) (point))))

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

;; this is not used

(defun mds--select-expression-at-point (prompt &optional default complete)
  (if t ;; (mds-showstat-bol)
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

(defun mds-cont ()
  "Send the 'cont' (continue) command to the debugger."
  (interactive)
  (mds-showstat-send-command "cont"))

(defun mds-into ()
  "Send the 'into' command to the debugger."
  (interactive)
  (mds-showstat-send-command "into"))

(defun mds-next ()
  "Send the 'next' command to the debugger."
  (interactive)
  (mds-showstat-send-command "next"))

(defun mds-outfrom ()
  "Send the 'outfrom' command to the debugger."
  (interactive)
  (mds-showstat-send-command "outfrom"))

(defun mds-quit ()
  "Send the 'quit' command to the debugger."
  (interactive)
  (mds-showstat-send-command "quit"))

(defun mds-return ()
  "Send the 'return' command to the debugger."
  (interactive)
  (mds-showstat-send-command "return"))

(defun mds-step ()
  "Send the 'step' command to the debugger."
  (interactive)
  (mds-showstat-send-command "step"))

;;}}}
;;{{{ (*) Stop points

(defun mds-showstat-get-state ()
  (and (re-search-backward "^ *\\([1-9][0-9]*\\)\\([ *?]\\)" nil t)
       (match-string-no-properties 1)))

(defvar mds-showstat-stoperror-history-list '("all" "traperror")
  "History list used by stoperror.")

(defvar mds-showstat-stopwhen-history-list nil
  "History list used by stopwhen.")

(defun mds--query-stop-var (cmd type hist)
  "Prompt the user with \"CMD [TYPE]: \", using history list HIST."
  (read-from-minibuffer (format "%s [%s]: " cmd type)
			nil nil nil nil hist))

(defun mds-breakpoint ()
  "Set a breakpoint at the current/previous state."
  (interactive)
  ;; Assume we are in the showstat buffer
  ;; TODO: An alternative is to move outward from
  ;; the Maple structure.  
  ;; If at an elif or else, then move ...
  (save-excursion
    (end-of-line)
    (let ((state (mds-showstat-get-state))
	  (inhibit-read-only t))
      (if state
	  (progn
	    ;; FIXME: only replace a space, not a ?
	    (replace-match "*" nil nil nil 2)
	    (mds-showstat-eval-expr (format "mdc:-Debugger:-stopat(\"%s\",%s)" mds-showstat-procname state)))
	(ding)
	(message "no previous state in buffer")))))

(defun mds-breakpoint-cond ()
  "Set a conditional breakpoint at the current/previous state."
  (interactive)
  ;; Assume we are in the showstat buffer
  (save-excursion
    (end-of-line)
    (if (re-search-backward "^ *\\([1-9][0-9]*\\)\\([ *?]\\)" nil t)
	(let ((state (match-string-no-properties 1))
	      (inhibit-read-only t)
	      (cond (mds--query-stop-var "stopat-cond" "condition" 'mds-showstat-stopwhen-history-list)))
	  (replace-match "?" nil nil nil 2)
	  (mds-showstat-eval-expr (format "debugopts('stopat'=[%s,%s,%s])" mds-thisproc state cond)))
      (ding)
      (message "no previous state in buffer"))))


(defun mds-stoperror (clear)
  "Query for and set or clear, if CLEAR is non-nil, a watchpoint on an error."
  (interactive "P")
  (let* ((cmd (if clear "unstoperror" "stoperror"))
	 (err (mds--query-stop-var cmd "errMsg" 'mds-showstat-stoperror-history-list)))
    (mds-showstat-eval-expr (format "%s %s" cmd err))))

(defun mds-stoperror-clear ()
  "Query for and clear a watchpoint on an error."
  (interactive)
  (mds-stoperror 'clear))


(defun mds-stopwhen-local (clear)
  "Set or clear, if CLEAR is non-nil, watchpoint on a variable.
Query for local variable, using symbol at point as default."
  (interactive "P")
  (let* ((cmd (if clear "unstopwhen" "stopwhen"))
	 (var (mds--query-stop-var cmd "var" 'mds-showstat-stopwhen-history-list)))
    (mds-showstat-eval-expr (format "%s procname %s" cmd var))))

(defun mds-stopwhen-global (clear)
  "Set or clear, if CLEAR is non-nil, watchpoint on a variable.
Query for global variable, using symbol at point as default."
  (interactive "P")
  (let* ((cmd (if clear "unstopwhen" "stopwhen"))
	 (var (mds--query-stop-var cmd "var" 'mds-showstat-stopwhen-history-list)))
    (mds-showstat-eval-expr (format "%s %s" cmd var))))

(defun mds-stopwhenif ()
  "Query and set a conditional watchpoint on a variable."
  (interactive)
  (let* ((cmd "stopwhenif")
	 (var (mds--query-stop-var cmd "var" 'mds-showstat-stopwhen-history-list))
	 (val (read-string "value: ")))
    (mds-showstat-eval-expr (format "%s(%s,%s)" cmd var val))))

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
	  (mds-showstat-eval-expr (format "unstopat %s %s" mds-showstat-procname state)))
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
  (if current-prefix-arg (mds-output-clear))
  (mds-showstat-send-client (format "mdc:-Format:-PrettyPrint(%s)\n" expr)))

(defun mds-eval-and-display-expr (expr &optional suffix)
  "Evaluate a Maple expression, EXPR, display result and print optional SUFFIX.
If called interactively, EXPR is queried."
  (interactive (list (mds-ident-around-point-interactive
		      "eval: " "")))
  (if current-prefix-arg (mds-output-clear))
  (mds-showstat-send-client (concat expr "\n")))


(defun mds-eval-and-display-expr-global (expr)
  "Evaluate a Maple expression, EXPR, in a global context.  
If called interactively, EXPR is queried.
The result is returned in the message area."
  (interactive (list (mds-ident-around-point-interactive
		      "global eval: " "")))
  (if current-prefix-arg (mds-output-clear))
  (mds-eval-and-display-expr (concat "statement " expr)))

;;}}}
;;{{{ (*) Information

(defun mds-args ()
  "Display the arguments of the current procedure."
  (interactive)
  (mds-showstat-eval-expr "args"))


(defun mds-show-args-as-equations ()
  "Display the parameters and arguments of the current Maple procedure as equations."
  (interactive)
  (if current-prefix-arg (mds-output-clear))
					; We need to use a global variable for the index,
					; one that isn't likely to appear in an expression.
					; Alternatively, a module export could be used.
  (mds-showstat-send-client (format "mdc:-Format:-ArgsToEqs(%s, [seq([_params[`_|_`]],`_|_`=1.._nparams)],[_rest],[_options])\n"
				  mds-thisproc)))

(defconst mds--flush-left-arg-re "^\\([a-zA-Z%_][a-zA-Z0-9_]*\\??\\) =")

(defun mds-prettify-args-as-equations (beg end)
  "Font lock the argument names in the region from BEG to END."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward mds--flush-left-arg-re end t)
      (put-text-property (match-beginning 1) (match-end 1) 'face 'mds-face-arg))))

(defun mds-showstack ()
  "Send the 'showstack' command to the debugger.
Note that the string displayed in the echo area has the current
procedure stripped from it."
  (interactive)
  (mds-showstat-eval-expr "showstack"))

(defun mds-showstop ()
  "Send the 'showstop' command to the debugger."
  (interactive)
  (mds-showstat-eval-expr "showstop"))

(defun mds-showerror (fmt)
  "Send the 'showerror' command to the debugger.
If FMT (prefix arg) is non-nil, display the formatted message,
otherwise hyperlink the raw message."
  (interactive "P")
  (if fmt
      (mds-showstat-eval-expr "mdc:-Debugger:-ShowError()")
    (mds-showstat-send-client "showerror\n")
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
	(mds-showstat-send-showstat (match-string-no-properties 1) nil))))

(defun mds-showexception (raw)
  "Send the 'showexception' command to the debugger.
If RAW (prefix arg) is non-nil, display the raw output, 
otherwise run through StringTools:-FormatMessage."
  (interactive "P")
  (if raw
      ;;(mds-showstat-eval-expr "showexception")
      (mds-showstat-send-client "showexception\n")
    (mds-showstat-eval-expr "mdc:-Debugger:-ShowException()")))

;;}}}
;;{{{ (*) Short cuts

(defun mds-send-last-command ()
  (interactive)
  ;; This uses the debugger history and only works when we haven't
  ;; done anything behind the scenes.  Need to save last command.
  (mds-showstat-send-client "\n"))
  

;;}}}
;;{{{ (*) View

(defun mds-view ()
  (interactive)
  (beep)
  (message "viewing currently not supported"))

;;}}}

;;{{{ (*) Miscellaneous

(defun mds-goto-current-state ()
  (interactive)
  "Move cursor to the current state in the showstat buffer."
  (pop-to-buffer (mds--get-client-live-buf mds-client))
  (mds-showstat-update (current-buffer) mds-showstat-procname mds-showstat-state))
   

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
non-nil, do so in the `mds-output-buffer', otherwise do so in 
the `mds-showstat-buffer'."
  (interactive "P")
  (if output-buffer
      (with-current-buffer (mds--get-client-out-buf mds-client)
	(toggle-truncate-lines))
    (toggle-truncate-lines)))

(defun mds-where (&optional depth)
  "Send the 'where' command to the debugger.
The optional DEPTH parameter is a positive integer that specifies
the number of activation levels to display."
  (interactive "P")
  (let ((cmd (if depth
		 (format "where %d\n" depth)
	       "where\n")))
    (mds-showstat-send-client cmd)))

(defun mds-activate-procname-at-point ()
  (if (looking-at mds-showstat-where-procname-re)
      (make-text-button (match-beginning 1) (match-end 1) 
			:type 'mds-showstat-open-button)))


(defun mds-highlight-where-output (beg end)
  "Font-lock the names of called functions in the region from BEG to END,
which is the output of `mds-where'."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward mds-showstat-where-procname-re end t)
      (make-text-button (match-beginning 1) (match-end 1) 
			:type 'mds-showstat-open-button))))

(define-button-type 'mds-showstat-open-button
  'help-echo "Open procedure"
  'action 'mds-showstat-open-procedure
  'follow-link t
  'face 'link)

(defun mds-showstat-open-procedure (button)
  "Open the procedure"
  (save-excursion
    (beginning-of-line)
    (unless (looking-at "TopLevel")
      (looking-at mds-showstat-where-procname-re)
      (let ((procname (match-string-no-properties 1))
	    (statement (buffer-substring-no-properties
			(match-end 0) (line-end-position))))
	(mds-showstat-send-showstat procname statement)))))


;; (defun mds-pop-to-mds-buffer ()
;;   "Pop to the Maple debugger buffer."
;;   (interactive)
;;   (pop-to-buffer mds-buffer))

(defun mds-help-debugger ()
  (interactive)
  (maplev-help-show-topic "debugger"))

(defun mds-info ()
  "Display the info page for Mds."
  (interactive)
  (info "mds"))

;;}}}

;;}}}
;;{{{ mode map

(defvar mds-showstat-mode-map
  (let ((map (make-sparse-keymap))
	(bindings
	 '(
	   (" " . mds-send-last-command)
	   ("A" . mds-show-args-as-equations)
	   ("a" . mds-args)
	   ("b" . mds-breakpoint)
	   ("B" . mds-breakpoint-cond)
	   ("c" . mds-cont)
	   ("C" . mds-output-clear)
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
	   ("T" . mds-toggle-truncate-lines)
	   ("u" . mds-unstopat)
	   ("v" . mds-view)
	   ("w" . mds-stopwhen-local)
	   ("W" . mds-stopwhen-global)
	   ("x" . mds-showexception)
	   ("X" . mds-showerror)
	   ("." . mds-eval-and-prettyprint)
	   ("\C-c\C-c" . mds-kill-maple)
	   ;;("\C-c\C-o" . mds-pop-to-mds-buffer)
	   )))
    (mapc (lambda (binding) (define-key map (car binding) (cdr binding)))
	  bindings)
    map))

;;}}}

;;{{{ mode-line

(defun mds-showstat-set-mode-line (proc)
  "Set the mode-line of an mds-showstat buffer.
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

;;}}}

;;{{{ menu

(defvar mds-showstat-menu nil)
(unless mds-showstat-menu
  (easy-menu-define
    mds-showstat-menu mds-showstat-mode-map
    "Menu for Mds showstat mode"
    `("Showstat"

      ("Tracing"
       ["Continue"	mds-cont t]
       ["Next"		mds-next t]
       ["Into"		mds-into t]
       ["Outfrom"	mds-outfrom t]
       ["Step"		mds-step t]
       ["Return"	mds-return t]
       ["Quit"		mds-step t]
       ["Kill"		mds-kill-maple t])

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
       ["Evaluate and prettyprint expression"	mds-eval-and-prettyprint t] )

      ("Information"
       ["Display parameters and values" mds-show-args-as-equations t]
       ["Show stack"			mds-showstack t]
       ["Show stack with arguments"	mds-where t]
       ["Show error"			mds-showerror t]
       ["Show error raw"		(mds-showerror t) t]
       ["Show exception"		mds-showexception t]
       ["Show exception raw"		(mds-showexception t) t] )

      ("Miscellaneous"
       ;;["Pop to Mds buffer"        mds-pop-to-mds-buffer t]
       ["Clear debugger output"    mds-output-clear t]
       ["Toggle truncate lines"    mds-toggle-truncate-lines t]
       ["Toggle display of arguments"   mds-toggle-show-args t] )
      

      ("Help"
       ["Help Maple debugger"      mds-help-debugger t]
       ["Info for Mds mode"        mds-info t])

      )))

;;}}}

;;{{{ showstat-mode

(define-derived-mode mds-showstat-mode maplev-proc-mode "showstat-mode"
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
\\[mds-quit] (quit) terminate debugging, return to mds buffer
\\[mds-kill-maple] kill and restart the Maple process

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
\\[mds-output-clear] clear the debugger output buffer
\\[mds-help-debugger] display help page for the Maple debugger
\\[mds-info] display info pages for the Maple debugger
\\[maplev-help-at-point] display a Maple help page
\\[maplev-proc-at-point] display a Maple procedure
\\[mds-toggle-truncate-lines] toggle whether to fold or truncate long lines
C-u \\[mds-toggle-truncate-lines] toggle truncation in debugger output buffer
"
  :group 'mds


  (setq mds-showstat-procname ""
	mds-showstat-state ""
	mds-showstat-arrow-position nil)

  (and mds-showstat-menu (easy-menu-add mds-showstat-menu))

  (add-hook 'kill-buffer-hook '(lambda () (setq mds-update-showstat-p t)) nil 'local))

;;}}}

(provide 'mds-showstat)

;; mds-showstat.el ends here