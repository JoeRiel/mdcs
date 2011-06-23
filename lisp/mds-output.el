;;; mds-output.el

;; Copyright (C) 2011 Joseph S. Riel, all rights reserved

;; Author:     Joseph S. Riel <jriel@maplesoft.com>
;; Created:    Jan 2011
;; Keywords:   maple, debugger
;;
;;; Commentary:

;; Code for the Maple Debugger Output buffer.

;;;

(require 'mds-regexps)

;;{{{ declarations

;; avoid compiler warnings

(defvar mds-showstat-state nil)
(declare-function mds-goto-state "mds-showstat")
(declare-function mds--get-client-live-buf "mds")
(declare-function mds--get-client-out-buf "mds")
(declare-function mds-send-client "mds")
(declare-function mds-showstat-view-dead-proc "mds-showstat")
(declare-function mds-windows-display-dead "mds-windows")

;;}}}
;;{{{ faces

(defface mds-args-face
  '((((class color) (background dark)) (:foreground "lawn green")))
  "Face for stack arguments."
  :group 'mds-faces)

(defface mds-debugger-cmd-face
  '((((class color) (background dark)) (:foreground "yellow")))
  "Face for debugger commands."
  :group 'mds-faces)

(defface mds-debugger-info-face
  '((((class color) (background dark)) (:foreground "light green")))
  "Face for debugger information."
  :group 'mds-faces)


(defface mds-entry-procname-face
  '((((class color) (background dark)) (:foreground "magenta1" :underline t))
    (t (:underline t )))
  "Face for entry procnames (those printed on entering a procedure) in output buffer."
  :group 'mds-faces)

(defface mds-inactive-link-face
  '((((class color) (background dark)) (:foreground "cyan1")))
  "Face for inactive links in output buffer."
  :group 'mds-faces)

(defface mds-info-face
  '((((class color) (background dark)) (:foreground "sandy brown")))
  "Face for debugger information."
  :group 'mds-faces)

(defface mds-maple-error-face
  '((((class color) (background dark)) (:foreground "orange red")))
  "Face for Maple errors."
  :group 'mds-faces)

(defface mds-prompt-face
  '((((class color) (background dark)) (:foreground "gray40")))
  "Face for prompt."
  :group 'mds-faces)

(defface mds-user-input-face
  '((((class color) (background dark)) (:foreground "lime green")))
  "Face for user input (sort-of)."
  :group 'mds-faces)
  
(defface mds-warning-face
  '((((class color) (background dark)) (:foreground "pink")))
  "Face for warning messages in output buffer."
  :group 'mds-faces)

(defface mds-watch-face
  '((((class color) (background dark)) (:foreground "spring green")))
  "Face for watched variables in output buffer."
  :group 'mds-faces)

;;}}}
;;{{{ constants

(defconst mds-output-prompt-re "^(\\*\\([0-9]*\\)\\*)"
  "Regular expression to match prompt. Group one matches
the statement number; if empty then prompt corresponds
to user-input.")

(defconst mds-output-ss-line-re " *[0-9!*]*\\(.*\\)$"
  "Regular expression to match statement line in showstat buffer.
The first group matches the statement, with some indentation.")

;;}}}
;;{{{ variables

(defvar mds-client nil
  "Client associated with the output buffer.")

(defvar mds-output-track-input t
  "If non-nil, track (echo) the input line to the output after each command.")

(make-variable-buffer-local 'mds-client)
(make-variable-buffer-local 'mds-output-track-input)

;;}}}

;;{{{ Create and clear buffer

(defun mds-output-create-buffer (client)
  "Create and return an `mds-output-buffer' with client CLIENT."
  (let ((buf (generate-new-buffer "*mds-output*")))
    (with-current-buffer buf
      (mds-output-mode)
      (setq mds-client client))
    buf))

(defun mds-output-clear ()
  "Clear the debugger output buffer."
  (interactive)
  (let ((buf (mds--get-client-out-buf mds-client)))
    (when (bufferp buf)
      (with-current-buffer buf
	(delete-region (point-min) (point-max))))))

;;}}}

;;{{{ Text insertion

(defun mds-insert-and-font-lock (msg face &optional tag)
  "Insert string MSG into current buffer, at point, with font-lock FACE.
If optional TAG is present, insert it into the buffer before MSG."
  (and tag (mds-insert-tag tag))
  (let ((beg (point)))
    (insert msg)
    (mds-put-face beg (point) face)))

(defun mds-insert-tag (tag)
  "Insert TAG as string with colon in the current buffer, at point."
  (let ((beg (point)))
    (insert (format "<%s>" tag))
    (mds-put-face beg (point) 'font-lock-string-face)
    (insert ": ")))

(defun mds-put-face (beg end face)
  "Put FACE as a `font-lock-fact' text property on text between BEG and END."
  (put-text-property beg end 'font-lock-face face))

(defun mds-output-append-input (buf msg face)
  "Append string MSG and newline to the output buffer BUF.
MSG is considered external input from a user, so the state-number
in the prompt is cleared. The purpose is to prevent `mds-output-goto-line'
from going to a statement that does not correspond to procedure evaluation."
  (with-current-buffer buf
    (beginning-of-line)
    (if (looking-at mds-output-prompt-re)
	(replace-match "" 'fixedcase 'literal nil 1))
    (goto-char (point-max))
    (mds-insert-and-font-lock msg face)
    (insert "\n")))

;;}}}

;;{{{ mds-output-get-ss-line

;; FIXME: this belongs in mds-ss.el

(defun mds-output-get-ss-line ()
  "Return the current input line of the live showstat buffer."
  ;; FIXME :: need to go to current line
  (with-current-buffer (mds--get-client-live-buf mds-client)
    (save-excursion
      (mds-goto-state mds-showstat-state)
      (beginning-of-line)
      (if (looking-at mds-output-ss-line-re)
	  (match-string 1)
	""))))

;;}}}

;;{{{ mds-output-display

(defun mds-output-display (buf msg &optional tag)
  "Display MSG in BUF, which is assumed an output buffer.
Optional TAG identifies the message type."
  (unless (string= msg "") ;; FIXME: is empty-string possible?
    (display-buffer buf)
    (with-selected-window (get-buffer-window buf)
      (with-current-buffer buf
	(goto-char (point-max))
	(if (not tag)
	    (insert msg)
	  (let ((beg (point)))
	    (cond
	     ((stringp tag)
	      ;; string tag (temporary)
	      (mds-insert-tag tag) 
	      (insert msg))
	     
	     ((eq tag 'cmd)
	      ;; Command
	      (mds-insert-and-font-lock msg 'mds-debugger-cmd-face)
	      (if mds-output-track-input
		  (insert (mds-output-get-ss-line) "\n")
		(insert "\n")))
	     
	     ((eq tag 'prompt)
	      ;; Insert prompt, with statement number (msg) embedded.
	      ;; Goto to beginning of line and replace line, that way
	      ;; an existing prompt is replaced.
	      (beginning-of-line)
	      (setq beg (point))
	      (insert "(*" msg "*) ")
	      (mds-put-face beg (point) 'mds-prompt-face)
	      (delete-region (point) (line-end-position))
	      (let* ((live-buf (mds--get-client-live-buf mds-client))
		     (trace-mode (buffer-local-value 'mds-showstat-trace live-buf)))
		(when trace-mode
		  (if mds-output-track-input
		      (insert (buffer-local-value 'mds-showstat-statement live-buf)))
		  (insert "\n")
		  (mds-send-client mds-client (concat trace-mode "\n")))))

	     ((eq tag 'output)
	      (insert msg))

	     ((eq tag 'addr-procname)
	      (insert msg "\n")
	      (goto-char beg)
	      (mds-activate-addr-procname 'mds-output-view-proc))
	     
	     ((or (eq tag 'stack)
		  (eq tag 'where))
	      ;; stack or where
	      (insert msg)
	      (goto-char beg)
	      (mds-activate-addr-procname 'mds-output-goto-proc))

	     ((eq tag 'args)
	      ;; args
	      (mds-insert-and-font-lock msg 'mds-args-face))

	     ((eq tag 'printf)
	      ;; Insert msg, but no newline; this could screw-up format
	      ;; but need to trust user here. 
	      (insert msg))

	     ((eq tag 'warn)
	      ;; warning
	      (mds-insert-and-font-lock msg 'mds-warning-face))

	     ((eq tag 'maple-err)
	      ;; maple error
	      (mds-insert-and-font-lock msg 'mds-maple-error-face))
	     
	     ((eq tag 'parse-err) 
	      ;; maple debugger parse error
	      (mds-insert-and-font-lock msg 'mds-maple-error-face))

	     ((eq tag 'stop)
	      (ding) (sleep-for 0.2) (ding)
	      (mds-insert-and-font-lock msg 'mds-info-face))

	     ((or (eq tag 'watch-conds)
		  (eq tag 'watch-errs))
	      (mds-insert-and-font-lock msg 'mds-watch-face))

	     ((eq tag 'debug-info)
	      (mds-insert-and-font-lock msg 'mds-debugger-info-face))

	     ((and tag (symbolp tag))
	      ;; unknown tag
	      (mds-insert-tag tag) (setq beg (point))
	      (insert msg))
	     )))
	(recenter -1)))))

;;}}}

;;{{{ Buttons

;; The following two button types could functionally be replaced by
;; one, but the button type is used to distinguish them, so different
;; names are required (there is probably a better way).

;; define button used to hyperlink entry procname
(define-button-type 'mds-output-view-proc
  'help-echo "Open procedure"
  'action 'mds-output-view-proc
  'follow-link t
  'face 'mds-entry-procname-face)

;; define button used to hyperlink showstack/where procnames
(define-button-type 'mds-output-goto-proc
  'help-echo "Open procedure and goto statement"
  'action 'mds-output-view-proc
  'follow-link t
  'face 'link)

(defun mds-output-view-proc (button)
  "Search at start of line for the Maple procedure name and
optional statement (call) from the output generated by the
'stack' and 'where' debugger commands."
  (save-excursion
    (unless (looking-at "TopLevel")
      (forward-line -1)
      (when (looking-at mds--addr-procname-re)
	(forward-line)
	(let ((addr     (match-string-no-properties 2))
	      (procname (match-string-no-properties 3))
	      (statement (buffer-substring-no-properties (match-end 0) (line-end-position))))
	  (mds-output-display-proc addr procname statement (and (string= statement "")
								"1")))))))

;;}}}

;;{{{ Goto Source Line

;; Functions for moving to the appropriate line of code
;; in the dead-ss buffer.

(defun mds-output-goto-source-click (click)
  "Goto the source corresponding to the output at position of mouse CLICK."
  (interactive "e")
  (set-buffer (window-buffer (car (nth 1 click))))
  (mds-output-goto-source-line (posn-point (event-start click))))


(defun mds-output-goto-source-line (pos)
  "Goto the line of source corresponding to the output at
position POS in the output buffer.  This works by finding
the closest prompt, extracting the line number, then finding
the previous procedure name.  To make this less likely to fail,
evaluating expressions should remove (or tag) the corresponding
prompt so that it is later not matched."
  (interactive "d")
  (goto-char pos)
  (end-of-line)
  (if (re-search-backward mds-output-prompt-re nil 'move)
      (let ((state (match-string-no-properties 1)))
	(if (string= state "")
	    (message "position does not correspond to output from procedure")
	  (let ((addr-procname (mds-output-get-enclosing-addr-procname)))
	    (if addr-procname
		(mds-output-display-proc (car addr-procname) (cdr addr-procname) "0" state)
	      (ding)
	      (message "no procedure found in buffer")))))))

(defun mds-output-display-proc (addr procname statement &optional state)
  "Display procedure PROCNAME, with address ADDR, in the dead buffer.  Put arrow at STATEMENT.
If STATEMENT is the string \"0\", then use STATE." ;; FIXME may be a bad choice
  (when procname
    (mds-showstat-view-dead-proc addr procname statement state)
    (mds-windows-display-dead mds-client)))
  

(defun mds-output-get-enclosing-addr-procname ()
  "Search upwards from point to find the first embedded addr and procname,
an return the two as a cons cell, (addr . procname).  If none is
found, return nil, and leave point at beginning of buffer."
  (let (fnd)
    (while (and
	    (re-search-backward mds--addr-procname-re nil 'move)
	    (not (setq fnd (get-text-property (point) 'invisible)))))
    (if fnd 
	(cons (match-string-no-properties 2) (match-string-no-properties 3)))))

;;}}}

;;{{{ mds-output-mode (and mode-map)

(defvar mds-output-mode-map nil
  "Keymap for `mdb-output-mode'.")

(unless mds-output-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-3] 'mds-output-goto-source-click)
    (setq mds-output-mode-map map)))


(defun mds-output-mode ()
  "Major mode for the mds output buffer."
  (kill-all-local-variables)
  (setq mode-name "mds-output")
  (use-local-map mds-output-mode-map)
  (font-lock-mode t)
  (run-hooks 'mds-output-mode-hook))

;;}}}

(provide 'mds-output)

;; mds-output.el ends here
