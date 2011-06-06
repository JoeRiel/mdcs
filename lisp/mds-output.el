;;; mds-output.el

;; Copyright (C) 2011 Joseph S. Riel, all rights reserved

;; Author:     Joseph S. Riel <jriel@maplesoft.com>
;; Created:    Jan 2011
;; Keywords:   maple, debugger
;;
;;; Commentary:

;; Code for the Maple Debugger Output buffer.

;;;

;;{{{ declarations

;; avoid compiler warnings

(declare-function mds--get-client-out-buf "mds")
(declare-function mds-showstat-send-showstat "mds-showstat")

;;}}}
;;{{{ faces

(defface mds-args-face
  '((((class color) (background dark)) (:foreground "lawn green")))
  "Face for stack arguments."
  :group 'mds-faces)

(defface mds-maple-error-face
  '((((class color) (background dark)) (:foreground "orange red")))
  "Face for Maple errors."
  :group 'mds-faces)

(defface mds-inactive-link-face
  '((((class color) (background dark)) (:foreground "cyan1")))
  "Face for inactive links in output buffer."
  :group 'mds-faces)

(defface mds-warning-face
  '((((class color) (background dark)) (:foreground "pink")))
  "Face for warning messages in output buffer."
  :group 'mds-faces)
;;}}}
;;{{{ constants

(defconst mds-output-procname-re "\\([^ \t\n]+\\)\\(: \\|$\\)"
  "Regular expression that matches the procedure name generated by
the debugger commands 'stack' and 'where'.  The name is stored in 
the first group.")

;;}}}
;;{{{ variables

(defvar mds-client nil
  "Client associated with the output buffer.")

(make-variable-buffer-local 'mds-client)

;;}}}

;;{{{ Create and clear buffer

(defun mds-output-create-buffer (client)
  "Create and return an `mds-output-buffer' with client CLIENT."
  (let ((buf (generate-new-buffer "*mds-output*")))
    (with-current-buffer buf
      (setq mds-client client)
      (font-lock-mode 't))
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
    (mds-put-face beg (1- (point-max)) face)))

(defun mds-insert-tag (tag)
  "Insert TAG as string with colon in the current buffer, at point."
  (let ((beg (point)))
    (insert (format "<%s>" (prin1-to-string tag)))
    (mds-put-face beg (point) 'font-lock-string-face)
    (insert ": ")))

(defun mds-put-face (beg end face)
  "Put FACE as a `font-lock-fact' text property on text between BEG and END."
  (put-text-property beg end 'font-lock-face face))

;;}}}

;;{{{ mds-output-display

(defun mds-output-display (buf msg &optional tag)
  "Display MSG in BUF, which is assumed an output buffer.
Optional TAG identifies the message type."
  (unless (string= msg "")
    (display-buffer buf)
    (with-selected-window (get-buffer-window buf)
      (with-current-buffer buf
	(goto-char (point-max))
	(let ((beg (point)))
	  (if (not tag)
	      (insert msg)
	    (cond
	     ((stringp tag)
	      ;; string tag (temporary)
	      (mds-insert-tag tag) (setq beg (point))
	      (insert msg))
	     ;; stack
	     ((eq tag 'stack)
	      (mds-insert-tag tag) (setq beg (point))
	      (insert msg)
	      (if (string= msg "TopLevel\n")
		  (mds-put-face beg (point) 'mds-inactive-link-face)
		(make-text-button beg (1- (point)) :type 'mds-output-view-proc-button)))
	     ;; where
	     ((eq tag 'where)
	      (mds-insert-tag tag) (setq beg (point))
	      (insert msg)
	      (goto-char beg)
	      (let ((toplev (looking-at "TopLevel")))
		(unless (re-search-forward ":\\( \\|$\\)" nil 't)
		  (error "no delimiter"))
		(if toplev
		    (mds-put-face beg (- (point) 2) 'mds-inactive-link-face)
		  (make-text-button beg (- (point) 2) :type 'mds-output-view-proc-button))))
	     ;; args
	     ((eq tag 'args) (mds-insert-and-font-lock msg 'mds-args-face tag))
	     ;; warning
	     ((eq tag 'warn) (mds-insert-and-font-lock msg 'mds-warning-face tag))
	     ;; maple error
	     ((eq tag 'maple-err) (mds-insert-and-font-lock msg 'mds-maple-error-face tag))
	     ;; maple parser error
	     ((eq tag 'parser-err) (mds-insert-and-font-lock msg 'mds-maple-error-face tag))
	     ;; unknown tag
	     ((and tag (symbolp tag))
	      (mds-insert-tag tag) (setq beg (point))
	      (insert msg))
	     )))
	(recenter -1)))))

;;}}}

;;{{{ Buttons

;; define button used to hyperlink procnames
(define-button-type 'mds-output-view-proc-button
  'help-echo "Open procedure"
  'action 'mds-output-view-procedure
  'follow-link t
  'face 'link)

;;}}}

;;{{{ mds-output-view-procedure

(defun mds-output-view-procedure (button)
  "Search at start of line for the Maple procedure name and
optional statement (call) from the output generated by the
'stack' and 'where' debugger commands.  Pass these to
`mds-showstat-send-showstat' to display the procedure."
  (save-excursion
    (beginning-of-line)
    ;; temporary to jump over tag
    (search-forward " ")
    (unless (looking-at "TopLevel")
      (looking-at mds-output-procname-re)
      (let ((procname (match-string-no-properties 1))
	    (statement (buffer-substring-no-properties
			(match-end 0) (line-end-position))))
	(mds-showstat-send-showstat procname statement)))))

;;}}}

(provide 'mds-output)

;; mds-output.el ends here
