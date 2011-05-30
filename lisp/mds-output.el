;;; mds-output.el

;; Copyright (C) 2011 Joseph S. Riel, all rights reserved

;; Author:     Joseph S. Riel <jriel@maplesoft.com>
;; Created:    Jan 2011
;; Keywords:   maple, debugger
;;
;;; Commentary:

;; 

;;;

;;{{{ faces
(defface mds-warning-face
  '((((class color) (background dark)) (:foreground "pink")))
  "Face for warning messages in output buffer."
  :group 'mds-faces)

(defface mds-inactive-link-face
  '((((class color) (background dark)) (:foreground "cyan1")))
  "Face for inactive links in output buffer."
  :group 'mds-faces)
;;}}}
;;{{{ variables

(defvar mds-client nil
  "Client associated with the output buffer.")

(make-variable-buffer-local 'mds-client)

;;}}}

(defsubst mds--get-client-out-buf  (client) (nth 4 client))


(defun mds-output-get-buffer ()
  (mds--get-client-out-buf mds-client))

(defun mds-output-clear ()
  "Clear the debugger output buffer."
  (interactive)
  (let ((buf (mds-output-get-buffer)))
    (when (bufferp buf)
      (with-current-buffer buf
	(delete-region (point-min) (point-max))))))

(defun mds-output-display (buf msg &optional tag)
  "Display MSG in `mds-output-buffer'."
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
	     ((eq tag 'stack)
	      ;; stack
	      (mds-insert-tag tag) (setq beg (point))
	      (insert msg)
	      (if (string= msg "TopLevel\n")
		  (mds-put-face beg (point) 'mds-inactive-link-face)
		(make-text-button beg (1- (point)) :type 'mds-output-view-proc-button)))
	     ((eq tag 'where)
	      ;; where
	      (mds-insert-tag tag) (setq beg (point))
	      (insert msg)
	      (goto-char beg)
	      (let ((toplev (looking-at "TopLevel")))
		(unless (re-search-forward ":\\( \\|$\\)" nil 't)
		  (error "no delimiter"))
		(if toplev
		    (mds-put-face beg (- (point) 2) 'mds-inactive-link-face)
		  (make-text-button beg (- (point) 2) :type 'mds-output-view-proc-button))))
	     ;; warning
	     ((eq tag 'warn)
	      (mds-insert-tag tag) (setq beg (point))
	      (insert msg)
	      (mds-put-face beg (1- (point-max)) 'mds-warning-face))
	     ((and tag (symbolp tag))
	      (mds-insert-tag tag) (setq beg (point))
	      (insert msg))
	     )))
	(recenter -1)))))


(defun mds-put-face (beg end face)
  (put-text-property beg end 'font-lock-face face))

(defun mds-insert-tag (tag)
  (let ((beg (point)))
    (insert (format "<%s>" (prin1-to-string tag)))
    (mds-put-face beg (point) 'font-lock-string-face)
    (insert ": ")))

(define-button-type 'mds-output-view-proc-button
  'help-echo "Open procedure"
  'action 'mds-output-view-procedure
  'follow-link t
  'face 'link)

(defconst mds-output-procname-re "\\([^ \t\n]+\\)\\(: \\|$\\)")

(defun mds-output-view-procedure (button)
  (save-excursion
    (beginning-of-line)
    ;; temporary to jump over tag
    (search-forward " ")
    (unless (looking-at "TopLevel")
      (looking-at mds-output-procname-re)
      (let ((procname (match-string-no-properties 1))
	    (statement (buffer-substring-no-properties
			(match-end 0) (line-end-position))))
	(mds-showstat-display-inactive procname statement)))))


(defun mds-output-create-buffer ()
  "Create and return an `mds-output-buffer'.
This must be called with the associated showstat buffer
as the current buffer.  It saves the showstat buffer
in the buffer-local variable `mds-output-showstat-buffer'."
  (let ((buf (generate-new-buffer "*mds-output*")))
    (with-current-buffer buf
      (setq mds-client nil)
      (font-lock-mode 't))
    buf))
    

(provide 'mds-output)

;; mds-output.el ends here
