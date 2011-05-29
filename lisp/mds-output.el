;;; mds-output.el
;;; -*- mode emacs-lisp; mode: folding -*-

;; Copyright (C) 2011 Joseph S. Riel, all rights reserved

;; Author:     Joseph S. Riel <jriel@maplesoft.com>
;; Created:    Jan 2011
;; Keywords:   maple, debugger
;;
;;; Commentary:

;; 

;;;

(defface mds-warning-face
  '((((class color) (background dark)) (:foreground "pink")))
  "Face for warning messages in output buffer."
  :group 'mds-faces)

(defface mds-inactive-link-face
  '((((class color) (background dark)) (:foreground "cyan1")))
  "Face for inactive links in output buffer."
  :group 'mds-faces)

(defun mds-output-clear ()
  "Clear the debugger output buffer."
  (interactive)
  (let ((buf mds-output-buffer))
    (when (bufferp buf)
      (with-current-buffer buf
	(delete-region (point-min) (point-max))))))

(defun mds-output-display (msg buf &optional tag)
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
		  (make-text-button  beg (- (point) 2) :type 'mds-showstat-open-button))))
	     ;; warning
	     ((eq tag 'warn)
	      (mds-insert-tag tag) (setq beg (point))
	      (insert msg)
	      (mds-put-face beg (1- (point-max)) 'mds-warning-face)))))
	(recenter -1)))))


(defun mds-put-face (beg end face)
  (put-text-property beg end 'font-lock-face face))

(defun mds-insert-tag (tag)
  (let ((beg (point)))
    (insert (format "<%s>" (prin1-to-string tag)))
    (mds-put-face beg (point) 'font-lock-string-face)
    (insert ": ")))


(defun mds-output-create-buffer ()
  "Create and return an `mds-output-buffer'."
  (let ((buf (generate-new-buffer "*mds-output*")))
    (with-current-buffer buf
      (font-lock-mode 't))
    buf))
    


;;(mds-output-warn "watch out")


(provide 'mds-output)

