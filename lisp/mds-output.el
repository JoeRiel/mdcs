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

(defun mds-output-clear ()
  "Clear the debugger output buffer."
  (interactive)
  (let ((buf mds-output-buffer))
    (when (bufferp buf)
      (with-current-buffer buf
	(delete-region (point-min) (point-max))))))

(defun mds-output-display (msg buf &rest ignore-for-now)
  "Display MSG in `mds-output-buffer'."
  (unless (string= msg "")
    (display-buffer buf)
    (with-selected-window (get-buffer-window buf)
      (with-current-buffer buf
	(goto-char (point-max))
	(let ((beg (point)))
	  (insert msg))
	(recenter -1)))))

(defun mds-put-warn-face (msg)
  (put-text-property 0 (1- (length msg)) 'font-lock-face 'mds-warning-face msg))

(defun mds-output-create-buffer ()
  "Return an `mds-output-buffer'."
  (let ((buf (generate-new-buffer "*mds-output*")))
    (with-current-buffer buf
      (font-lock-mode 't))))
    


;;(mds-output-warn "watch out")


(provide 'mds-output)

