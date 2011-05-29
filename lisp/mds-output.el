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


(defun mds-output-display (msg &optional func )
  "Display MSG in `mds-debugger-output-buffer'."
  (unless (string= msg "")
    (let ((buf mds-showstat-output-buffer))
      (display-buffer buf)
      (with-selected-window (get-buffer-window buf)
	(with-current-buffer buf
	  (goto-char (point-max))
	  (let ((beg (point)))
	    (insert msg)
	    (when func
	      (goto-char beg)
	      (apply func)))
	  (recenter -1))))))
    
(defun mds-output-clear ()
  "Clear the debugger output buffer."
  (interactive)
  (let ((buf mds-showstat-output-buffer))
    (when (bufferp buf)
      (with-current-buffer buf
	(delete-region (point-min) (point-max))))))

(provide 'mds-output)