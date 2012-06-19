;;; mds-li.el --- implement lineinfo mode

;; Copyright (C) 2012 Joseph S. Riel, all rights reserved

;; Author:     Joseph S. Riel <jriel@maplesoft.com>
;; Created:    Jan 2012
;; Keywords:   maple, debugger
;;
;;; Commentary:

;; This file contains the source for the Emacs Maple debugger server.
;; It is a part of the Maple Debugger Client Server package.

;;{{{ License

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc.,  51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;}}}

;;; Code:

;;{{{ Requirements

(eval-when-compile
  (require 'mds-client)
  (require 'mds-custom)
  (require 'mds-ss)
  (require 'mds-wm)
  (require 'maplev)
  (defvar mds-truncate-lines))


;;}}}

;;{{{ Faces

(defface mds-li-breakpoint-face
  '((((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark))  (:foreground "DimGray"   :bold t))
    (((class color)     (background light)) (:foreground "red"))
    (((class color)     (background dark))  (:foreground "red"))
    (t (:bold t)))
  "Face for highlighting breakpoint fringe bitmap."
  :group 'mds-faces)

;;}}}

;;{{{ Variables

(defvar mds-li-addr ""            "Address of the current procedure.")
(defvar mds-li-arrow-position nil "Marker for current-line.")
(defvar mds-li-beg nil            "Character position of current position.")   
(defvar mds-li-file-name ""       "Name of the current source-file.")
(defvar mds-li-state 0            "Current state (posint).")

(mapc #'make-variable-buffer-local
      '(mds-li-addr
	mds-li-arrow-position
	mds-li-beg
	mds-li-file-name
	mds-li-state))

(add-to-list 'overlay-arrow-variable-list 'mds-li-arrow-position)

;;}}}

;;{{{ Create buffer

(defun mds-li-create-buffer (client)
  "Create and return an `mds-li-buffer' with `mds-client' set to CLIENT."
  (let ((buf (generate-new-buffer "*mds-li*")))
    (with-current-buffer buf
      (mds-li-mode)
      (setq mds-client client))
    buf))

;;}}}

;;{{{ Update source buffer
  
(defun mds-li-update (buffer file addr procname state beg breakpoints)
  "Update source BUFFER with source-file FILE.  
PROCNAME is the procedure name.
STATE is  the current state (a string corresponding to an integer).
BEG is the character position of the beginning of the statement in FILE.
BREAKPOINTS is a list of the position of the beginning of lines that have breakpoints.

Put point at BEG and move the current statement marker."
  (set-buffer buffer)
  (let ((same-file (string= file mds-li-file-name))
	(same-addr (string= addr mds-li-addr))
	buffer-read-only)
    (unless same-file
      ;; insert the new file and update buffer-local variable
      (delete-region (point-min) (point-max))
      (mds-li-remove-breakpoint (point-min))
      (insert-file-contents file)
      (setq mds-li-file-name file))
    (unless same-addr
      ;; insert breakpoints and update buffer-local variable
      (let (brkpt)
	(while breakpoints
	  (setq brkpt (car breakpoints)
		breakpoints (cdr breakpoints))
	  (goto-char (1+ brkpt))
	  (mds-li-set-breakpoint (line-beginning-position))))
      (setq mds-li-addr addr))
    (unless (and same-file same-addr)
      ;; Update the mode-line
      (mds-li-set-mode-line file
			    procname
			    (car (mds-client-id mds-client)))))
  (setq mds-li-state (string-to-number state))
  (goto-char beg)
  (set-marker mds-li-arrow-position (line-beginning-position))
  (setq mds-li-beg beg))

;;}}}

;;{{{ Functions

(defun mds-li-get-state-list (pos)
  "Return the statement information of source at POS.
If line information is available, a list of three strings is
returned: address, statement number, and beginning character
offset."
  (let ((result (mds-ss-request (format "mdc:-LineInfo:-LookupStatement(\"%s\",%s,%s,%d)"
					mds-li-file-name
					(mds-client-get-addr mds-client)
					(line-number-at-pos (point))
					(1- (point))))))
    (if (string= result "\"no candidates\"")
	(error "No candidates... "))
    (split-string result ", ")))
	   
    
(defun mds-li-goto-current-state (client)
  "Move point to current statement beginning in line-info buffer of CLIENT.
Set cursor to ready."
  (mds-wm-select-code-window client)
  (goto-char mds-li-beg)
  (setq cursor-type mds-cursor-ready))

(defun mds-li-goto-state (state)
  "Move point to the beginning of statement number STATE in the current procedure."
  (goto-char (1+ (string-to-number 
		  (mds-ss-request (format "mdc:-LineInfo:-Get(%s,%d,'ret_begin')" 
					  (mds-client-get-addr mds-client) state))))))
  


(define-fringe-bitmap 'mds-li-breakpoint  [60 126 255 255 255 255 126 60])

(defun mds-li-set-breakpoint (pos)
  "Set a breakpoint symbol at POS."
  (mds-li-remove-breakpoint pos)
  (let* ((display-string
	  `(left-fringe mds-li-breakpoint . ,(cons 'mds-li-breakpoint-face nil)))
	 (before-string (propertize "*" 'display display-string))
	 (ov (make-overlay pos pos)))
    (overlay-put ov 'before-string before-string)
    (overlay-put ov 'breakpoint t)
    ov))

(defun mds-li-remove-breakpoint (pos)
  "Remove all breakpoint symbols at POS."
  (let ((overlays (overlays-in pos pos)))
    (while overlays
      (let ((ov (car overlays)))
	(if (overlay-get ov 'breakpoint)
	    (delete-overlay ov))
	(setq overlays (cdr overlays))))))



;;}}}

;;{{{ Commands

(defun mds-li-breakpoint (&optional clear)
  "Set breakpoint at point.  If optional CLEAR is non-nil, clear the breakpoint."
  (interactive)
  (let ((state (mds-li-get-state-list (point))))
    ;; Turn on/off fringe bitmap.
    (goto-char (string-to-number (nth 2 state)))
    (if clear
	(mds-li-remove-breakpoint (line-beginning-position))
      (mds-li-set-breakpoint (line-beginning-position)))
    ;; Set/clear the actual breakpoint.
    (mds-ss-eval-debug-code (format "debugopts('stopat'=[pointto(%s),%s%s])"
				    (car state)
				    (if clear "-" "")
				    (nth 1 state)))))

(defun mds-li-unstopat ()
  "Clear breakpoint at point."
  (interactive)
  (mds-li-breakpoint 'clear))

(defun mds-li-here (cnt)
  "Skip until the statement at point is reached CNT times."
  (interactive "p")
  (message "Skipping to point...")
  (let ((addr-state-beg (mds-li-get-state-list (point))))
    (mds-ss-eval-proc-statement (format "_here %d %s %s"
					cnt
					(nth 0 addr-state-beg)
					(nth 1 addr-state-beg)))))

(defun mds-li-open-source-at-point ()
  "Open the file associated with current procedure."
  (interactive)
  (if (mds-client-has-source-p mds-client)
      (with-current-buffer (mds-client-li-buf mds-client)
	(when mds-li-file-name
	  (let ((point (point)))
	    (find-file mds-li-file-name)
	    (goto-char point))))
    (beep)
    (message "No line-info source associated with current procedure.")))

;;{{{ (*) Evaluation

(defun mds-li-eval-and-prettyprint-prev ()
  "Call `mds-eval-and-prettyprint' with point at the preceding statement."
  (interactive)
  (save-excursion
    (if (= 1 mds-li-state)
	(progn
	  (beep)
	  (message "No preceding state."))
      (mds-li-goto-state (1- mds-li-state))
      (let ((expr (mds-expr-at-point)))
	(mds-ss-eval-expr (format "mdc:-Format:-PrettyPrint(%s)" expr) expr)))))
				

;;}}}

;;}}}

;;{{{ mode-line

(defun mds-li-set-mode-line (file procname &optional label)
  "Set the mode-line of the mds-li buffer.
FILE is the name of the source file, PROCNAME is the current procedure,
LABEL is the user id."
  (setq mode-line-format
	(list
	 mode-line-buffer-identification
	 (and label (concat "   " (propertize (format "[%s]" label) 'face 'bold)))
	 "   "
	 (propertize (format "[%s: %s]" procname file) 'face 'bold)
	 "-%-")))

;;}}}
;;{{{ mode-map

;; Copy mds-ss-mode-map, but rebind some keys

(defvar mds-li-mode-map nil
  "Keymap for `mds-li-mode'.")

(unless mds-li-mode-map
  (let ((map (copy-keymap mds-ss-mode-map))
	(bindings
	 '(("b" . mds-li-breakpoint)
	   ("B" . mds-breakpoint-cond)
	   ("g" . mds-goto-procname)
	   ("G" . mds-li-open-source-at-point)
	   ("h" . mds-li-here)
	   ("I" . mds-stopwhenif)
	   ("l" . mds-goto-current-state)
	   ("L" . mds-ss-refresh)
	   ("q" . mds-quit)
	   ("u" . mds-li-unstopat)
	   ("," . mds-li-eval-and-prettyprint-prev)
	   )))
    (mapc (lambda (binding) (define-key map (car binding) (cdr binding)))
	  bindings)
    (setq mds-li-mode-map map)))

;;}}}
;;{{{ mds-li-mode

(define-derived-mode mds-li-mode mds-ss-mode "source-mode"
  "Major mode for stepping through source code of a debugged Maple procedure."
  :group 'mds
  (setq mds-li-arrow-position (make-marker))
  (setq tab-width maplev-indent-level)
  (if mds-truncate-lines (toggle-truncate-lines 1)))


;;}}}


(provide 'mds-li)


;;; mds-li.el ends here
