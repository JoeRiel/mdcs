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
  (require 'mds-ss)
  (defvar mds-ss-mode-map)
  (defvar mds-truncate-lines))

(declare-function mds-ss-eval-proc-statement "mds-ss")
(declare-function mds-ss-get-addr "mds-ss")
(declare-function mds-ss-get-state "mds-ss")
(declare-function mds-ss-mode "mds-ss")
(declare-function mds-ss-request "mds-ss")

;;}}}

;;{{{ Variables

;; FIXME; this cannot be global!
(defvar mds-display-source-flag nil "Non-nil means display the source file, if available.")

(defvar mds-li-arrow-position nil "Marker for current-line.")
(defvar mds-li-file-name ""       "Name of the current source-file.")

(mapc #'make-variable-buffer-local
      '(mds-li-arrow-position
	mds-li-file-name))

(add-to-list 'overlay-arrow-variable-list 'mds-li-arrow-position)

;;}}}

;;{{{ Create Buffer

(defun mds-li-create-buffer (client)
  "Create and return an `mds-li-buffer' with client CLIENT."
  (let ((buf (generate-new-buffer "*mds-li*")))
    (with-current-buffer buf
      (mds-li-mode)
      (setq mds-client client)
      (if mds-truncate-lines
	  (toggle-truncate-lines 1)))
    buf))


;;}}}
  
;;{{{ Display Source

(defun mds-li-display-source (buf file beg)
  "Display, in buffer BUF, the source-file FILE, with point at BEG.
Move the current statement marker.  The buffer has major mode
`mds-li-mode'."
  (switch-to-buffer buf)
  (unless (string= file mds-li-file-name)
    ;; insert the new file
    (let (buffer-read-only)
      (delete-region (point-min) (point-max))
      (insert-file-contents file)
      (setq mds-li-file-name file)))
  (goto-char beg)
  (set-marker mds-li-arrow-position (line-beginning-position)))

;;}}}

;;{{{ Functions

(defun mds-li-addr ()
  "Return the address of the current procedure."
  (with-current-buffer (mds-client-live-buf mds-client)
    (mds-ss-get-addr)))

(defun mds-li-get-statement (pos)
  "Get the statement number, as string, associated with POS."
  (string-to-number (mds-ss-request (format "mdc:-LineInfo:-LookupStatement(\"%s\",%s,%s,%d)"
					    mds-li-file-name
					    (mds-li-addr)
					    (line-number-at-pos (point))
					    (1- (point))))))

;;}}}

;;{{{ Commands

(defun mds-li-breakpoint ()
  "Set breakpoint at point."
  (interactive)
  (let ((state (mds-li-get-statement (point))))
    (with-current-buffer (mds-client-live-buf mds-client)
      (mds-goto-state (number-to-string state))
      (mds-breakpoint))))

(defun mds-li-unstopat ()
  "Clear breakpoint at point."
  (interactive)
  (let ((state (mds-li-get-statement (point))))
    (with-current-buffer (mds-client-live-buf mds-client)
      (mds-goto-state (number-to-string state))
      (mds-unstopat))))

(defun mds-li-here (cnt)
  (interactive "p")
  (with-current-buffer (mds-client-live-buf mds-client)
    (mds-here 1)))
  


;;}}}

;;{{{ mode-map

;; Copy mds-ss-mode-map, but rebind some keys

(defvar mds-li-mode-map
  (let ((map (copy-keymap mds-ss-mode-map))
	(bindings
	 '(
	   ("b" . mds-li-breakpoint)
	   ("B" . mds-breakpoint-cond)
	   ("g" . mds-goto-procname)
	   ("G" . mds-goback-save)
	   ("h" . mds-li-here)
	   ("I" . mds-stopwhenif)
	   ("l" . mds-goto-current-state)
	   ("L" . mds-ss-refresh)
	   ("q" . mds-quit)
	   ("u" . mds-li-unstopat)
	   )))
    (mapc (lambda (binding) (define-key map (car binding) (cdr binding)))
	  bindings)
    map))
;;}}}
;;{{{ mds-li-mode

(define-derived-mode mds-li-mode mds-ss-mode "source-mode"
  "Major mode for stepping through source code of a debugged Maple procedure."
  :group 'mds
  (setq mds-li-arrow-position (make-marker))
  )


;;}}}


(provide 'mds-li)


;;; mds-li.el ends here
