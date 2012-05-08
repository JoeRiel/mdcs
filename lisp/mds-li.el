;;; mds-li.el

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

(declare-function mds-ss-eval-proc-statement "mds-ss")
(declare-function mds-ss-get-addr "mds-ss")
(declare-function mds-ss-get-state "mds-ss")
(declare-function showstat-mode "mds-ss")

;; FIXME; this cannot be global!
(defvar mds-display-source-flag nil)

;;{{{ First Attempt

(defun mds-li-display-source ()
  "Display the source file in the other window.
The source file and offset are stored as a cons cell
in `mds-display-source-flag'; after extracting the contents,
clear the variable.  Currently this variable is global; it should
be buffer local though it might not really matter."

  (let ((filename (car mds-display-source-flag))
	(offset   (cdr mds-display-source-flag)))
    (setq mds-display-source-flag nil)
    (find-file-other-window filename)
    (goto-char (1+ offset))))

(defun mds-li-goto-source ()
  "Goto the location in the source file corresponding to the
current statement in the showstat buffer.  If the procedure does
not have *lineinfo* data, print an appropriate message."
  (interactive)
  (mds-ss-eval-proc-statement (format "_lineinfo %s %s"
				      (mds-ss-get-addr)
				      (mds-ss-get-state))))

(defun mds-li-handle (msg)
  "Handle the Maple output returned as a result of calling `mds-li-goto-source'.
If MSG matches the expected format, assign the variable
`mds-display-source-flag' a cons-cell consisting of the filename
and offset; otherwise print a message indicating that no lineinfo
data is available."
  (if (string-match "^\\([^ ]+\\), \\([0-9]+\\), \\([0-9]+\\), \\([0-9]+\\)$" msg)
      (let ((filename (match-string-no-properties 1 msg))
	    (offset   (string-to-number (match-string-no-properties 3 msg))))
	(setq mds-display-source-flag (cons filename offset)))
    (setq mds-display-source-flag nil)
    (ding)
    (message "no lineinfo data available for current procedure")))

;;}}}

;;{{{ external methods

;; These are intended to be called ...

;; (defun mds-li-update (buf addr procname state)
;;   "Update the source buffer BUF.
;; ADDR is the address of PROCNAME,

;; "

;;}}}


;;{{{ mds-src-mode

(define-derived-mode mds-src-mode showstat-mode "source-mode"
  "Major mode for stepping through source code of a debugged Maple procedure."
  :group 'mds
  
  )


;;}}}



(provide 'mds-li)


;; mds-li.el ends here
