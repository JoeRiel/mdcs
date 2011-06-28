;;; mds-edit.el

;; Copyright (C) 2011 Joseph S. Riel, all rights reserved

;; Author:     Joseph S. Riel <jriel@maplesoft.com>
;; Created:    Jan 2011
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

(defconst mds-edit-ss-mark-re "^ +[0-9]+[*!]?"
  "Regexp that matches the statement mark added by showstat.")

(defun mds-edit-remove-ss-marks ()
  "Remove the statement numbers and other marks from the showstat
listing of a procedure."
  (goto-char (point-min))
  (while (re-search-forward mds-edit-ss-mark-re nil 'move)
    (replace-match (make-string (- (match-end 0) (match-beginning 0)) ?\s))))

(defun mds-edit-ss-to-proc ()
  (interactive)
  (save-excursion
    (mds-edit-remove-ss-marks)
    (maplev-indent-buffer)
    (maplev-end-of-defun)
    (forward-line -1)
    (when (looking-at "end proc$")
      (end-of-line)
      (insert ":"))))


(provide 'mds-edit)

;;; mds-edit.el ends here

