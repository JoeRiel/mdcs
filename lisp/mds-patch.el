;;; mds-patch.el

;; Copyright (C) 2011 Joseph S. Riel, all rights reserved

;; Author:     Joseph S. Riel <jriel@maplesoft.com>
;; Created:    Jan 2011
;; Keywords:   maple, debugger
;;
;;; Commentary:

;; This file contains the live-patching elisp code for the Emacs Maple
;; debugger server.  It is a part of the Maple Debugger Client-Server
;; package.

;; Purpose:
;;
;; The live-patching code permits modifying a Maple procedure.

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

(eval-when-compile
  (require 'maplev)
  (require 'mds-client)
  (require 'mds-re)
  (require 'mds-ss)
)

;;{{{ Create patch buffer
(defun mds-patch ()
  "Copy the showstat buffer to a new buffer for patching."
  (interactive)
  ;; Save point position, begin editing there.
  ;; Use the procname as the buffer name (more or less).
  ;; Remove statement numbers and debug symbols
  ;; Store address in buffer-local variable (mds-patch-address)
  ;; Indent the code

  ;; delete invisible address string
  (let ((ss-buf (current-buffer)) ; assume we are in a ss-buf
	(ss-addr (mds-client-get-addr mds-client))
	(client mds-client)
	(point (point)))
    (set-buffer (get-buffer-create mds-ss-procname))
    (erase-buffer)
    (insert-buffer-substring ss-buf)
    (goto-char point)
    (save-excursion
      ;; remove hidden address
      (goto-char (point-min))
      (forward-line)
      (delete-region (point-min) (point))
      ;; remove numbers and marks, change mode, indent
      (mds-patch-remove-numbers)
      (mds-patch-mode)
      (mds-client-set-addr client ss-addr)
      (setq mds-client client)
      (maplev-indent-buffer)
      (toggle-truncate-lines 1))
    (switch-to-buffer-other-window (current-buffer))))
;;}}}
;;{{{ Cleanup buffer

(defun mds-patch-remove-numbers ()
  "Remove the statement numbers and debug marks from buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward mds--statement-number-and-marks-re nil t)
      (replace-match ""))))

;;}}}
;;{{{ Install patch
(defun mds-patch-install ()
  "Install the patch in the patch buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward ":= " (line-end-position))
    (let ((str (buffer-substring-no-properties
		(point) (point-max))))
      (mds-client-send mds-client
		   (format "statement mdc:-InstallPatch(%s,%s)\n"
			   (mds-client-get-addr mds-client) str))
      (message "Installed patch"))))
;;}}}

;;{{{ menu
(defvar mds-patch-menu nil)
(unless mds-patch-menu
  (easy-menu-define
    mds-patch-menu maplev-mode-map
    "Menu for mds patch mode"
    `("Patch"
      ["Install"  mds-patch-install t]
      )))
;;}}}
;;{{{ mode

(define-derived-mode mds-patch-mode maplev-mode "Maple Patch Mode"
  "Major mode for live patching a Maple procedure."
  :group 'mds

  (let ((map mds-patch-mode-map)
	(bindings
	 '(([(control c) (control p)] . mds-patch-install))))
    ;; assign bindings
    (mapc (lambda (binding) (define-key map (car binding) (cdr binding)))
	  bindings)
    (setq mds-patch-mode-map map))
  )

;;}}}

(provide 'mds-patch)
