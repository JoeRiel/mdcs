;;; mds-cp.el

;; Copyright (C) 2011 Joseph S. Riel, all rights reserved

;; Author:     Joseph S. Riel <jriel@maplesoft.com>
;; Created:    June 2011
;; Keywords:   maple, debugger
;;
;;; Commentary:

;; Code for the Maple Debugger Control Panel.

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


;;; Code

(require 'widget)
(eval-when-compile
  (require 'mds-wm)
  (require 'wid-edit))

;;{{{ declarations

;; avoid compiler warnings

(declare-function mds--get-client-out-buf "mds")
(declare-function mds-ss-view-dead-proc "mds-ss")
(declare-function mds-wm-display-dead "mds-wm")

;;}}}
;;{{{ faces

(defface mds-cp-face
  '((((class color) (background dark)) (:foreground "lawn green")))
  "Face for stack arguments."
  :group 'mds-faces)

;;}}}
;;{{{ constants

;;}}}
;;{{{ variables

;; this won't work.  We need to be able to switch clients.

(defvar mds-cp-ss-buf nil "Live showstat buffer associated with control panel.")
(defvar mds-cp-out-buf nil "Output associated with control panel.")

(defvar mds-cp-frame nil "Floating frame used for control panel.")

;;}}}


(defun mds-cp-select-next-client (&optional backwards)
  (mds-ss-modeline-hilite (mds--client-get-live-buf (mds-wm-selected-client)) 'off)
  (mds-ss-modeline-hilite (mds--client-get-live-buf (mds-wm-next-client backwards))))

;;{{{ mds-cp-mode

(define-derived-mode mds-cp-mode fundamental-mode "mds-cp-mode"
  "Long-winded description"
  :group 'mds

  (delete-region (point-min) (point-max))

  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			    (mds-cp-select-next-client 'backwards))
		 "<-")
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			    (mds-cp-select-next-client 'backwards))
		 "->")

  (widget-insert "\n")
  
  (widget-create 'push-button "Next")
  (widget-create 'push-button "Into")
  (widget-create 'push-button "Step")
  (widget-create 'push-button "Cont")
  (widget-insert "\n")
  (widget-create 'push-button "Outfrom")
  (widget-create 'push-button "Return")
  (widget-create 'push-button "Quit")

  (use-local-map widget-keymap) ; what is this?
  (widget-setup)
  )

;; (mds-ss-modeline-hilite (current-buffer))


;;}}}

;;{{{ mds-cp-create
(defun mds-cp-create ()
  "Create the control panel frame and assign to `mds-cp-frame'."
  
  (let* ((frame (make-frame '((title . "mds control panel")
			      (height . 5)
			      (width . 50)
			      (vertical-scroll-bars . nil)
			      (menu-bar-line . 0)
			      (minibuffer . nil)
			      (unsplittable . t)
			      )))
	 (window (frame-first-window frame))
	 (buffer (get-buffer-create "*mds-control-panel*")))

    (let ((cp-ht  (frame-pixel-height frame))
	  (cp-wd  (frame-pixel-width frame))
	  (mds-ht (frame-pixel-height))
	  (mds-wd (frame-pixel-width)))
      (set-frame-position frame (/ (- mds-wd cp-wd) 2) mds-ht))
    (select-frame frame)
    (select-window window)
    (delete-other-windows)
    (set-window-buffer window buffer)
    (set-buffer buffer)
    (mds-cp-mode)))

;;}}}

(provide 'mds-cp)

;; mds-cp.el ends here
