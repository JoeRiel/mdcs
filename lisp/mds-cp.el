;;; mds-conrol-panel.el

;; Copyright (C) 2011 Joseph S. Riel, all rights reserved

;; Author:     Joseph S. Riel <jriel@maplesoft.com>
;; Created:    Jan 2011
;; Keywords:   maple, debugger
;;
;;; Commentary:

;; Code for the Maple Debugger Control Panel.

;;;

(require 'widget)
(eval-when-compile
  (require 'mds-windows)
  (require 'wid-edit))

;;{{{ declarations

;; avoid compiler warnings

(declare-function mds--get-client-out-buf "mds")
(declare-function mds-showstat-view-dead-proc "mds-showstat")
(declare-function mds-windows-display-dead "mds-windows")

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

;;{{{ mds-cp-mode

(define-derived-mode mds-cp-mode fundamental-mode "mds-control-panel-mode"
  "Long winded description"
  :group 'mds

  (delete-region (point-min) (point-max))

  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (mds-windows-cycle-clients 'backward))
		 "<-")
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (mds-windows-cycle-clients))
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

;; (mds-cp-create)
;

(let ((win (get-buffer-window)))
  (setq mds-overlay (make-overlay (point-min) (point-max) nil 'rear-advance)))

(overlay-put mds-overlay 'face '(:background "gray15"))
; (setq mode-line-format `(:propertize ,mode-line-format face default))

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

;; mds-control-panel.el ends here
