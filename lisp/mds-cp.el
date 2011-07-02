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

(eval-when-compile
  (require 'mds-client)
  (require 'wid-edit)
  (require 'widget)
  (require 'mds-wm))
  

;;{{{ declarations

;; avoid compiler warnings

(declare-function mds-client-out-buf "mds")
(declare-function mds-ss-eval-proc-statement "mds-ss")
(declare-function mds-ss-view-dead-proc "mds-ss")
(declare-function mds-ss-modeline-hilite "mds-ss")
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
  (mds-ss-modeline-hilite (mds-client-live-buf (mds-wm-active-client)) 'off)
  (mds-ss-modeline-hilite (mds-client-live-buf (mds-wm-cycle-clients backwards))))

(defmacro mds-cp-cmd (cmd)
  "Send CMD (a string) to the active client.  This is equivalent
to using the key-binding in the active client's live showstat buffer."
  `(lambda (&rest ignore)
     (interactive)
     (select-frame mds-frame)
     (with-current-buffer (mds-cp-get-ss-live-buf)
       (mds-ss-eval-proc-statement ,cmd 'save))))

(defmacro mds-cp-ss-cmd (cmd)
  "Execute elisp command CMD in the ss-live-buf."
  `(lambda (&rest ignore)
     (interactive)
     (select-frame mds-frame)
     (with-current-buffer (mds-cp-get-ss-live-buf)
       (,cmd))))

(defun mds-cp-next (&rest ignore)
  (select-frame mds-frame)
  (with-current-buffer (mds-cp-get-ss-live-buf)
    (mds-ss-eval-proc-statement "next" 'save))
  (select-frame mds-cp-frame))

(defun mds-cp-step (&rest ignore)
  (interactive)
  (select-frame mds-frame)
  (with-current-buffer (mds-cp-get-ss-live-buf)
    (mds-ss-eval-proc-statement "step" 'save)))
;;  (select-frame mds-cp-frame))

(defun mds-cp-get-ss-live-buf ()
  "Return the live showstat buffer of the active client."
  (mds-client-live-buf (mds-wm-active-client)))


;;{{{ mds-cp-mode-map

(defvar mds-cp-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-keymap)
    (let ((bindings
	   `(
	     ("c" . ,(mds-cp-cmd "cont"))
	     ("i" . ,(mds-cp-cmd "into"))
	     ("n" . ,(mds-cp-cmd "next"))
	     ("o" . ,(mds-cp-cmd "outfrom"))
	     ("r" . ,(mds-cp-cmd "return"))
	     ("s" . ,(mds-cp-cmd "step"))
	     )))
      (mapc (lambda (binding) (define-key map (car binding) (cdr binding)))
	    bindings)
      map)))

;;}}}

;;{{{ mds-cp-mode


(define-derived-mode mds-cp-mode fundamental-mode "mds-cp-mode"
  "Major mode for the mds control panel."

  :group 'mds

  
  (erase-buffer)

  ;;{{{ create widgets
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			    (mds-cp-select-next-client 'backwards))
		 "<-")
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			    (mds-cp-select-next-client))
		 "->")

  (widget-insert "\n")
  (widget-create 'push-button :notify (mds-cp-cmd "next") "Next")
  (widget-create 'push-button :notify (mds-cp-cmd "into") "Into")
  (widget-create 'push-button :notify (mds-cp-cmd "step") "Step")
  (widget-create 'push-button :notify (mds-cp-cmd "cont") "Cont")
  (widget-insert "\n")
  (widget-create 'push-button :notify (mds-cp-cmd "outfrom") "Outfrom")
  (widget-create 'push-button :notify (mds-cp-cmd "return")  "Return")
  (widget-create 'push-button :notify (mds-cp-cmd "quit")    "Quit")
  (widget-insert "\n")
  (widget-create 'push-button :notify (mds-cp-cmd "where")     "Where")
  (widget-create 'push-button :notify (mds-cp-cmd "showstack") "Showstack")

  ;;(use-local-map widget-keymap) ; what is this?
  (use-local-map mds-cp-mode-map)
  (widget-setup)
  ;;}}}


  ) ; end: mds-cp-mode


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
    (setq mds-cp-frame frame)
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

;; (mds-cp-create)

(provide 'mds-cp)

;; mds-cp.el ends here


