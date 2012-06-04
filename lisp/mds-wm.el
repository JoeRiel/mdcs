;;; mds-wm.el

;; Copyright (C) 2011 Joseph S. Riel, all rights reserved

;; Author:     Joseph S. Riel <jriel@maplesoft.com>
;; Created:    June 2011
;; Keywords:   maple, debugger
;;
;;; Commentary:

;; Code for a simple window manager for mds

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

;;{{{ requirements and declarations

;; avoid compiler warnings

(eval-when-compile
  (require 'mds-client))

(declare-function mds-client-id "mds")
(declare-function mds-client-live-buf "mds")
(declare-function mds-client-dead-buf "mds")
(declare-function mds-client-out-buf  "mds")

;;}}}
;;{{{ customizations

(defgroup mds-wm nil
  "Maple Debugger Server Window Manager"
  :group 'mds)

(defcustom mds-wm-side-by-side t
  "If non-nil, the showstat and output windows are displayed 
side-by-side in a single client view."
  :type 'boolean
  :group 'mds-wm)

(defcustom mds-wm-ss-fractional-size nil
  "If non-nil, specifies the fractional size of the showstat
window in relation to the frame size.  Width is used when
`mds-wm-side-by-side' is non-nil, otherwise height is used.  If
nil, then the window is set to half the frame size."
  :type 'float
  :group 'mds-wm)

;;}}}
;;{{{ variables

(defvar mds-frame nil "Frame used by mds")
(defvar mds-clients '())  ;; duplicate, here to avoid warnings
(defvar mds-wm-grouped-clients '()
  "List of grouped clients.  Clients which are displayed
together are combined in a sublist.  Clients that are not grouped
are not in sublists.")

;;}}}

;;{{{ Display particular buffers

(defun mds-wm-display-live-buf ()
  "Display the live showstat buffer.
If the buffer is already displayed, do nothing.  If the output
buffer is diplayed, display the showstat buffer in another
window."
  (let ((ss-buf (mds-client-live-buf mds-client)))
    (unless (get-buffer-window ss-buf) ;; what about on other frame?
      (let ((win (get-buffer-window (mds-client-out-buf mds-client))))
	(if (null win)
	    (switch-to-buffer ss-buf)
	  (if (= 1 (length (window-list)))
	      ;; out-buf is only window displayed,
	      ;; so just start over.  Hackish.
	      (mds-wm-display-client mds-client)
	    (select-window win)
	    (switch-to-buffer-other-window ss-buf)))))))

;;}}}
	  

;;{{{ display single client

(defun mds-wm-display-client (client)
  "Display the live-showstat buffer and the output buffer of CLIENT.
nThe split direction and initial size of the showstat window are
determined by `mds-wm-side-by-side' and `mds-wm-ss-size'.  Return
the client."
  (if (not (eq (selected-frame) mds-frame))
      ;; first set focus to selected frame;
      ;; without that, xfce doesn't switch frames
      (select-frame-set-input-focus (selected-frame)))
  (mds-wm-select-frame-set-input-focus mds-frame)
  (if mouse-autoselect-window
      ;; without this, there are weird problems with window splits or selecting live-buf
      (sleep-for 0.1))
  (let ((buf1 (mds-client-live-buf client))
	(buf2 (mds-client-out-buf client)))
    (delete-other-windows (select-window (display-buffer buf1)))
    (set-window-buffer (split-window nil 
				     (and mds-wm-ss-fractional-size
					  (round (* mds-wm-ss-fractional-size
						    (if mds-wm-side-by-side
							(window-width)
						      (window-height)))))
				     mds-wm-side-by-side)
		       buf2)
    client))

(defun mds-wm-display-dead (client)
  "Display the dead showstat buffer of CLIENT in a window."
  (let ((dead-buf (mds-client-dead-buf client)))
    (unless (get-buffer-window-list dead-buf)
      (let ((out-buf (mds-client-out-buf client)))
	(if (not (get-buffer-window-list out-buf))
	    ;; no visible buffers, use the basic method
	    (mds-wm-display-client client))
	;; split (one of) the visible out-buf windows
	(let ((win (display-buffer out-buf)))
	  (select-window win)
	  (set-window-buffer (split-window-vertically)
			     dead-buf))))))

;;}}}
;;{{{ group common clients

(defun mds-wm-display-group (clients)
  "Display a group of CLIENTS, which is a list of clients.
The screen is split vertically such that all showstat buffers 
go on the top and the output buffers on the bottom.  The top
and bottom panes are then split equally into n smaller panes,
with n being the number of clients."
  (if clients
      (let ((n (length clients))
	    wbot wtop)
	(setq wtop (select-window (display-buffer (mds-client-live-buf (car clients)))))
	(delete-other-windows wtop)
	(setq wbot (split-window-vertically))
	(while clients
	  ;; split output buffers (bottom half of frame)
	  (set-window-buffer wbot (mds-client-out-buf (car clients)))
	  (select-window wbot)
	  (if (> n 1) (setq wbot (split-window-horizontally (/ (window-width) n))))

	  (set-window-buffer wtop (mds-client-live-buf (car clients)))
	  (select-window wtop)
	  (if (> n 1) (setq wtop (split-window-horizontally (/ (window-width) n))))
	  ;; split live buffers (top half of frame)
	  (setq n (1- n)
		clients (cdr clients))))))

(defun mds-wm-group-clients (clients)
  "Group clients with a common base name."
  (let ((alist '())
	id base client)
    (while clients
      (setq client (car clients)
	    id (car (mds-client-id client)))
      (when (string-match "\\([^-]+\\)\\((-[0-9]+\\)?$" id)
	(setq base (match-string 1 id))
	(let ((entry (assoc base alist)))
	  (if entry
	      (setcdr entry (cons client (cdr entry)))
	    (setq alist (cons (cons base (list client)) alist)))))
      (setq clients (cdr clients)))
    (mapcar #'cdr alist)))

(defun mds-wm-group-update (clients)
  "Call `mds-wm-group-clients' to group the clients, and
assign the list to `mds-wm-grouped-clients'."
  (setq mds-wm-grouped-clients (mds-wm-group-clients mds-clients)))

;;}}}
;;{{{ mds-wm-cycle-clients

(defun mds-wm-cycle-clients (&optional backwards)

;; If client is not visible, make it so without rotating.
;; That should probably be a seperate function

  "Rotate the ... to first client in on list, then rotate list.
If BACKWARDS is non-nil, rotate backwards, otherwise rotate forwards."
  (interactive)
  (if mds-clients
      (let ((client (car mds-clients)))
	(and (> (length mds-clients) 1)
	     ;; client is already displayed
	     (get-buffer-window (mds-client-live-buf client) mds-frame)
	     ;; rotate list
	     (setq mds-clients
		   (if 'backwards
		       ;; FIXME: not the most efficient technique
		       (cons (car (setq mds-clients (reverse mds-clients))) (reverse (cdr mds-clients)))
		     (reverse (cons client (reverse (cdr mds-clients))))
		     )))
	;; display and return the client
	(mds-wm-display-client (car mds-clients)))))

    

(defun mds-wm-cycle-groups ()
  "Pop to first group of clients on list, then rotate list."
  (interactive)
  (if mds-wm-grouped-clients
      (let* ((G mds-wm-grouped-clients)
	     (g (car G)))
	(and (> (length G) 1)
	     ;; group is already displayed
	     ;; (get-buffer-window (mds-client-live-buf client))
	     ;; rotate list
	     (setq G (reverse (cons g (reverse (cdr G))))))
	;; display the live buffer.  Maybe the whole thing.
	(let ((g (car G)))
	  (if (listp g)
	      (mds-wm-display-group g)
	    (mds-wm-display-client g))
	  (setq mds-wm-grouped-clients G)))
    (message "no debugger groups")))


(defvar mds-wm-group nil)
;; (setq L '#1=(a b c . #1#))

;; (defun mds-wm-circ (L)
;;   (nconc L L))

;; (setq L (mds-wm-circ '("a" "b" "c")))

;; (let* ((x (car L))
;;        (y (cadr L)))
;;   (while (not (eq x y))
;;     (insert y)
;;     (setq L (cdr L)
;; 	  y (cadr L))))


;;}}}

(defun mds-wm-active-client ()
  "Return the active client."
  ;; FIXME:
  (car mds-clients))

;;{{{ Call operating system's window manager to get focus

(defun mds-wm-get-focus-wmctrl ()
  "Call shell program wmctrl to give emacs the focus.
This is only appropriate for a linux system."
  (shell-command "wmctrl -xa emacs"))

;;}}}

;;{{{ mds-wm-toggle-focus-ss-out
(defun mds-wm-toggle-focus-ss-out ()
  "Toggle focus between ss-live and output buffer."
  (interactive)
  (let ((liv (mds-client-live-buf mds-client)))
    (if liv
	(pop-to-buffer (if (eq liv (current-buffer))
			   (mds-client-out-buf mds-client)
			 liv)))))
;;}}}

;;{{{ mds-wm-select-frame-set-input-focus

;; Enhanced from select-frame-set-input-focus
;; to limit the mouse movement.

(defun mds-wm-select-frame-set-input-focus (frame)
  "Select FRAME, raise it, and set input focus, if possible.
If `mouse-autoselect-window' is non-nil, and mouse is not inside
selected window, move mouse pointer to center of FRAME's selected window.  
Otherwise, if `focus-follows-mouse' is non-nil, move mouse cursor to FRAME."
  (select-frame frame)
  (raise-frame frame)
  ;; Ensure, if possible, that FRAME gets input focus.
  (when (memq (window-system frame) '(x w32 ns))
    (x-focus-frame frame))
  ;; Move mouse cursor if necessary.
  (cond
   (mouse-autoselect-window
    (let* ((edges (window-inside-edges (frame-selected-window frame)))
	  (xy (cdr (mouse-position)))
	  (x (car xy))
	  (y (cdr xy)))
      (unless (and x
		   (>= x (nth 0 edges)) (<= x (nth 2 edges))
		   (>= y (nth 1 edges)) (<= y (nth 3 edges)))
	
	;; Move mouse cursor into FRAME's selected window to avoid that
	;; Emacs mouse-autoselects another window.
	(set-mouse-position frame
			    (/ (+ (nth 0 edges) (nth 2 edges)) 2)
			    (/ (+ (nth 1 edges) (nth 3 edges)) 2)))))
   (focus-follows-mouse
    ;; Move mouse cursor into FRAME to avoid that another frame gets
    ;; selected by the window manager.
    (set-mouse-position frame (1- (frame-width frame)) 0))))

;;}}}

(provide 'mds-wm)

;; mds-wm.el ends here

