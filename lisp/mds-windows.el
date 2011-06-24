;; Simple window managemer for mds

;; For single-thread debugging, the default window configuration is to
;; split the main window horizontally (emacs-wise), with the live
;; showstat buffer on the left and the output buffer on the right.
;; When the dead showstat buffer is activate, it vertically splits the
;; output buffer.


;; avoid compiler warnings

(declare-function mds--get-client-id "mds")
(declare-function mds--get-client-live-buf "mds")
(declare-function mds--get-client-dead-buf "mds")
(declare-function mds--get-client-out-buf  "mds")

(defvar mds-frame nil "Frame used by mds")

(defvar mds-clients '())  ;; duplicate, here to avoid warnings
(defvar mds-windows-grouped-clients '()
  "List of grouped clients.  Clients which are displayed
together are combined in a sublist.  Clients that are not grouped
are not in sublists.")



(defun mds-windows-display-client (client)
  "Display the live-showstat buffer and the output buffer of CLIENT.
The buffers are displayed in side-by-side windows that fill the
frame, the showstat buffer on the left.  Return nil."
  (select-frame mds-frame)
  (delete-other-windows (select-window (display-buffer (mds--get-client-live-buf client))))
  (set-window-buffer (split-window-horizontally) (mds--get-client-out-buf client)))


(defun mds-windows-display-dead (client)
  "Display the dead showstat buffer of CLIENT in a window."
  (let ((dead-buf (mds--get-client-dead-buf client)))
    (unless (get-buffer-window-list dead-buf)
      (let ((out-buf (mds--get-client-out-buf client)))
	(if (not (get-buffer-window-list out-buf))
	    ;; no visible buffers, use the basic method
	    (mds-windows-display-client client))
	;; split (one of) the visible out-buf windows
	(let ((win (display-buffer out-buf)))
	  (select-window win)
	  (set-window-buffer (split-window-vertically)
			     dead-buf))))))

(defun mds-windows-display-group (clients)
  "Display a group of CLIENTS, which is a list of clients.
The screen is split vertically such that all showstat buffers 
go on the top and the output buffers on the bottom.  The top
and bottom panes are then split equally into n smaller panes,
with n being the number of clients."
  (if clients
      (let ((n (length clients))
	    wbot wtop)
	(setq wtop (select-window (display-buffer (mds--get-client-live-buf (car clients)))))
	(delete-other-windows wtop)
	(setq wbot (split-window-vertically))
	(while clients
	  ;; split output buffers (bottom half of frame)
	  (set-window-buffer wbot (mds--get-client-out-buf (car clients)))
	  (select-window wbot)
	  (if (> n 1) (setq wbot (split-window-horizontally (/ (window-width) n))))

	  (set-window-buffer wtop (mds--get-client-live-buf (car clients)))
	  (select-window wtop)
	  (if (> n 1) (setq wtop (split-window-horizontally (/ (window-width) n))))
	  ;; split live buffers (top half of frame)
	  (setq n (1- n)
		clients (cdr clients))))))

(defun mds-windows-group-clients (clients)
  "Group clients with a common base name."
  (let ((alist '())
	id base client)
    (while clients
      (setq client (car clients)
	    id (car (mds--get-client-id client)))
      (when (string-match "\\([^-]+\\)-[0-9]+$" id)
	(setq base (match-string 1 id))
	(let ((entry (assoc base alist)))
	  (if entry
	      (setcdr entry (cons client (cdr entry)))
	    (setq alist (cons (cons base (list client)) alist)))))
      (setq clients (cdr clients)))
    (mapcar #'cdr alist)))

(defun mds-windows-group-update (clients)
  (setq mds-windows-grouped-clients (mds-windows-group-clients mds-clients)))
  


;;{{{ mds-windows-cycle-clients

(defun mds-windows-cycle-clients (&optional backward)
  "Pop to first client on list, then rotate list.
If dir is positive, then rotate forward, otherwise rotate backward."
  (interactive)
  (if mds-clients
      (let* ((L mds-clients)
	     (client (car L)))
	(and (> (length L) 1)
	     ;; client is already displayed
	     (get-buffer-window (mds--get-client-live-buf client) 'visible)
	     ;; rotate list
	     (setq mds-clients
		   (if 'backward
		       ;; FIXME: not the most efficient technique
		       (cons (car (setq L (reverse L))) (reverse (cdr L)))
		     (reverse (cons client (reverse (cdr L))))
		     ))
	     ;; display the live buffer.  Maybe the whole thing.
	     (mds-windows-display-client (car mds-clients))))))

(defun mds-windows-cycle-groups ()
  "Pop to first group of client on list, then rotate list."
  (interactive)
  (if mds-windows-grouped-clients
      (let* ((G mds-windows-grouped-clients)
	     (g (car G)))
	(and (> (length G) 1)
	     ;; group is already displayed
	     ;; (get-buffer-window (mds--get-client-live-buf client))
	     ;; rotate list
	     (setq G (reverse (cons g (reverse (cdr G))))))
	;; display the live buffer.  Maybe the whole thing.
	(let ((g (car G)))
	  (if (listp g)
	      (mds-windows-display-group g)
	    (mds-windows-display-client g))
	  (setq mds-windows-grouped-clients G)))))

;;}}}


(defun mds-windows-get-focus-wmctrl ()
  "Call shell program wmctrl to give emacs the focus."
  (shell-command "wmctrl -xa emacs"))

(provide 'mds-windows)

