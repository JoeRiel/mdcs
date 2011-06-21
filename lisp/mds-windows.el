;; Simple window managemer for mds

;; For single-thread debugging, the default window configuration is to
;; split the main window horizontally (emacs-wise), with the live
;; showstat buffer on the left and the output buffer on the right.
;; When the dead showstat buffer is activate, it vertically splits the
;; output buffer.


;; avoid compiler warnings

(declare-function mds--get-client-live-buf "mds")
(declare-function mds--get-client-dead-buf "mds")
(declare-function mds--get-client-out-buf  "mds")

;;(defvar mds-clients '())
(defvar mds-windows-grouped-clients '())


(defun mds-windows-display-client (client)
  "Display the live-showstat buffer and the output buffer of CLIENT.
The buffers are displayed in side-by-side windows that fill the
frame, the showstat buffer on the left.  Return nil."
  (let ((display-buffer-reuse-frames 't))
    (delete-other-windows (select-window (display-buffer (mds--get-client-live-buf client))))
    (set-window-buffer (split-window-horizontally) (mds--get-client-out-buf client))))


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

;; Not used, but may later	  
(defun mds-get-tallest-window (windows)
  "Return the tallest window in the list WINDOWS."
  (let ((mx 0) ht win)
    (while windows
      (setq ht (window-height (car windows)))
      (if (> ht mx)
	  (setq mx ht
		win (car windows)))
      (setq windows (cdr windows)))
    win))

(defun mds-windows-display-client-pair (client1 client2)
  "Display the live-showstat buffer and output buffer of CLIENT1 and CLIENT2
in a four-window display."
  (let* ((display-buffer-reuse-frames 't)
	 (wtop (select-window (display-buffer (mds--get-client-live-buf client1))))
	 wbot)
    (delete-other-windows wtop)
    (set-window-buffer (setq wbot (split-window-vertically)) (mds--get-client-out-buf client1))
    (select-window wbot)
    (set-window-buffer (split-window-horizontally) (mds--get-client-out-buf client2))
    (select-window wtop)
    (set-window-buffer (split-window-horizontally) (mds--get-client-live-buf client2))))

(defun mds-windows-select-and-display-client-pair ()
  (interactive)
  (let* ((clients (mds-select-accepted-clients mds-clients))
	 (len (length clients)))
    (cond
     ((< 2 len)	
      (error "less than two registered clients"))
     ((= 2 len)
      (mds-windows-display-client-pair (nth 0 clients) (nth 1 clients)))
     (t
      ;; hack for now to avoid selecting them
      (error "need to write code to select 2 of the clients")))))

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
	  (set-window-buffer wtop (mds--get-client-live-buf (car clients)))
	  (select-window wtop)
	  (if (> n 1) (setq wtop (split-window-horizontally (/ (window-width) n))))
	  (set-window-buffer wbot (mds--get-client-out-buf (car clients)))
	  (select-window wbot)
	  (if (> n 1) (setq wbot (split-window-horizontally (/ (window-width) n))))
	  (setq n (1- n)
		clients (cdr clients))))))

(defun mds-windows-group-clients (clients)
  "Group clients with a common base name."
  (let ((alist '())
	id base client)
    (while clients
      (setq client (car clients)
	    id (mds--get-client-id client))
      (when (string-match "\\([^-]+\\)-[0-9]+$" id)
	(setq base (match-string 1 id))
	(let ((entry (assoc base alist)))
	  (if entry
	      (setcdr entry (cons client (cdr entry)))
	    (setq alist (cons (cons base (list client)) alist)))))
      (setq clients (cdr clients)))
    (mapcar #'cdr alist)))


;;(setq mds-windows-grouped-clients (mds-windows-group-clients mds-clients))


;;{{{ mds-windows-cycle-clients

(defun mds-windows-cycle-clients ()
  "Pop to first group of client on list, then rotate list."
  (interactive)
  (if mds-clients
      (let* ((L mds-clients)
	     (client (car L)))
	(and (> (length L) 1)
	     ;; client is already displayed
	     (get-buffer-window (mds--get-client-live-buf client))
	     ;; rotate list
	     (setq mds-clients (reverse (cons client (reverse (cdr L))))))
	;; display the live buffer.  Maybe the whole thing.
	(mds-windows-display-client (car mds-clients)))))

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

(provide 'mds-windows)

