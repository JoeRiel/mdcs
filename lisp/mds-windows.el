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
    (set-window-buffer (setq wbot (split-window-vertically) (mds--get-client-out-buf client1))
    (select-window wbot)
    (set-window-buffer (split-window-horizontally) (mds--get-client-out-buf client2))
    (select-window wtop)
    (set-window-buffer (split-window-horizontally) (mds--get-client-live-buf client2))))

(defun mds-windows-select-and-display-client-pair ()
  (interactive)
  (let* ((clients (mds-select-accepted-clients mds-clients))
	 ((len (length clients)))
    (cond
     ((< 2 len)	
      (error "less than two registered clients"))
     ((= 2 len)
      (mds-windows-display-client-pair (nth 0 clients) (nth 1 clients)))
     (t
      ;; hack for now to avoid selecting them
      (error "need to write code to select 2 of the clients")))))
    

(provide 'mds-windows)

