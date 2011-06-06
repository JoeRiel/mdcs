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

(defun mds-windows-create (client)
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
	    (mds-windows-create client))
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

(provide 'mds-windows)

