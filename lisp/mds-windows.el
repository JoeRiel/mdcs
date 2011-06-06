;; Simple window managemer for mds

;; For single-thread debugging, the default window configuration is to
;; split the main window horizontally (emacs-wise), with the live
;; showstat buffer on the left and the output buffer on the left.
;; When the dead showstat buffer is activated, it will replace the
;; live buffer until that buffer receives new output.  Those
;; two will toggle.
;;
;; It isn't clear what to do when the user moves/adds windows.  We shall see.


;; avoid compiler warnings

(declare-function mds--get-client-live-buf "mds")
(declare-function mds--get-client-dead-buf "mds")
(declare-function mds--get-client-out-buf  "mds")

(defun mds-windows-create (client)
  "Display the live-showstat buffer and the output buffer of CLIENT.
The buffers are displayed in side-by-side windows that fill the
frame, the showstat buffer on the left.  Return nil."
  (let ((display-buffer-reuse-frames 't)
	(live-buf (mds--get-client-live-buf client))
	(out-buf  (mds--get-client-out-buf  client)))
    ;; display live-buf in one window that fills frame
    (delete-other-windows (select-window (display-buffer live-buf)))
    (set-window-buffer (split-window-horizontally) out-buf)))


;; (defun mds-windows-display-dead (client)
;;   "Display the dead showstat-buffer of CLIENT in the window
;; also used by the live buffer."
;;   ;; temporarily set window user options
;;   (let ((display-buffer-reuse-frames 't)
;; 	(same-window-buffer-names 
;; 	 (list (buffer-name (mds--get-client-live-buf client))
;; 	       (buffer-name (mds--get-client-dead-buf client)))))
;;     ;; display the buffer
;;     (display-buffer dead-buf)))

(defun mds-windows-display-dead (client)
    ;; select the output window
  (mds-windows-create client)
  (select-window (display-buffer (mds--get-client-out-buf client)))
  (set-window-buffer (split-window-vertically)
		     (mds--get-client-dead-buf client)))
		     
    

(provide 'mds-windows)

