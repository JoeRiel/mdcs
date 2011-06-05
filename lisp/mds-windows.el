;; Simple window management for mds

;; For single-thread debugging, the default window configuration is to
;; split the main window horizontally (emacs-wise), with the live
;; showstat buffer on the left and the output buffer on the left.
;; When the dead showstat buffer is activated, it will replace the
;; output buffer until that buffer receives new output.  Those
;; two will toggle.
;;
;; It isn't clear what to do when the user moves/adds windows.  We shall see.


;; avoid compiler warnings

(declare-function mds--get-client-live-buf "mds")
(declare-function mds--get-client-dead-buf "mds")
(declare-function mds--get-client-out-buf  "mds")

(defun mds-windows-create (client)
  (let ((display-buffer-reuse-frames 't)
	(live-buf (mds--get-client-live-buf client))
	(dead-buf (mds--get-client-dead-buf client))
	(out-buf  (mds--get-client-out-buf  client)))
    (let ((show-window (display-buffer live-buf))
	  other-window)
      (select-window show-window)
      (delete-other-windows show-window)
      (split-window-horizontally)
      (display-buffer out-buf 'not-this-window))))

(provide 'mds-windows)
