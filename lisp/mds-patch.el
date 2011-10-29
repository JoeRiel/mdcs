(defun mds-ss-patch ()
  "Copy the showstat buffer to a new buffer for patching."
  (interactive)
  ;; Save point position, begin editing there.
  ;; Use the procname as the buffer name (more or less).
  ;; Remove statement numbers and debug symbols
  ;; Store address in buffer-local variable (mds-patch-address)
  ;; Indent the code
  (let ((ss-buf (current-buffer)) ; assume we are in a ss-buf
	(ss-addr mds-ss-addr)
	(client mds-client)
	(point (point)))
    (set-buffer (get-buffer-create mds-ss-procname))
    (erase-buffer)
    (insert-buffer-substring ss-buf)
    ;; delete invisible address string
    (goto-char (point-min))
    (forward-line)
    (delete-region (point-min) (point))
    
    ;; remove numbers and marks, change mode, indent
    (goto-char point)
    (save-excursion
      (mds-patch-remove-numbers)
      (mds-patch-mode)
      (setq mds-ss-addr ss-addr
	    mds-client client)
      (maplev-indent-buffer)
      (toggle-truncate-lines 1))
    (switch-to-buffer (current-buffer)))))

(define-derived-mode mds-patch-mode maplev-mode "Maple Patch Mode"
  "Major mode for live patching a Maple procedure."
  :group 'mds
  )

(defconst mds--statement-number-and-marks-re "^\\s-*[1-9][0-9]*[ *?]"
  "Regexp that matches from the left margin to the ...")


(defun mds-patch-remove-numbers ()
  "Remove the statement numbers and debug mark from buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward mds--statement-number-and-marks-re nil t)
      (replace-match ""))))
      
(defun mds-patch-install ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward ":= " (line-end-position))
    (let ((str (buffer-substring-no-properties
		(point) (point-max))))
      (setq str (replace-regexp-in-string "\"" "\\\\\"" str))
      (mds-client-send mds-client
		   (format "mdcInstallPatch(%s,\"%s\")\n"
			   mds-ss-addr
			   str)))))

