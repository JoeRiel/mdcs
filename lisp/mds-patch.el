(defun mds-ss-patch ()
  "Copy the showstat buffer to a new buffer for patching."
  (interactive)
  ;; Save point position, begin editing there.
  ;; Use the procname as the buffer name (more or less).
  ;; Remove statement numbers and debug symbols
  ;; Store address in buffer-local variable (mds-patch-address)
  ;; Indent the code
  (let ((ss-buf (current-buffer) ; assume we are in a ss-buf
	(point (point)))
    (set-buffer (get-buffer-create mds-ss-procname))
    (erase-buffer)
    (insert-buffer-substring ss-buf)
    (mds-patch-remove-numbers)
    (mds-patch-mode)
    (goto-char (point))
    (switch-to-buffer (current-buffer)))))

(define-derived-mode mds-patch-mode maplev-proc-mode "Maple Patch Mode"
  "Major mode for live patching a Maple procedure."
  :group 'mds
  )

(defconst mds--statement-number-and-marks-re "^\\s-*[1-9][0-9]*[ *?]"
  "Regexp that matches from the left margin to the ...")

(defun mds-patch-remove-numbers ()
  "Remove the statement numbers and debug markers from a buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward mds-patch-state mds--statement-number-and-marks-re nil t)
      ;; replace match with spaces
      