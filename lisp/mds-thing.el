(global-set-key [f11] 'mds-symbol-at-point)

(defun mds-symbol-at-point ()
  (interactive)
  (thing-at-point 'symbol))