;;; mds-thing.el --- Create mds things


;;; Commentary:
;; 


;;; Code:

(require 'thingatpt)

(defun mds-thing-bounds-of-procname-at-point ()
  "Return the start and end points of the procedure name at point.
The result is a paired list of character positions for the
procedure name located at the current point in the current
buffer."
  (save-excursion
    (let ((pps (parse-partial-sexp (line-beginning-position) (point))))
      (if (and (nth 3 pps) (= (nth 3 pps) ?`))
	  (goto-char (nth 8 pps))
	(if (looking-back "`\\(:-?\\)?")
	    (goto-char (+ 3 (match-beginning 0)))
	  (if (and (not (bolp)) (looking-back "\\S-"))
	      (forward-symbol -1))))
      (while (looking-back "\\S-:-")
	(if (string= (match-string 0) "`:-")
	    (re-search-backward "`" (line-beginning-position) nil 2)
	  (forward-symbol -1)))
      (if (and (not (eolp)) (looking-at "\\S-"))
	  (let ((beg (point)))
	    ;; move to end
	    (while (progn
		     (if (looking-at "`")
			 (re-search-forward "`" (line-end-position) nil 2)
		       (forward-symbol 1))
		     (if (looking-at ":-")
			 (or (forward-char 2) t))))
	    (cons beg (point)))))))

;; With this put, we can now use (thing-at-point 'procname)
;; to get the procname at point.

(put 'procname 'bounds-of-thing-at-point 'mds-thing-bounds-of-procname-at-point)

(provide 'mds-thing)

;;; mds-thing.el ends here
