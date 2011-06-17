(eval-and-compile
  (require 'maplev))

(defconst mds--addr-tag-re "<\\([0-9]+\\)>"
  "Regexp that matches an address tag.  The first group matches the address,
the total match includes the delimiters.")

(defconst mds--name-re
  (concat maplev--symbol-re                  ; base name
	  "\\(?::-" maplev--symbol-re "\\)*" ; optional module components
          "\\(?:\\[[^][]*\\]\\)*"            ; optional indices
          "\\(?:()\\|([^*][^()]*)\\)*")      ; optional arguments (crude, no parens)
  "Regular expression for Maple names.  Unlike `maplev--name-re',
no white-space is allowed between elements.")


(defconst mds--addr-procname-re (concat "\\(" mds--addr-tag-re "\n\\)"
					"\\(" mds--name-re "\\)"
					"\\(?:[: ]? *\\)") 
  "Regexp that matches an address tag and procedure name.
The address, with delimiters, is stored in group 1, just the
address is in group 2, and the procedure name is in group 3.")
					

(defconst mds--debugger-status-re
  (concat "^" mds--addr-tag-re "\n" 
	  "\\([^\n]+\\):\n"
	  "\\s-*\\([1-9][0-9]*\\)[ *?]")
  "Regexp that matches the status output of the debugger.
The first group matches the address, the second matches the
procedure name, the third group matches the state number.")

(defconst mds-start-tag-re "^<\\([^>]+\\)>"
  "Regular expression that matches start tag.
The tag has format <tag-name>.  Group 0 matches the tag,
group 1 matches tag-name.")

(defconst mds--client-attach-re "^open from \\([^\n]+\\)\n$"
  "Regexp to match message when a client attaches.
The first group identifies SOMETHING.")

(defconst mds-end-of-msg-re "---EOM---")

(defun mds-activate-addr-procname (&optional button)
  (when (looking-at mds--addr-procname-re)
;;   (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face font-lock-builtin-face)
    (put-text-property (match-beginning 1) (match-end 1) 'invisible t)
    (if button
	(if (string= (match-string 3) "TopLevel")
	    (put-text-property (match-beginning 3) (match-end 3) 'font-lock-face 'mds-inactive-link-face)
	  (make-text-button (match-beginning 3) (match-end 3) :type button)))))

(provide 'mds-regexps)
