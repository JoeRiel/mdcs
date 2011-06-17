(defconst mds--addr-tag-re "<\\([0-9]+\\)>"
  "Regexp that matches an address tag.  The first group matches the address,
the total match includes the delimiters.")

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


(provide 'mds-regexps)