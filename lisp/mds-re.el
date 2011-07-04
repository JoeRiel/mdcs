;;; mds-re.el

;; Copyright (C) 2011 Joseph S. Riel, all rights reserved

;; Author:     Joseph S. Riel <jriel@maplesoft.com>
;; Created:    Jan 2011
;; Keywords:   maple, debugger 
;;
;;; Commentary:

;; This file contains the source for the Emacs Maple debugger server.
;; It is a part of the Maple Debugger Client Server package.

;;{{{ License

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc.,  51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;}}}



(defconst mds--addr-tag-re "<\\([0-9]+\\)>"
  "Regexp that matches an address tag.  The first group matches the address,
the total match includes the delimiters.")

(defconst mds--simple-name-re  "[a-zA-Z_][a-zA-Z0-9_]*"
  "Regular expression for a simple name.")

(defconst mds--quoted-name-re  "`[^`\n\\\\]*\\(?:\\\\.[^`\n\\\\]*\\)*`"
  "Regular expression for a Maple quoted name.
It correctly handles escaped backquotes in a name, but not doubled
backquotes.  It intentionally fails for the exceptional case where a
name has a newline character.")

(defconst mds--symbol-re (concat "\\(?:" 
				 mds--simple-name-re 
				 "\\|"
				 mds--quoted-name-re
				 "\\)")
  "Regular expression for a Maple symbol.")

(defconst mds--name-re
  (concat mds--symbol-re                  ; base name
	  "\\(?::-" mds--symbol-re "\\)*" ; optional module components
          "\\(?:\\[[^[]]*\\]\\)*"         ; optional indices
          "\\(?:()\\|([^*][^()]*)\\)*")   ; optional arguments (crude, no parens)
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
	  "\\s-*\\([1-9][0-9]*\\)[ *?]"
	  "\\(.*\\)")
  "Regexp that matches the status output of the debugger.
The first group matches the address, the second matches the
procedure name, the third group matches the state number,
the fourth group matches the statement.")

(defconst mds-procname-assignment-re "^\\([^ \t\n]+\\) := *"
  "Match an assignment to a procedure.
The procname is flush left.  See diatribe in
`mds-ss-where-procname-re'.")


(defconst mds-start-tag-re "^<\\([^>]+\\)>"
  "Regular expression that matches start tag.
The tag has format <tag-name>.  Group 0 matches the tag,
group 1 matches tag-name.")

(defconst mds--client-attach-re "^open from \\([^\n]+\\)\n$"
  "Regexp to match message when a client attaches.
The first group identifies SOMETHING.")

(defconst mds-end-of-msg-re "---EOM---")

(defconst mds-ss-mark-re "^ +[0-9]+[*!]?"
  "Regexp that matches the statement mark added by showstat.")

(defun mds-activate-addr-procname (&optional button)
  "If looking at an address-procname, hide the address and apply
BUTTON to the procname.  If the procname is TopLevel, then just
change its face to `mds-inctive-link-face'.  Return a cons cell of
the address and procname."
  (if (looking-at mds--addr-procname-re)
    (let ((addr (match-string 2))
	  (procname (match-string 3)))
      (put-text-property (match-beginning 1) (match-end 1) 'invisible t)
      (if button
	  (if (string= procname "TopLevel")
	      (put-text-property (match-beginning 3) (match-end 3) 'font-lock-face 'mds-inactive-link-face)
	    (make-text-button (match-beginning 3) (match-end 3) :type button)))
      (cons addr procname))))

(provide 'mds-re)

;; mds-re.el ends ehre