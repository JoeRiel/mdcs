;;; mds-re.el --- Assign regular expressions

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

;;; Code:

(require 'rx)

(defmacro mds-rx (&rest regexps)
  "Macro that extends `rx'.  REGEXPS follows forms in `rx'.
Taken from www.emacswiki.org/emacs/rx with correction at
emacs.1067599.n5.nabble.com/rx-Adding-custom-sexps-to-work-with-td212025.html.

The 'decor' tag matches the decoration following a statement
number in a showstat buffer; the \"*\" matches a breakpoint, the
\"?\" matches a stopat conditional\, the \"!\" matches the
current position."
  (let* ((add-ins (list
		   `(decor   . ,(rx (opt (any ?\s ?* ??)) (opt ?!))) ; decoration in a showstat buffer
		   `(integer . ,(rx (opt ?-) (+ digit)))
		   `(posint  . ,(rx (any "1-9") (* digit)))
		   `(nonnegint . ,(rx (+ digit)))
		   ))
	 (rx-constituents (append add-ins rx-constituents)))
    (cond ((null regexps)
	   (error "No regexp"))
	  ((cdr regexps)
	   (rx-to-string `(and ,@regexps) t))
	  (t
	   (rx-to-string (car regexps) t)))))

(defconst mds-re-addr-tag
  (mds-rx ?< (group integer) ?>)
  "Regexp that matches an address tag.
The first group matches the address,
the total matches includes the address with delimiters.")

(defconst mds-re-simple-name
  (rx (any "a-z" "A-Z" ?% ?_)
      (* (any "a-z" "A-Z" digit ?_ ??))
      (opt ?~))
  "Regexp for a simple name.")

(defconst mds-re-quoted-name
  (rx ?`
      (* (not (any ?` ?\n ?\\)))  ; normal*
      (* (and ?\\ not-newline (* (not (any ?` ?\n ?\\)))))  ; (special normal*)*
      ?`)
  "Regexp for a Maple quoted name.
It correctly handles escaped back-quotes in a name, but not doubled
back-quotes.  It intentionally fails for the exceptional case where a
name has a newline character.")

(defconst mds-re-symbol
  (concat "\\(?:"
	  mds-re-simple-name
	  "\\|"
	  mds-re-quoted-name
	  "\\)")
  "Regexp for a Maple symbol.")

(defconst mds-re-name
  (concat mds-re-symbol                  ; base name
	  "\\(?::-" mds-re-symbol "\\)*" ; optional module components
          "\\(?:\\[[^[]]*\\]\\)*"         ; optional indices
          "\\(?:()\\|([^*][^()]*)\\)*")   ; optional arguments (crude, no parens)
  "Regexp for Maple names.
Unlike `maplev--name-re', no white-space is allowed between
elements.")

(defconst mds-re-addr-procname
  (concat "\\(" mds-re-addr-tag "\n\\)"
	  "\\(" mds-re-name "\\)"
	  "\\(?:[: ]? *\\)")
  "Regexp that matches an address tag and procedure name.
It has three groups:
\(1) address, with delimiters,
\(2) address,
\(3) procedure name")

(defconst mds-re-line-info
  (concat
   (mds-rx line-start
	   (group (+ (not (any ?\s))))	; filename [assume no space]
	   ?\s
	   (group nonnegint)		; line number
	   ?\s
	   (group nonnegint)		; char offset to beginning
	   ?\s
	   (group nonnegint)		; char offset to end
	   (group (* ?\s nonnegint))	; states with breakpoints
	   ?:)
   mds-re-addr-tag                     ; address
   (mds-rx ?\n
	   (group (+ not-newline))	; procedure name
	   ?: ?\n (* blank)
	   (group posint)		; statement number
	   decor
	   (* blank)
	   (group (* not-newline))))	; statement
  "Regexp that matches the line-info output status output of the debugger.
It has nine groups:
\(1) filename of source,
\(2) line number in source,
\(3) character offset to beginning of statement,
\(4) character offset to end of statement,
\(5) states with breakpoints,
\(6) address of current procedure,
\(7) name of current procedure,
\(8) state number,
\(9) statement")

(defconst mds-re-client-attach
  (rx line-start
      "open from "
      (group (+ not-newline))
      ?\n
      line-end)
  "Regexp to match message when a client attaches.
The first group identifies SOMETHING.")

(defconst mds-re-ss-statement
  (mds-rx line-start
	  (* blank)
	  (group posint)  ; statement number
	  (group decor)   ; decorations
	  (+ blank)
	  (group (not space) (* not-newline))) ; statement
  "Regexp that matches the start of a line in a showstat buffer.
It has three groups:
\(1) statement number,
\(2) decoration,
\(3) Maple statement")

(defconst mds-re-ss-line
  (mds-rx line-start
	  (* blank)
	  (opt (group posint)  ; statement number
	       decor           ; decorations
	       (+ blank))
	  (group (not space) (* not-newline)) ; statement
	  line-end)
  "Regexp that matches a body line in the showstat output.
It has two groups:
\(1) matches the statement number (may be nil),
\(2) matches the Maple statement.")

(defconst mds-re-statement-number-and-marks
  (mds-rx line-start
	  (* blank)
	  posint
	  decor)
  "Regexp that matches the statement number and decoration in a showstat buffer.
The regexp is anchored to the left margin.")

(defun mds-activate-addr-procname (&optional button)
  "If looking at addr-procname, hide address and apply BUTTON to the procname.
If the procname is TopLevel, then just
change its face to `mds-inactive-link'.  Return a cons cell of
the address and procname."
  (if (looking-at mds-re-addr-procname)
      (let ((addr (match-string-no-properties 2))
	    (procname (match-string-no-properties 3)))
	(put-text-property (match-beginning 1) (match-end 1) 'invisible t)
	(if button
	    (if (string= procname "TopLevel")
		(put-text-property (match-beginning 3) (match-end 3) 'font-lock-face 'mds-inactive-link)
	      (make-text-button (match-beginning 3) (match-end 3) :type button)))
	(cons addr procname))))

(provide 'mds-re)

;;; mds-re.el ends here
