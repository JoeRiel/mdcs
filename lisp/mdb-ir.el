;; mdb-ir.el -- Input ring for handling command history

;; Copyright (C) 2009 Joseph S. Riel.

;; Author: Joe Riel <jriel@maplesoft.com>
;; Keywords: ring command history

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
;;{{{ Commentary

;; This code defines a data-structure and commands suitable for
;; handling the command-history of a process in a shell.  The
;; `mdb-ir-create' command returns the empty data structure; it is called
;; with a size and marker arguments.  The size sets the maximum size
;; of the ring, the marker points to the left side of the current
;; input line in the buffer, to the right of the prompt.
;;
;; The data-structure contains a ring that can grow to its allocated
;; size.  Strings are entered into the ring with the `mdb-ir-enter'
;; command; it grabs the input line of the associated
;; buffer. Scrolling the history, via the `mdb-ir-scroll' command,
;; displays the stored strings in the buffer at the marker saved in
;; the structure.
;;
;; The `mdb-ir-import' and `mdb-ir-export' commands import and export,
;; respectively, a list of strings.  The `mdb-ir-clear' command clears
;; the ring.

;;}}}
;;{{{ Data structure

;; The data structure of the input ring consists of a dotted pair of
;; dotted pairs; that permits accessing any element with two basic
;; operations.  Here is the layout:
;;
;;   ((hd . vc) . (cu . mk))
;;
;; hd: index into vc; it points to the head of the ring, which contains
;;     the newest entry.
;; cu: index into vc; it points to the current location in the ring,
;;     which is decreased and increased when scrolling through the ring
;;     to access elements.  Nil indicates that no scrolling has occurred 
;;     since the last entry.
;; vc: a vector that stores the strings.  The vector (as all elisp vectors)
;;     is indexed from 0 to n-1, with n being the size.
;; mk: a marker that is supposed to point to the right of the prompt of
;;     the current line in the buffer.  It determines where to get/insert
;;     the input line.

;; The following local procedures set and get the elements in the
;; data-structure.  They are inlined to be fast; but that should be
;; insignificant.  They are not intended to be used by external
;; routines.

(defsubst mdb-ir--get-hd (mdb-ir) (caar mdb-ir))
(defsubst mdb-ir--get-vc (mdb-ir) (cdar mdb-ir))
(defsubst mdb-ir--get-cu (mdb-ir) (cadr mdb-ir))
(defsubst mdb-ir--get-mk (mdb-ir) (cddr mdb-ir))

(defsubst mdb-ir--set-hd (mdb-ir hd) (setcar (car mdb-ir) hd))
(defsubst mdb-ir--set-cu (mdb-ir cu) (setcar (cdr mdb-ir) cu))

;;}}}
;;{{{ Exported commands

;; These are the commands used to access the input-ring
;; and display its content.

(defun mdb-ir-create (size mark)
  "Create an input-ring data-structure that can hold SIZE elements.
MARK is a marker in the input buffer that points to the right of
the prompt; it is used to insert text and to get the current
line.  

The initialized elements are nil, the head index is at SIZE-1,
and the current index is nil."
  (unless (and (integerp size) (> size 0)) 
    (error "The size must be an integer greater than 0"))
  (unless (markerp mark) (error "The mark is not a marker"))
  (cons (cons (1- size) (make-vector size nil)) (cons nil mark)))

(defun mdb-ir-clear (mdb-ir)
  "Clear the input-ring MDB-IR.
The vector is filled with nil, the head index is set to 0, and
the current index to nil."
  (fillarray (mdb-ir--get-vc mdb-ir) nil)
  (mdb-ir--set-hd mdb-ir 0)
  (mdb-ir--set-cu mdb-ir nil))

(defun mdb-ir-enter (mdb-ir)
  "Enter the line in the buffer into input-ring MDB-IR.
Check whether the inserted ITEM matches the preceding item, if
so, do not insert it.  Null the current-index."
  (let ((hd (mdb-ir--get-hd mdb-ir))
	(vc (mdb-ir--get-vc mdb-ir))
	(cu (mdb-ir--get-cu mdb-ir))
	(in (mdb-ir--get-line mdb-ir)))
    (if (null cu)
	;; No partial entry, so just check whether the input
	;; matches the current content of hd.  If so, do nothing,
	;; otherwise insert it and increment hd.
	(unless (equal in (aref vc hd))
	  (mdb-ir--insert-only mdb-ir in))
      ;; A partial entry has already been inserted at hd.  If it
      ;; equals the previous entry, then clear it and roll-back the
      ;; head index; otherwise replace it with the current input.
      (let* ((pr (mod (1- hd) (length vc)))
	     (pv (aref vc pr)))
	(if (equal pv in)
	    ;; Clear the partial-entry and roll-back the head-index.
	    (progn
	      (aset vc hd nil)
	      (mdb-ir--set-hd mdb-ir pr))
	  ;; Replace the partial-entry.
	  (aset vc hd in))))
    ;; Clear current index
    (mdb-ir--set-cu mdb-ir nil)))

(defun mdb-ir-scroll (mdb-ir &optional next)
  "Scroll the input-ring MDB-IR and update the display.
If the optional argument NEXT is non-nil, scroll forward (to newer entries),
otherwise scroll backward, to later entries.

  If not done already, push current line into ring, MDB-IR.
Display new input, moving point to either mark or, if the input
line is the head position, at the end of line."
  (if (null (mdb-ir--get-cu mdb-ir))
      ;; Current location is nil, which means we are at head and the
      ;; input line has not been inserted.  Insert input into the
      ;; ring, then set current-index to head.
      (progn
	(mdb-ir--insert-only mdb-ir (mdb-ir--get-line mdb-ir))
	(mdb-ir--set-cu mdb-ir (mdb-ir--get-hd mdb-ir)))
    (when (= (mdb-ir--get-cu mdb-ir) (mdb-ir--get-hd mdb-ir))
      ;; Current-index has returned to head; consequently replace,
      ;; rather than insert, the input line into the ring.
      (aset (mdb-ir--get-vc mdb-ir) (mdb-ir--get-hd mdb-ir) (mdb-ir--get-line mdb-ir))))
  (let* ((vc (mdb-ir--get-vc mdb-ir))
	 (ln (length vc))
	 (cu (mod (+ (if next 1 -1)
		     (mdb-ir--get-cu mdb-ir)) ln))
	 (curr (aref vc cu)))
    (mdb-ir--delete-line mdb-ir)
    (if curr
	(progn
	  ;; Replace input line with the current string,
	  ;; and update the current-index in the ring.
	  (insert curr)
	  (mdb-ir--set-cu mdb-ir cu))
      ;; The current location in the vector is nil; move across the
      ;; the gap, which extends from hd+1 to the end of the vector.
      ;; N.B. once the ring is full, a one-cell gap can appear when
      ;; scrolling from a duplicate entry.  Because that gap, which
      ;; follows the head index, is filled when scrolling, it never
      ;; triggers this clause.
      (setq cu (mdb-ir--set-cu mdb-ir (if next
				  0
				(mdb-ir--get-hd mdb-ir))))
      (insert (aref vc cu))
      ;; Move point to appropriate location.
      (mdb-ir--move-point mdb-ir))))

(defun mdb-ir-export (mdb-ir)
  "Return a list of the strings in the input-ring MDB-IR.
The first string is the oldest, the last the newest."
  (let* ((hd (mdb-ir--get-hd mdb-ir))
	 (vc (mdb-ir--get-vc mdb-ir))
	 (ln (length vc))
	 (ix hd)
	 (lst nil)
	 elem)
    ;; Cons up a list.
    (while (and (setq elem (aref vc ix))            ;; elem <> empty
		(/= hd (setq ix (mod (1- ix) ln)))) ;; hd <> i--
      (setq lst (cons elem lst)))
    lst))

(defun mdb-ir-import (mdb-ir lst)
  "Import into the input-ring MDB-IR the list of strings LST.
The elements are added following the head of the list, and the
head repositioned.  Non-string elements in the list are not
inserted.

The head-index is positioned to point to the last element in the
list, the current-index is set to nil."
  (let (elem)
    (while lst
      (if (stringp (setq elem (pop lst)))
	  (mdb-ir--insert-only mdb-ir elem))))
  (mdb-ir--set-cu mdb-ir nil))

;;}}}
;;{{{ Internal functions

(defun mdb-ir--get-line (mdb-ir)
  "Return the input-line of the buffer associated with the input-ring, MDB-IR.
The input line is from the marker to the end of the line."
  (with-current-buffer (mdb-ir--buffer mdb-ir)
    (buffer-substring-no-properties (mdb-ir--get-mk mdb-ir) (line-end-position))))

(defun mdb-ir--delete-line (mdb-ir)
  "Delete the input line of the buffer associated with the input-ring, MDB-IR.
The input line is from the marker to the end of the line."
  (with-current-buffer (mdb-ir--buffer mdb-ir)
    (delete-region (mdb-ir--get-mk mdb-ir) (line-end-position))))

(defun mdb-ir--insert-only (mdb-ir item)
  "Insert into input-ring MDB-IR item ITEM.
Increment the head index modulo the vector length."
  (let* ((vc (mdb-ir--get-vc mdb-ir))
	 (hd (mdb-ir--set-hd mdb-ir (mod (1+ (mdb-ir--get-hd mdb-ir)) (length vc)))))
    (aset vc hd item)))

(defun mdb-ir--move-point (mdb-ir)
  "Move point to appropriate position in input buffer of MDB-IR.
This is called after scrolling.  If back at the head position,
goto the end of line; otherwise goto the mark.

This provides a visual cue that we have returned; it also puts
point in a useful place.  Better might be to save the home
location, but that requires another memory slot."
  (let ((mk (mdb-ir--get-mk mdb-ir))
	(cu (mdb-ir--get-cu mdb-ir))
	(hd (mdb-ir--get-hd mdb-ir)))
    (with-current-buffer (mdb-ir--buffer mdb-ir)
      (goto-char (if (and cu (/= cu hd))  ; not home
		     mk
		   (line-end-position))))))

(defun mdb-ir--mdb-ir-p (mdb-ir)
  "Return t if MDB-IR is an input-ring data-structure, othewise nil.

This performs a surface test; more robust is to verify the contents
of the vector and the values of the indices."
  (and (consp mdb-ir)
       (consp (car mdb-ir))
       (consp (cdr mdb-ir))
       (integerp (mdb-ir--get-hd mdb-ir))
       (vectorp  (mdb-ir--get-vc mdb-ir))
       (markerp  (mdb-ir--get-mk mdb-ir))
       (let ((cu (mdb-ir--get-cu mdb-ir)))
	 (or (null cu)
	     (integerp cu)))))

(defun mdb-ir--buffer (mdb-ir)
  "Return the buffer associated with the input-ring MDB-IR.
If nil, raise an error."
  (let ((mk (mdb-ir--get-mk mdb-ir)))
    (unless (markerp mk)
      (error "No marker assigned to input-ring marker"))
    (or (marker-buffer mk)
	(error "No buffer assigned to input-ring marker"))))

;;}}}

(provide 'mdb-ir)

;;; mdb-ir.el ends here

;; Local Variables:
;; folded-file: nil
;; End:
