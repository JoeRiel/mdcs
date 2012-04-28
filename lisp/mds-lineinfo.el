;;; mds-lineinfo.el

;; Copyright (C) 2012 Joseph S. Riel, all rights reserved

;; Author:     Joseph S. Riel <jriel@maplesoft.com>
;; Created:    Jan 2012
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

(defvar mds-lineinfo-root NULL
  "String variable that hold the root directory of the lineinfo path.
This suggests that the entire build must be from one directory.")



(defun mds-lineinfo-find-file (file beg end)
  "Open FILE with point at position BEG.
The file is opened in a window different than the live showstat buffer.
The FILE path is relative to `mds-lineinfo-root'.")

(defun mds-lineinfo-



(provide 'mds-lineinfo)

;; mds-lineinfo.el ends ehre