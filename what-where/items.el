;; -*- lexical-binding: t -*-

;;; what-where/items.el --- Items for `what-where'.
;;
;; Copyright (C) 2017-2025  Edgar Gonzàlez i Pellicer
;;
;; Author: Edgar Gonzàlez i Pellicer <edgar.gip@gmail.com>
;; Keywords: what, where, items
;; Version: 0.2
;;
;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This file is `require'd by what-where.el, and should not need to be loaded
;; explicitly.

;; This file is *NOT* part of GNU Emacs.

;;; Code:

(require 'cl-lib)

(cl-defstruct (what-where-item
               (:constructor what-where-make-item)
               (:copier nil))
  focus-start
  focus-end
  type
  contents
  features
  actions
  (score nil))

(defvar what-where-items ()
  "Current set of items found by `what-where'.")

(defvar what-where-selected-item ()
  "First item selected by the user among those in `what-where-items'.")

(defun what-where-clear-items ()
  "Clear the set of items in `what-where-items'."
  (setf what-where-items ())
  (setf what-where-selected-item nil))

(defun what-where-add-item (item)
  "Add ITEM to `what-where-items'."
  (push item what-where-items))

(defun what-where-select-item (item)
  "Set the ITEM as `what-where-selected-item', unless another one has already
been selected for the current query."
  (unless what-where-selected-item
    (setf what-where-selected-item item)))

(defvar what-where-source-buffer nil
  "Source buffer that `what-where' was called from.")

(defun what-where-set-source-buffer (buffer)
  "Set `what-where-source-buffer' to BUFFER."
  (setf what-where-source-buffer buffer))

(provide 'what-where/items)

;;; what-where/items.el ends here

;;; Local Variables:
;;; coding: utf-8
;;; End:
