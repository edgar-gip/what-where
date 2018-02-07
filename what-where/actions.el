;;; what-where/actions.el --- Standard actions for `what-where'.
;;
;; Copyright (C) 2017  Edgar Gonzàlez i Pellicer
;;
;; Author: Edgar Gonzàlez i Pellicer <edgar.gip@gmail.com>
;; Keywords: what, where, actions
;; Version: 0.1
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

(defun what-where-copy-action (contents)
  "Return a `what-where-action' to copy CONTENTS to the kill ring."
  (let ((fn `(lambda ()
               (kill-new ,contents))))
    (make-what-where-action :shortcut ?c :description "(C)opy"
                            :function fn
                            :feedback (format "Copied '%s' to kill ring."
                                              contents)
                            :is-terminal-p t)))

(defun what-where-replace-action (contents focus-start focus-end)
  "Return a `what-where-action' to replace the area between FOCUS-START and
FOCUS-END with CONTENTS."
  (let ((fn `(lambda ()
               (with-current-buffer what-where-source-buffer
                 (save-excursion
                   (delete-region ,focus-start ,focus-end)
                   (goto-char ,focus-start)
                   (insert ,contents))))))
    (make-what-where-action :shortcut ?r :description "(R)eplace"
                            :function fn
                            :is-terminal-p t)))

(defun what-where-find-action (focus-start)
  "Return a `what-where-action' to find the file starting at FOCUS-START (using
`ffap')."
  (let ((fn `(lambda ()
               (when (and what-where-source-buffer
                          (buffer-live-p what-where-source-buffer))
                 (let ((window (get-buffer-window what-where-source-buffer)))
                   (unless window
                     (setf window selected-window))
                   (with-selected-window window
                     (switch-to-buffer what-where-source-buffer)
                     (save-excursion
                       (goto-char ,focus-start)
                       (call-interactively 'ffap-other-window))))))))
    (make-what-where-action :shortcut ?f :description "(F)ind"
                            :function fn
                            :is-terminal-p 'early)))

(provide 'what-where/actions)

;;; what-where/actions.el ends here

;;; Local Variables:
;;; coding: utf-8
;;; End:
