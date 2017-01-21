;;; what-where.el --- Find what you look at and where you are.
;;
;; Copyright (C) 2017  Edgar Gonzàlez i Pellicer
;;
;; Author: Edgar Gonzàlez i Pellicer <edgar.gip@gmail.com>
;; Keywords: what, where
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
;; In order to enable this mode, `require' this file and add `what-where-mode'
;; to the desired mode hooks:
;;
;;   (require 'what-where)
;;   (add-hook 'c++-mode-hook 'what-where-mode)

;; This file is *NOT* part of GNU Emacs.

;;; Code:

(require 'cl-lib)

(defconst what-where-default-providers
  '(what-where-dummy)
  "Default set of providers for `what-where`.")

(defgroup what-where ()
  "Customization options for `what-where-mode'.")

(defcustom what-where-providers what-where-default-providers
  "Set of providers for `what-where`."
  :type 'hook
  :options what-where-default-providers)

(cl-defstruct what-where-item
  type
  contents
  score
  action)

(defvar what-where-items ()
  "Current set of items found by `what-where'.")

(defun what-where-clear-items ()
  "Clear the set of items in `what-where-items'."
  (setf what-where-items ()))

(defun what-where-add-item (item)
  "Add ITEM to `what-where-items'."
  (push item what-where-items))

(defun what-where-dummy ()
  "Dummy `what-where' provider."
  (let ((item0 (make-what-where-item :type "Type0"
                                     :contents "Contents0"
                                     :score 0.0
                                     :action #'(lambda ()
                                                 (message "Action0"))))
        (item1 (make-what-where-item :type "Type1"
                                     :contents "Contents1"
                                     :score 0.1
                                     :action #'(lambda ()
                                                 (message "Action1"))))
        (item2 (make-what-where-item :type "Type2"
                                     :contents "Contents2"
                                     :score 0.2)))
    (what-where-add-item item0)
    (what-where-add-item item1)
    (what-where-add-item item2)))

(defun what-where-report-refresh ()
  "Refresh the display in the `what-where-report-mode' window."
  (setq tabulated-list-entries nil)
  (let ((i 0))
    (dolist (item what-where-items)
      (push (list i (vector (what-where-item-type item)
                            (what-where-item-contents item)
                            (number-to-string (what-where-item-score item))))
            tabulated-list-entries)
      (incf i))))

(defun what-where-report-goto ()
  "Trigger the action for the current item in `what-where-report-mode'."
  (interactive)
  (let ((line (1- (line-number-at-pos))))
    (when (and (>= line 0) (< line (length what-where-items)))
      (let* ((item (nth line what-where-items))
             (action (what-where-item-action item)))
        (if action
            (funcall action)
          (message "No action defined"))))))

(defvar what-where-report-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "<return>") 'what-where-report-goto)
    map)
  "Local keymap for `what-where-report-mode'.")

(define-derived-mode what-where-report-mode
  tabulated-list-mode "what-where-report"
  "Major mode for the results of a `what-where' query."
  :group 'what-where
  :syntax-table nil
  :abbrev-table nil
  (setq tabulated-list-format [("Type" 8 t)
                               ("Contents" 40 nil)
                               ("Score" 8 t)])
  (setq tabulated-list-sort-key (cons "Score" t))
  (tabulated-list-init-header)
  (hl-line-mode))

(defun what-where (nofocus)
  "Display what you look at and where you are."
  (interactive "P")
  (what-where-clear-items)
  (run-hooks 'what-where-providers)
  (let ((buffer (get-buffer-create "*What/Where*")))
    (with-current-buffer buffer
      (what-where-report-mode)
      (what-where-report-refresh)
      (tabulated-list-print t)
      (if nofocus
          (display-buffer buffer)
        (pop-to-buffer buffer)
        (hl-line-highlight))
      (goto-char (point-min)))))

(defvar what-where-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c !") 'what-where)
    map)
  "Keymap for `what-where-mode'.")

(define-minor-mode what-where-mode
  "Minor mode to find what you look at and where you are."
  :init-value nil
  :lighter nil
  :keymap what-where-mode-map)

(provide 'what-where)

;;; what-where.el ends here

;;; Local Variables:
;;; coding: utf-8
;;; End:
