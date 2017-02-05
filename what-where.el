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
(require 'popup)
(require 'what-where/utils)

(defconst what-where-default-providers
  '(what-where-ffap-provider
    what-where-numbers-provider)
  "Default set of providers for `what-where'.")

(defgroup what-where ()
  "Customization options for `what-where-mode'.")

(defcustom what-where-providers what-where-default-providers
  "Set of providers for `what-where'."
  :type 'hook
  :options what-where-default-providers
  :group 'what-where)

(cl-defstruct what-where-action
  shortcut
  description
  function
  feedback
  is-terminal-p)

(cl-defstruct what-where-item
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

(defun what-where-generate-items ()
  "Generate and score the set of items for the current point."
  (when what-where-items
    (what-where-ranker-update))
  (what-where-clear-items)
  (what-where-set-source-buffer (current-buffer))
  (run-hooks 'what-where-providers)
  (what-where-ranker-score-items)
  (setf what-where-items
        (sort what-where-items
              #'(lambda (a b)
                  (> (what-where-item-score a) (what-where-item-score b))))))

(defvar what-where-source-buffer nil
  "Source buffer that `what-where' was called from.")

(defun what-where-set-source-buffer (buffer)
  "Set `what-where-source-buffer' to BUFFER."
  (setf what-where-source-buffer buffer))

(defun what-where (nofocus)
  "Display what you look at and where you are."
  (interactive "P")
  (what-where-generate-items)
  (what-where-report-render-items nofocus))

(defvar what-where-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c !") 'what-where)
    map)
  "Keymap for `what-where-mode'.")

(define-minor-mode what-where-mode
  "Minor mode to find what you look at and where you are."
  :init-value nil
  :lighter " WW"
  :keymap what-where-mode-map)

(require 'what-where/actions)
(require 'what-where/ranker)
(require 'what-where/report)

(require 'what-where/ffap)
(require 'what-where/numbers)

(provide 'what-where)

;;; what-where.el ends here

;;; Local Variables:
;;; coding: utf-8
;;; End:
