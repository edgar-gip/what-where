;;; what-where.el --- Find what you look at and where you are.
;;
;; Copyright (C) 2017-2025  Edgar Gonzàlez i Pellicer
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

(require 'what-where/actions)
(require 'what-where/customize)
(require 'what-where/items)
(require 'what-where/ranker)
(require 'what-where/report)
(require 'what-where/utils)

(require 'what-where/ffap-provider)
(require 'what-where/numbers-provider)

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

;;;###autoload
(defun what-where (nofocus)
  "Display what you look at and where you are."
  (interactive "P")
  (what-where-generate-items)
  (if what-where-items
      (what-where-report-render-items nofocus)
    (when what-where-beep-on-no-items (beep))
    (message "No items found.")))

(defvar what-where-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map what-where-hotkey 'what-where)
    map)
  "Keymap for `what-where-mode'.")

;;;###autoload
(define-minor-mode what-where-mode
  "Minor mode to find what you look at and where you are."
  :init-value nil
  :lighter " WW"
  :keymap what-where-mode-map)

;;;###autoload
(define-globalized-minor-mode global-what-where-mode what-where-mode
  (lambda () (what-where-mode t)))

(provide 'what-where)

;;; what-where.el ends here

;;; Local Variables:
;;; coding: utf-8
;;; End:
