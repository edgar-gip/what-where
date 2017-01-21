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

(defgroup what-where ()
  "Customization options for `what-where-mode'.")

(defun what-where (here)
  "Display what you look at and where you are from HERE."
  (interactive "d")
  (message "what-where: %s" here))

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
