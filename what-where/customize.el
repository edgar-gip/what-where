;;; what-where/customize.el --- Customization options for `what-where'.
;;
;; Copyright (C) 2017  Edgar Gonzàlez i Pellicer
;;
;; Author: Edgar Gonzàlez i Pellicer <edgar.gip@gmail.com>
;; Keywords: what, where, customization
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

(defconst what-where-default-providers
  '(what-where-ffap-provider
    what-where-numbers-provider)
  "Default set of providers for `what-where'.")

(defgroup what-where ()
  "Customization options for `what-where-mode'."
  :group 'convenience)

(defcustom what-where-providers what-where-default-providers
  "Set of providers for `what-where'."
  :type 'hook
  :options what-where-default-providers
  :group 'what-where)

(provide 'what-where/customize)

;;; what-where/customize.el ends here

;;; Local Variables:
;;; coding: utf-8
;;; End:
