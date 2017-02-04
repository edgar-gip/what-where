;;; what-where/ffap.el --- `ffap'-related provider for `what-where'.
;;
;; Copyright (C) 2017  Edgar Gonzàlez i Pellicer
;;
;; Author: Edgar Gonzàlez i Pellicer <edgar.gip@gmail.com>
;; Keywords: what, where, ffap
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
;; This file is `require'd by what-where.el. To enable the provider, include
;; `what-where/ffap-provider' in `what-where-providers' (it is already part of
;; `what-where-default-providers').

;; This file is *NOT* part of GNU Emacs.

;;; Code:

(require 'cl-lib)

(defun what-where/ffap-provider ()
  "`ffap'-related provider for `what-where'."
  (let ((file (ffap-file-at-point)))
    (when file
      (let* ((focus-start (cl-first ffap-string-at-point-region))
             (focus-end (cl-second ffap-string-at-point-region))
             (features (what-where/features
                        provider:ffap
                        type:file
                        actions:can-copy
                        actions:can-find
                        (file:is-absolute
                         (*b* (file-name-absolute-p file)))))
             (copy-action (what-where/copy-action file))
             (find-action (what-where/find-action focus-start))
             (item (make-what-where-item :focus-start focus-start
                                         :focus-end focus-end
                                         :type "File"
                                         :contents file
                                         :features features
                                         :actions (list copy-action
                                                        find-action))))
        (what-where-add-item item)))))

(provide 'what-where/ffap)

;;; what-where/ffap.el ends here

;;; Local Variables:
;;; coding: utf-8
;;; End:
