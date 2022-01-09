;;; what-where/utils.el --- Utilities for `what-where'.
;;
;; Copyright (C) 2017  Edgar Gonzàlez i Pellicer
;;
;; Author: Edgar Gonzàlez i Pellicer <edgar.gip@gmail.com>
;; Keywords: what, where, utils
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

(require 'cl-lib)

(defmacro what-where-features (&rest specs)
  "Expand a feature list specification for use inside a `what-where-item'."
  (cl-flet* ((expand-value (value)
                (if (or (not (listp value))
                        (not (= (length value) 2)))
                    value
                  (let ((functor (cl-first value))
                        (argument (cl-second value)))
                    (cl-case functor
                      (*b*
                       `(if ,argument 1 0))
                      (otherwise
                       value)))))
             (expand-feature (name arguments expanded-value)
               (cond
                ((symbolp name)
                 (cl-assert (null arguments))
                 `(cons ',name ,expanded-value))
                ((stringp name)
                 (if arguments
                     `(cons (intern (format ,name ,@arguments))
                            ,expanded-value)
                   `(cons (intern ,name) ,expanded-value)))
                (t
                 (error "Bad feature name: %s" name))))
             (expand-spec (spec)
               (cond
                ((symbolp spec)
                 `(cons ',spec 1))
                ((listp spec)
                 (let ((name (cl-first spec))
                       (value (cl-first (last (cl-rest spec))))
                       (arguments (butlast (cl-rest spec))))
                   (expand-feature name arguments
                                   (expand-value value))))
                (t
                 (error "Bad feature spec: %s" feature-spec)))))
    (let ((processed-features (mapcar #'expand-spec specs)))
      `(list ,@processed-features))))

(provide 'what-where/utils)

;;; what-where/utils.el ends here

;;; Local Variables:
;;; coding: utf-8
;;; End:
