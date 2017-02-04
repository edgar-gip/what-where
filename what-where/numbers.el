;;; what-where/numbers.el --- Number-related provider for `what-where'.
;;
;; Copyright (C) 2017  Edgar Gonzàlez i Pellicer
;;
;; Author: Edgar Gonzàlez i Pellicer <edgar.gip@gmail.com>
;; Keywords: what, where, numbers
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
;; `what-where/numbers-provider' in `what-where-providers' (it is already part
;; of `what-where-default-providers').

;; This file is *NOT* part of GNU Emacs.

;;; Code:

(require 'cl-lib)

(defcustom what-where/numbers-timezone "UTC0"
  "Timezone for the `what-where/numbers' provider."
  :type 'string
  :group 'what-where)

(defcustom what-where/numbers-time-format "%02d-%02d-%02d %02d:%02d:%02d"
  "Format string for timestamps for the `what-where/numbers' provider."
  :type 'string
  :group 'what-where)

(defcustom what-where/numbers-time-min-year 1980
  "Minimum year for the `what-where/numbers' provider."
  :type 'integer
  :group 'what-where)

(defcustom what-where/numbers-time-max-year 2100
  "Maximum year for the `what-where/numbers' provider."
  :type 'integer
  :group 'what-where)

(defcustom what-where/numbers-roman-lowercase nil
  "Whether to lowercase roman numbers in the `what-where/numbers' provider."
  :type 'boolean
  :group 'what-where)

(defcustom what-where/numbers-hexadecimal-prefix
  '((c-mode . "0x") (c++-mode . "0x") (emacs-lisp-mode . "#x"))
  "Prefix to use when displaying hexadecimal numbers in each major mode."
  :type '(alist :key-type symbol :value-type string)
  :group 'what-where)

(defcustom what-where/numbers-octal-prefix
  '((c-mode . "0") (c++-mode . "0") (emacs-lisp-mode . "#o"))
  "Prefix to use when displaying octal numbers in each major mode."
  :type '(alist :key-type symbol :value-type string)
  :group 'what-where)

(defun what-where/looking-at-number ()
  "Returns a list (MATCHED-NUMBER FOCUS-START FOCUS-END) if the
current point is over a number, or NIL otherwise."
  (save-match-data
    (when (looking-at "[[:digit:]]+")
      (let ((matched-string (match-string-no-properties 0))
            (focus-start (point))
            (focus-end (match-end 0)))
        (when (looking-back "[[:digit:]]+" nil t)
          (setf focus-start (match-beginning 0))
          (setf matched-string (concat (match-string-no-properties 0)
                                       matched-string)))
        (let ((matched-number (string-to-number matched-string)))
          (list matched-number focus-start focus-end))))))

(defun what-where/numbers-provider ()
  "Number-related provider for `what-where'."
  (save-match-data
    (let ((match (what-where/looking-at-number)))
      (when match
        (let ((matched-number (cl-first match))
              (focus-start (cl-second match))
              (focus-end (cl-third match)))
          (what-where/numbers-generate-items matched-number
                                             focus-start focus-end))))))

(defun what-where/numbers-generate-items (matched-number focus-start focus-end)
  "Generate possible `what-where-items' for MATCHED-NUMBER, which starts at
FOCUS-START and ends at FOCUS-END."
  (what-where/numbers-generate-timestamp-items matched-number
                                               focus-start focus-end)
  (what-where/numbers-generate-timestamp-items (/ matched-number 1000)
                                               focus-start focus-end)
  (what-where/numbers-generate-timestamp-items (/ matched-number 1000000)
                                               focus-start focus-end)
  (what-where/numbers-generate-roman-items matched-number
                                           focus-start focus-end)
  (what-where/numbers-generate-base10-items matched-number
                                            focus-start focus-end))

(defun what-where/numbers-generate-timestamp-items (matched-number
                                                    focus-start focus-end)
  "Generate possible timestamp-related `what-where-items' for MATCHED-NUMBER,
which starts at FOCUS-START and ends at FOCUS-END."
  (let* ((matched-time (decode-time matched-number what-where/numbers-timezone))
         (year (nth 5 matched-time))
         (seconds (nth 0 matched-time))
         (minutes (nth 1 matched-time))
         (hour (nth 2 matched-time))
         (day (nth 3 matched-time))
         (month (nth 4 matched-time))
         (contents (format what-where/numbers-time-format
                           year month day hour minutes seconds))
         (features (what-where/features
                    provider:numers
                    type:timestamp
                    actions:can-copy
                    actions:can-replace
                    ("numbers:timestamp:log_year:%d"
                     (truncate (log (1+ (abs year)) 10)) 1)
                    ("numbers:timestamp:log_rel_year:%d"
                     (truncate (log (1+ (abs (- year 1970))) 10)) 1)
                    (numbers:timestamp:in-year-range
                     (*b* (<= what-where/numbers-time-min-year year
                              what-where/numbers-time-max-year)))))
         (copy-action (what-where/copy-action contents))
         (replace-action (what-where/replace-action contents
                                                    focus-start focus-end))
         (item (make-what-where-item :focus-start focus-start
                                     :focus-end focus-end
                                     :type "Timestamp"
                                     :contents contents
                                     :features features
                                     :actions (list copy-action
                                                    replace-action))))
        (what-where-add-item item)))

(defconst what-where/numbers-roman-values
  '((1000 . "M") (900 . "CM") (500 . "D") (400 . "CD")
    (100 . "C") (90 . "XC") (50 . "L") (40 . "XL")
    (10 . "X") (9 . "IX") (5 . "V") (4 . "IV") (1 . "I"))
  "Alist with (VALUE . DIGITS) roman number values.")

(defun what-where/numbers-generate-roman-items (matched-number
                                                focus-start focus-end)
  "Generate possible roman-number-related `what-where-items' for MATCHED-NUMBER,
which starts at FOCUS-START and ends at FOCUS-END."
  (when (< matched-number 4000)
    (let ((digits nil)
          (possible-values what-where/numbers-roman-values)
          (remaining matched-number))
      (while (> remaining 0)
        (cl-assert possible-values)
        (let ((current-value (car (car possible-values)))
              (current-digit (cdr (car possible-values))))
          (if (> current-value remaining)
              (setf possible-values (cdr possible-values))
            (push current-digit digits)
            (decf remaining current-value))))
      (let* ((all-digits (apply #'concat (nreverse digits)))
             (contents (if what-where/numbers-roman-lowercase
                           (downcase all-digits) all-digits))
             (features (what-where/features
                        provider:numers
                        type:roman
                        actions:can-copy
                        actions:can-replace
                        ("numbers:roman:log:%d"
                         (truncate (log (1+ (abs matched-number)) 10)) 1)))
             (copy-action (what-where/copy-action contents))
             (replace-action (what-where/replace-action contents
                                                        focus-start focus-end))
             (item (make-what-where-item :focus-start focus-start
                                         :focus-end focus-end
                                         :type "Roman"
                                         :contents contents
                                         :features features
                                         :actions (list copy-action
                                                        replace-action))))
        (what-where-add-item item)))))

(defun what-where/numbers-generate-base10-items (matched-number
                                                 focus-start focus-end)
  "Generate possible base-conversion-related `what-where-items'
for MATCHED-NUMBER, which starts at FOCUS-START and ends at
FOCUS-END, and which is assumed to be in base 10."
  (let ((hexadecimal-parent-mode
         (apply #'derived-mode-p
                (mapcar #'car what-where/numbers-hexadecimal-prefix)))
        (octal-parent-mode
         (apply #'derived-mode-p
                (mapcar #'car what-where/numbers-octal-prefix))))
    (when hexadecimal-parent-mode
      (let* ((contents
              (format "%s%x"
                      (cdr (assq hexadecimal-parent-mode
                                 what-where/numbers-hexadecimal-prefix))
                      matched-number))
             (features (what-where/features
                        provider:numers
                        type:dec-to-hex
                        actions:can-copy
                        actions:can-replace
                        ("numbers:hex:len:%d" (length contents) 1)))
             (copy-action (what-where/copy-action contents))
             (replace-action (what-where/replace-action contents
                                                        focus-start focus-end))
             (item (make-what-where-item :focus-start focus-start
                                         :focus-end focus-end
                                         :type "Dec->Hex"
                                         :contents contents
                                         :features features
                                         :actions (list copy-action
                                                        replace-action))))
        (what-where-add-item item)))
    (when octal-parent-mode
      (let* ((contents
              (format "%s%x"
                      (cdr (assq octal-parent-mode
                                 what-where/numbers-octal-prefix))
                      matched-number))
             (features (what-where/features
                        provider:numers
                        type:dec-to-oct
                        actions:can-copy
                        actions:can-replace
                        ("numbers:oct:len:%d" (length contents) 1)))
             (copy-action (what-where/copy-action contents))
             (replace-action (what-where/replace-action contents
                                                        focus-start focus-end))
             (item (make-what-where-item :focus-start focus-start
                                         :focus-end focus-end
                                         :type "Dec->Oct"
                                         :contents contents
                                         :features features
                                         :actions (list copy-action
                                                        replace-action))))
        (what-where-add-item item)))))

(provide 'what-where/numbers)

;;; what-where/numbers.el ends here

;;; Local Variables:
;;; coding: utf-8
;;; End:
