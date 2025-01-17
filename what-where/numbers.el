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
;; `what-where-numbers-provider' in `what-where-providers' (it is already part
;; of `what-where-default-providers').

;; This file is *NOT* part of GNU Emacs.

;;; Code:

(require 'cl-lib)

(defun what-where-looking-at-number ()
  "Returns a list (MATCHED-NUMBER FOCUS-START FOCUS-END) if the
current point is over a number, or NIL otherwise."
  (save-match-data
    (when (looking-at "[[:xdigit:]]+")
      (let ((matched-string (match-string-no-properties 0))
            (focus-start (point))
            (focus-end (match-end 0)))
        (when (looking-back "[[:xdigit:]]+" nil t)
          (setf focus-start (match-beginning 0))
          (setf matched-string (concat (match-string-no-properties 0)
                                       matched-string)))
        (list matched-string focus-start focus-end)))))

(defun what-where-numbers-provider ()
  "Number-related provider for `what-where'."
  (save-match-data
    (let ((match (what-where-looking-at-number)))
      (when match
        (let ((matched-string (cl-first match))
              (focus-start (cl-second match))
              (focus-end (cl-third match)))
          (what-where-numbers-generate-items matched-string
                                             focus-start focus-end))))))

(defun what-where-numbers-generate-items (matched-string focus-start focus-end)
  "Generate possible `what-where-items' for MATCHED-STRING, which starts at
FOCUS-START and ends at FOCUS-END."
  (when (string-match-p "^[01]+$" matched-string)
    (let ((matched-number (string-to-number matched-string 2)))
      (what-where-numbers-generate-base-items 2 matched-number
                                              focus-start focus-end)))
  (when (string-match-p "^[0-7]+$" matched-string)
    (let ((matched-number (string-to-number matched-string 8)))
      (what-where-numbers-generate-base-items 8 matched-number
                                              focus-start focus-end)))
  (when (string-match-p "^[[:digit:]]+$" matched-string)
    (let ((matched-number (string-to-number matched-string 10)))
      (what-where-numbers-generate-timestamp-items matched-number
                                                   focus-start focus-end)
      (what-where-numbers-generate-timestamp-items (/ matched-number 1000)
                                                   focus-start focus-end)
      (what-where-numbers-generate-timestamp-items (/ matched-number 1000000)
                                                   focus-start focus-end)
      (what-where-numbers-generate-roman-items matched-number
                                               focus-start focus-end)
      (what-where-numbers-generate-base-items 10 matched-number
                                              focus-start focus-end)))
  (let ((matched-number (string-to-number matched-string 16)))
    (what-where-numbers-generate-base-items 16 matched-number
                                            focus-start focus-end)))

(defcustom what-where-numbers-timezone "UTC0"
  "Timezone for the `what-where-numbers' provider."
  :type 'string
  :group 'what-where)

(defcustom what-where-numbers-time-format "%02d-%02d-%02d %02d:%02d:%02d"
  "Format string for timestamps for the `what-where-numbers' provider."
  :type 'string
  :group 'what-where)

(defcustom what-where-numbers-time-min-year 1980
  "Minimum year for the `what-where-numbers' provider."
  :type 'integer
  :group 'what-where)

(defcustom what-where-numbers-time-max-year 2100
  "Maximum year for the `what-where-numbers' provider."
  :type 'integer
  :group 'what-where)

(defun what-where-numbers-generate-timestamp-items (matched-number
                                                    focus-start focus-end)
  "Generate possible timestamp-related `what-where-items' for MATCHED-NUMBER,
which starts at FOCUS-START and ends at FOCUS-END."
  (let* ((matched-time (decode-time matched-number what-where-numbers-timezone))
         (year (nth 5 matched-time))
         (seconds (nth 0 matched-time))
         (minutes (nth 1 matched-time))
         (hour (nth 2 matched-time))
         (day (nth 3 matched-time))
         (month (nth 4 matched-time))
         (contents (format what-where-numbers-time-format
                           year month day hour minutes seconds))
         (features (what-where-features
                    provider:numers
                    type:timestamp
                    actions:can-copy
                    actions:can-replace
                    ("numbers:timestamp:log_year:%d"
                     (truncate (log (1+ (abs year)) 10)) 1)
                    ("numbers:timestamp:log_rel_year:%d"
                     (truncate (log (1+ (abs (- year 1970))) 10)) 1)
                    (numbers:timestamp:in-year-range
                     (*b* (<= what-where-numbers-time-min-year year
                              what-where-numbers-time-max-year)))))
         (copy-action (what-where-copy-action contents))
         (replace-action (what-where-replace-action contents
                                                    focus-start focus-end))
         (item (make-what-where-item :focus-start focus-start
                                     :focus-end focus-end
                                     :type "Timestamp"
                                     :contents contents
                                     :features features
                                     :actions (list copy-action
                                                    replace-action))))
        (what-where-add-item item)))

(defconst what-where-numbers-roman-values
  '((1000 . "M") (900 . "CM") (500 . "D") (400 . "CD")
    (100 . "C") (90 . "XC") (50 . "L") (40 . "XL")
    (10 . "X") (9 . "IX") (5 . "V") (4 . "IV") (1 . "I"))
  "Alist with (VALUE . DIGITS) roman number values.")

(defcustom what-where-numbers-roman-lowercase nil
  "Whether to lowercase roman numbers in the `what-where-numbers' provider."
  :type 'boolean
  :group 'what-where)

(defun what-where-numbers-generate-roman-items (matched-number
                                                focus-start focus-end)
  "Generate possible roman-number-related `what-where-items' for MATCHED-NUMBER,
which starts at FOCUS-START and ends at FOCUS-END."
  (when (< matched-number 4000)
    (let ((digits nil)
          (possible-values what-where-numbers-roman-values)
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
             (contents (if what-where-numbers-roman-lowercase
                           (downcase all-digits) all-digits))
             (features (what-where-features
                        provider:numers
                        type:roman
                        actions:can-copy
                        actions:can-replace
                        ("numbers:roman:log:%d"
                         (truncate (log (1+ (abs matched-number)) 10)) 1)))
             (copy-action (what-where-copy-action contents))
             (replace-action (what-where-replace-action contents
                                                        focus-start focus-end))
             (item (make-what-where-item :focus-start focus-start
                                         :focus-end focus-end
                                         :type "Roman"
                                         :contents contents
                                         :features features
                                         :actions (list copy-action
                                                        replace-action))))
        (what-where-add-item item)))))

(defconst what-where-numbers-base-names
  '((2 . "Bin") (8 . "Oct") (10 . "Dec") (16 . "Hex"))
  "Alist with (VALUE . NAME) for base names.")

(defconst what-where-numbers-base-formatters
  '((2 . what-where-numbers-format-binary)
    (8 . what-where-numbers-format-octal)
    (10 . what-where-numbers-format-decimal)
    (16 . what-where-numbers-format-hexadecimal))
  "Alist with (VALUE . FUNCTION) for number formatters by base.")

(defconst what-where-numbers-base-prefix-variable
  '((2 . what-where-numbers-binary-prefix)
    (8 . what-where-numbers-octal-prefix)
    (16 . what-where-numbers-hexadecimal-prefix))
  "Alist with (VALUE . VARIABLE) for customization variables of base prefixes.")

(defcustom what-where-numbers-binary-prefix
  '((c++-mode . "0b") (emacs-lisp-mode . "#b"))
  "Prefix to use when displaying hexadecimal numbers in each major mode."
  :type '(alist :key-type symbol :value-type string)
  :group 'what-where)

(defcustom what-where-numbers-octal-prefix
  '((c-mode . "0") (c++-mode . "0") (emacs-lisp-mode . "#o"))
  "Prefix to use when displaying octal numbers in each major mode."
  :type '(alist :key-type symbol :value-type string)
  :group 'what-where)

(defcustom what-where-numbers-hexadecimal-prefix
  '((c-mode . "0x") (c++-mode . "0x") (emacs-lisp-mode . "#x"))
  "Prefix to use when displaying hexadecimal numbers in each major mode."
  :type '(alist :key-type symbol :value-type string)
  :group 'what-where)

(defun what-where-numbers-format-binary (number)
  "Format NUMBER as a binary number."
  (do ((remaining number (lsh remaining -1))
       (digits nil (cons (if (zerop (logand remaining 1)) ?0 ?1) digits))
       (n-digits 0 (logand (1+ n-digits) 7)))
      ((and (zerop remaining) (zerop n-digits))
       (if (null digits) "0" (concat digits)))))

(defun what-where-numbers-format-octal (number)
  "Format NUMBER as an octal number."
  (format "%o" number))

(defun what-where-numbers-format-decimal (number)
  "Format NUMBER as a decimal number."
  (format "%d" number))

(defun what-where-numbers-format-hexadecimal (number)
  "Format NUMBER as a hexadecimal number."
  (format "%x" number))

(defun what-where-numbers-generate-base-items (src-base matched-number
                                               focus-start focus-end)
  "Generate possible base-conversion-related `what-where-items'
for MATCHED-NUMBER, which starts at FOCUS-START and ends at
FOCUS-END, and which was expressed in SRC-BASE."
  (let ((src-base-name (cdr (assq src-base what-where-numbers-base-names))))
    (dolist (tgt-base '(2 8 10 16))
      (unless (= src-base tgt-base)
        (let* ((tgt-base-name
                (cdr (assq tgt-base what-where-numbers-base-names)))
               (type-label (format "%s->%s" src-base-name tgt-base-name))
               (prefix-variable
                (cdr (assq tgt-base what-where-numbers-base-prefix-variable)))
               (prefix-parent-mode
                (apply #'derived-mode-p
                       (mapcar #'car (symbol-value prefix-variable))))
               (formatter
                (cdr (assq tgt-base what-where-numbers-base-formatters)))
               (contents (funcall formatter matched-number))
               (features (what-where-features
                          provider:numers
                          type:base
                          ("src-base:%d" src-base 1)
                          ("tgt-base:%d" tgt-base 1)
                          actions:can-copy
                          actions:can-replace))
               (copy-action (what-where-copy-action contents))
               (replace-action (what-where-replace-action contents
                                                          focus-start
                                                          focus-end))
               (item (make-what-where-item :focus-start focus-start
                                           :focus-end focus-end
                                           :type type-label
                                           :contents contents
                                           :features features
                                           :actions (list copy-action
                                                          replace-action))))
          (what-where-add-item item)
          (when prefix-parent-mode
            (let* ((prefix (cdr (assq prefix-parent-mode
                                      (symbol-value prefix-variable))))
                   (contents* (concat prefix contents))
                   (features* (cons '("has-prefix" . 1) features))
                   (type-label* (concat type-label "*"))
                   (copy-action* (what-where-copy-action contents*))
                   (replace-action* (what-where-replace-action contents*
                                                               focus-start
                                                               focus-end))
                   (item*
                    (make-what-where-item :focus-start focus-start
                                          :focus-end focus-end
                                          :type type-label*
                                          :contents contents*
                                          :features features*
                                          :actions (list copy-action*
                                                         replace-action*))))
              (what-where-add-item item*))))))))

(provide 'what-where/numbers)

;;; what-where/numbers.el ends here

;;; Local Variables:
;;; coding: utf-8
;;; End:
