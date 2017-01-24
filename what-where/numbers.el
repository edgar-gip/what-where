;;; what-where/numbers.el --- Number-related provider for `what-where'.
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
;; This file is `require'd by what-where.el. To enable the provider, include
;; `what-where/numbers-provider' in `what-where-providers' (it is already part
;; of `what-where-default-providers').

;; This file is *NOT* part of GNU Emacs.

;;; Code:

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

(defun what-where/numbers-provider ()
  "Number-related provider for `what-where'."
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
                                               focus-start focus-end))

(defun what-where/numbers-generate-timestamp-items (matched-number
                                                    focus-start focus-end)
  "Generate possible timestamp-related `what-where-items' for MATCHED-NUMBER,
which starts at FOCUS-START and ends at FOCUS-END."
  (let* ((matched-time (decode-time matched-number what-where/numbers-timezone))
         (year (nth 5 matched-time)))
    (when (and (<= what-where/numbers-time-min-year year
                   what-where/numbers-time-max-year))
      (let* ((seconds (nth 0 matched-time))
             (minutes (nth 1 matched-time))
             (hour (nth 2 matched-time))
             (day (nth 3 matched-time))
             (month (nth 4 matched-time))
             (contents (format what-where/numbers-time-format
                               year month day hour minutes seconds))
             (action `(lambda ()
                        (kill-new ,contents)
                        (message "Copied '%s' to kill ring." ,contents)))
             (item (make-what-where-item :focus-start focus-start
                                         :focus-end focus-end
                                         :type "Timestamp"
                                         :contents contents
                                         :score 1.0
                                         :action action)))
        (what-where-add-item item)))))

(provide 'what-where/numbers)

;;; what-where.el ends here

;;; Local Variables:
;;; coding: utf-8
;;; End:
