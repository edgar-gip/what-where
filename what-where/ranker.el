;;; what-where/ranker.el --- Ranking model for `what-where'.
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
;; This file is `require'd by what-where.el, and should not need to be loaded
;; explicitly.

;; This file is *NOT* part of GNU Emacs.

;;; Code:

(require 'cl-lib)

(defcustom what-where/ranker-margin 1.0
  "Minimum margin for the `what-where/ranker' model."
  :type 'float
  :group 'what-where)

(defcustom what-where/ranker-learning-rate 1.0
  "Learning rate for the `what-where/ranker' model."
  :type 'float
  :group 'what-where)

(cl-defstruct what-where/ranker-model
  epochs
  current-weights
  average-weights)

(defun what-where/ranker-create-model ()
  "Create an empty `what-where/ranker-model'."
  (make-what-where/ranker-model :epochs 0
                                :current-weights (make-hash-table :test 'eq)
                                :average-weights (make-hash-table :test 'eq)))

(defvar what-where/ranker-model (what-where/ranker-create-model)
  "Model used by the `what-where/ranker'.")

(defun what-where/ranker-score-item (item &optional use-current-p)
  "Score ITEM with `what-where/ranker-model'. If USE-CURRENT-P, use the
current weights instead of the average ones."
  (let ((sum 0)
        (epochs (what-where/ranker-model-epochs what-where/ranker-model))
        (weights (if use-current-p
                     (what-where/ranker-model-current-weights
                      what-where/ranker-model)
                   (what-where/ranker-model-average-weights
                    what-where/ranker-model))))
    (unless (zerop epochs)
      (dolist (pair (what-where-item-features item))
        (let ((feature (car pair))
              (value (cdr pair)))
          (incf sum (* value (gethash feature weights 0.0)))))
      (unless use-current-p
        (setf sum (/ sum epochs))))
    sum))

(defun what-where/ranker-score-items ()
  "Score the `what-where-items' with `what-where/ranker-model', using the
average weights."
  (dolist (item what-where-items)
    (setf (what-where-item-score item) (what-where/ranker-score-item item))))

(defun what-where/ranker-update* (positive-features negative-features)
  "Update the `what-where/ranker-model' given the best
POSITIVE-FEATURES and NEGATIVE-FEATURES."
  (let ((current-weights
         (what-where/ranker-model-current-weights what-where/ranker-model)))
    ;; Update the weights following the gradient.
    (dolist (pair positive-features)
      (let* ((feature (car pair))
             (value (cdr pair))
             (new-weight (+ (gethash feature current-weights 0.0)
                            (* what-where/ranker-learning-rate value))))
        (if (zerop new-weight)
            (remhash feature current-weights)
          (puthash feature new-weight current-weights))))
    (dolist (pair negative-features)
      (let* ((feature (car pair))
             (value (cdr pair))
             (new-weight (- (gethash feature current-weights 0.0)
                            (* what-where/ranker-learning-rate value))))
        (if (zerop new-weight)
            (remhash feature current-weights)
          (puthash feature new-weight current-weights))))))

(defun what-where/ranker-update ()
  "Update the MODEL given that, out of `what-where-items',
`what-where-selected-item' was chosen."
  ;; Find the score of the selected item (if any), and of the highest scored
  ;; non-selected item (if any).
  (let ((selected-item-score
         (and what-where-selected-item
              (what-where/ranker-score-item what-where-selected-item t)))
        (best-negative-item nil)
        (best-negative-item-score nil))
    (dolist (item what-where-items)
      (unless (eq item what-where-selected-item)
        (let ((score (what-where/ranker-score-item item t)))
          (when (or (null best-negative-item-score)
                    (> score best-negative-item-score))
            (setf best-negative-item item)
            (setf best-negative-item-score score)))))
    ;; Update the model (current weights) following the subgradient.
    (cond
     ((and what-where-selected-item best-negative-item)
      (cond
       ((< selected-item-score
           (+ best-negative-item-score what-where/ranker-margin))
        (what-where/ranker-update*
         (what-where-item-features what-where-selected-item)
         (what-where-item-features best-negative-item)))
       ((< selected-item-score 0)
        (what-where/ranker-update*
         (what-where-item-features what-where-selected-item) ()))))
     (what-where-selected-item
      (when (< selected-item-score 0)
        (what-where/ranker-update*
         (what-where-item-features what-where-selected-item) ())))
     (best-negative-item
      (when (> best-negative-item-score (- what-where/ranker-margin))
        (what-where/ranker-update*
         () (what-where-item-features best-negative-item)))))
    ;; Add the current weights to the average ones, and count one more epoch.
    (let ((current-weights (what-where/ranker-model-current-weights
                            what-where/ranker-model))
          (average-weights (what-where/ranker-model-average-weights
                            what-where/ranker-model)))
      (maphash #'(lambda (feature weight)
                   (let ((new-weight (+ (gethash feature average-weights 0.0)
                                        weight)))
                     (if (zerop new-weight)
                         (remhash feature average-weights)
                       (puthash feature new-weight average-weights))))
               current-weights))
    (incf (what-where/ranker-model-epochs what-where/ranker-model))))

(provide 'what-where/ranker)

;;; what-where/ranker.el ends here

;;; Local Variables:
;;; coding: utf-8
;;; End:
