;;; what-where/ranker.el --- Ranking model for `what-where'.
;;
;; Copyright (C) 2017  Edgar Gonzàlez i Pellicer
;;
;; Author: Edgar Gonzàlez i Pellicer <edgar.gip@gmail.com>
;; Keywords: what, where, ranker
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
(require 'pp)

(defcustom what-where-ranker-learn t
  "Whether the `what-where-ranker' model should be learnt."
  :type 'boolean
  :group 'what-where)

(defcustom what-where-ranker-margin 1.0
  "Minimum margin for the `what-where-ranker' model."
  :type 'float
  :group 'what-where)

(defcustom what-where-ranker-learning-rate 1.0
  "Learning rate for the `what-where-ranker' model."
  :type 'float
  :group 'what-where)

(defcustom what-where-ranker-update-when-none-selected nil
  "Whether the `what-where-ranker' model needs to be updated when no selection
was made for a particular focus."
  :type 'boolean
  :group 'what-where)

(defcustom what-where-ranker-model-file
  (expand-file-name (concat (if (boundp 'user-emacs-directory)
                                user-emacs-directory
                              "~/.emacs.d/")
                            "/what-where-ranker.dat"))
  "File to store the `what-where-ranker' model."
  :type 'string
  :group 'what-where)

(defcustom what-where-ranker-epochs-to-save-model 10
  "Number of training epochs between saves of the `what-where-ranker' model."
  :type '(choice (const :tag "None" nil)
                 (integer :tag "Epochs"))
  :group 'what-where)

(cl-defstruct what-where-ranker-model
  epochs
  current-weights
  average-weights)

(defvar what-where-ranker-model nil
  "Model used by the `what-where-ranker'.")

(defvar what-where-ranker-last-model-save-epochs nil
  "Number of training epochs of `what-where-ranker-model' last time it was
saved.")

(defun what-where-ranker-save-model ()
  "Save `what-where-ranker-model' to `what-where-ranker-model-file'."
  (with-demoted-errors
      (concat "Error serializing `what-where-ranker-model' to "
              what-where-ranker-model-file ": %s")
    (let ((epochs (what-where-ranker-model-epochs what-where-ranker-model))
          (current-weights nil)
          (average-weights nil))
      (maphash #'(lambda (feature weight)
                   (push (cons feature weight) current-weights))
               (what-where-ranker-model-current-weights
                what-where-ranker-model))
      (maphash #'(lambda (feature weight)
                   (push (cons feature weight) average-weights))
               (what-where-ranker-model-average-weights
                what-where-ranker-model))
      (let ((serialization (list epochs current-weights average-weights)))
        (with-temp-buffer
          (pp serialization (current-buffer))
          (write-region (point-min) (point-max) what-where-ranker-model-file)))
      (setf what-where-ranker-last-model-save-epochs epochs))
    (message "Saved `what-where-ranker-model' to %s"
             what-where-ranker-model-file)))

(defun what-where-ranker-shutdown-model ()
  "Shut down the `what-where-ranker-model'."
  (when (and what-where-ranker-model
             (> (what-where-ranker-model-epochs what-where-ranker-model)
                what-where-ranker-last-model-save-epochs))
    (what-where-ranker-update)
    (what-where-ranker-save-model)))

(defun what-where-ranker-load-model ()
  "Load `what-where-ranker-model' from `what-where-ranker-model-file'."
  (when (file-exists-p what-where-ranker-model-file)
    (with-demoted-errors
        (concat "Error deserializing `what-where-ranker-model' from "
                what-where-ranker-model-file ": %s")
      (with-temp-buffer
        (insert-file-contents what-where-ranker-model-file)
        (goto-char (point-min))
        (let ((serialization (read (current-buffer))))
          (cl-assert (listp serialization) nil
                     "File contents must be a list")
          (cl-assert (= (length serialization) 3) nil
                     "File contents must be a three-element list")
          (cl-assert (integerp (cl-first serialization)) nil
                     "First element of file contents must be an integer")
          (cl-assert (listp (cl-second serialization)) nil
                     "Second element of file contents must be a list")
          (cl-assert (cl-every #'(lambda (x)
                                   (and (consp x) (symbolp (car x))
                                        (floatp (cdr x))))
                               (cl-second serialization) nil)
                     (concat "Each element in second element of file contents "
                             "must be a (symbol . float) cons cell"))
          (cl-assert (listp (cl-third serialization)) nil
                     "Third element of file contents must be a list")
          (cl-assert (cl-every #'(lambda (x)
                                   (and (consp x) (symbolp (car x))
                                        (floatp (cdr x))))
                               (cl-third serialization) nil)
                     (concat "Each element in third element of file contents "
                             "must be a (symbol . float) cons cell"))
          (let ((epochs (cl-first serialization))
                (current-weights (make-hash-table :test 'eq))
                (average-weights (make-hash-table :test 'eq)))
            (dolist (pair (cl-second serialization))
              (puthash (car pair) (cdr pair) current-weights))
            (dolist (pair (cl-third serialization))
              (puthash (car pair) (cdr pair) average-weights))
            (setf what-where-ranker-model
                  (make-what-where-ranker-model
                   :epochs epochs
                   :current-weights current-weights
                   :average-weights average-weights))
            (setf what-where-ranker-last-model-save-epochs epochs)
            (message "Loaded `what-where-ranker-model' from %s"
                     what-where-ranker-model-file)
            t))))))

(defun what-where-ranker-create-model ()
  "Create an empty `what-where-ranker-model'."
  (setf what-where-ranker-model
        (make-what-where-ranker-model
         :epochs 0
         :current-weights (make-hash-table :test 'eq)
         :average-weights (make-hash-table :test 'eq)))
  (setf what-where-ranker-last-model-save-epochs 0)
  (message "Created new `what-where-ranker-model'"))

(defun what-where-ranker-ensure-model ()
  "Ensures `what-where-ranker-model' exists."
  (unless what-where-ranker-model
    (or (what-where-ranker-load-model)
        (what-where-ranker-create-model))
    (add-hook 'kill-emacs-hook 'what-where-ranker-shutdown-model)))

(defun what-where-ranker-score-item (item &optional use-current-p)
  "Score ITEM with `what-where-ranker-model'. If USE-CURRENT-P, use the
current weights instead of the average ones."
  (let ((sum 0)
        (epochs (what-where-ranker-model-epochs what-where-ranker-model))
        (weights (if use-current-p
                     (what-where-ranker-model-current-weights
                      what-where-ranker-model)
                   (what-where-ranker-model-average-weights
                    what-where-ranker-model))))
    (unless (zerop epochs)
      (dolist (pair (what-where-item-features item))
        (let ((feature (car pair))
              (value (cdr pair)))
          (cl-incf sum (* value (gethash feature weights 0.0)))))
      (unless use-current-p
        (setf sum (/ sum epochs))))
    sum))

(defun what-where-ranker-score-items ()
  "Score the `what-where-items' with `what-where-ranker-model', using the
average weights."
  (what-where-ranker-ensure-model)
  (dolist (item what-where-items)
    (setf (what-where-item-score item) (what-where-ranker-score-item item))))

(defun what-where-ranker-update* (positive-features negative-features)
  "Update the `what-where-ranker-model' given the best
POSITIVE-FEATURES and NEGATIVE-FEATURES."
  (let ((current-weights
         (what-where-ranker-model-current-weights what-where-ranker-model)))
    ;; Update the weights following the gradient.
    (dolist (pair positive-features)
      (let* ((feature (car pair))
             (value (cdr pair))
             (new-weight (+ (gethash feature current-weights 0.0)
                            (* what-where-ranker-learning-rate value))))
        (if (zerop new-weight)
            (remhash feature current-weights)
          (puthash feature new-weight current-weights))))
    (dolist (pair negative-features)
      (let* ((feature (car pair))
             (value (cdr pair))
             (new-weight (- (gethash feature current-weights 0.0)
                            (* what-where-ranker-learning-rate value))))
        (if (zerop new-weight)
            (remhash feature current-weights)
          (puthash feature new-weight current-weights))))))

(defun what-where-ranker-update ()
  "Update the MODEL given that, out of `what-where-items',
`what-where-selected-item' was chosen."
  (what-where-ranker-ensure-model)
  (when (and what-where-ranker-learn
             (or what-where-ranker-update-when-none-selected
                 what-where-selected-item))
    ;; Find the score of the selected item (if any), and of the highest scored
    ;; non-selected item (if any).
    (let ((selected-item-score
           (and what-where-selected-item
                (what-where-ranker-score-item what-where-selected-item t)))
          (best-negative-item nil)
          (best-negative-item-score nil))
      (dolist (item what-where-items)
        (unless (eq item what-where-selected-item)
          (let ((score (what-where-ranker-score-item item t)))
            (when (or (null best-negative-item-score)
                      (> score best-negative-item-score))
              (setf best-negative-item item)
              (setf best-negative-item-score score)))))
      ;; Update the model (current weights) following the subgradient.
      (cond
       ((and what-where-selected-item best-negative-item)
        (cond
         ((< selected-item-score
             (+ best-negative-item-score what-where-ranker-margin))
          (what-where-ranker-update*
           (what-where-item-features what-where-selected-item)
           (what-where-item-features best-negative-item)))
         ((< selected-item-score 0)
          (what-where-ranker-update*
           (what-where-item-features what-where-selected-item) ()))))
       (what-where-selected-item
        (when (< selected-item-score 0)
          (what-where-ranker-update*
           (what-where-item-features what-where-selected-item) ())))
       (best-negative-item
        (when (> best-negative-item-score (- what-where-ranker-margin))
          (what-where-ranker-update*
           () (what-where-item-features best-negative-item)))))
      ;; Add the current weights to the average ones, and count one more epoch.
      (let ((current-weights (what-where-ranker-model-current-weights
                              what-where-ranker-model))
            (average-weights (what-where-ranker-model-average-weights
                              what-where-ranker-model)))
        (maphash #'(lambda (feature weight)
                     (let ((new-weight (+ (gethash feature average-weights 0.0)
                                          weight)))
                       (if (zerop new-weight)
                           (remhash feature average-weights)
                         (puthash feature new-weight average-weights))))
                 current-weights))
      (incf (what-where-ranker-model-epochs what-where-ranker-model))
      ;; Check if serialization is needed.
      (when (and what-where-ranker-epochs-to-save-model
                 (>= (- (what-where-ranker-model-epochs what-where-ranker-model)
                        what-where-ranker-last-model-save-epochs)
                     what-where-ranker-epochs-to-save-model))
        (what-where-ranker-save-model)))))

(provide 'what-where/ranker)

;;; what-where/ranker.el ends here

;;; Local Variables:
;;; coding: utf-8
;;; End:
