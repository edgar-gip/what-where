;;; what-where/report.el --- Report window for `what-where'.
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

(defcustom what-where/report-show-negative-by-default nil
  "Whether to show items with negative-scores by default in
`what-where/report'."
  :type 'boolean
  :group 'what-where)

(defface what-where/report-focus-face
  '((t :background "green"))
  "Face to highlight the focus of the current `what-where/report' item."
  :group 'what-where)

(defface what-where/report-filtered-item-face
  '((t :foreground "dark gray"))
  "Face to highlight the focus of the current `what-where/report' item."
  :group 'what-where)

(defmacro with-what-where/report-current-item (item-symbol ellipsis-clause
                                               nil-clause &rest body)
  "Execute BODY with ITEM-SYMBOL bound to the currently selected item. If
unset, execute ELLIPSIS-CLAUSE or NIL-CLAUSE."
  (declare (indent 3))
  (let ((id-symbol (gensym)))
    `(let ((,id-symbol (tabulated-list-get-id)))
       (cl-case ,id-symbol
        ((nil)
         ,nil-clause)
        ((ellipsis)
         ,ellipsis-clause)
        (otherwise
         (let ((,item-symbol (nth ,id-symbol what-where-items)))
           ,@body))))))

(defmacro with-what-where/report-current-item* (item-symbol &rest body)
  "Execute BODY with ITEM-SYMBOL bound to the currently selected item, which is
assumed to exist."
  (declare (indent 1))
  (let ((id-symbol (gensym)))
    `(let ((,id-symbol (tabulated-list-get-id)))
       (cl-assert (and ,id-symbol (not (eq ,id-symbol 'ellipsis))))
       (let ((,item-symbol (nth ,id-symbol what-where-items)))
         ,@body))))

(defvar what-where/report-focus-overlay nil
  "Overlay to highlight the focus of the current `what-where/report' item.")

(defun what-where/report-create-focus-overlay ()
  "Create an overlay for the focus of the current `what-where/report' item."
  (let ((overlay (make-overlay 0 0 what-where-source-buffer)))
    (overlay-put overlay 'face 'what-where/report-focus-face)
    overlay))

(defun what-where/report-delete-focus-overlay ()
  "Delete the overlay for the focus of the current `what-where/report' item."
  (when what-where/report-focus-overlay
    (delete-overlay what-where/report-focus-overlay)))

(defun what-where/report-move-focus-overlay ()
  "Move the overlay for the focus of the current `what-where/report' item."
  (when (buffer-live-p what-where-source-buffer)
    (unless what-where/report-focus-overlay
      (setf what-where/report-focus-overlay
            (what-where/report-create-focus-overlay)))
    (with-what-where/report-current-item item
        nil nil
        (let ((focus-start (what-where-item-focus-start item))
              (focus-end (what-where-item-focus-end item)))
          (move-overlay what-where/report-focus-overlay
                        focus-start focus-end what-where-source-buffer)))))

(defvar what-where/report-show-negative nil
  "Whether to show items with negative-scores in `what-where/report'.")

(defun what-where/report-refresh ()
  "Refresh the display in the `what-where/report' window."
  (setq tabulated-list-entries nil)
  (let ((threshold
         (and (not what-where/report-show-negative)
              what-where-items
              (if (> (what-where-item-score (cl-first what-where-items)) 0)
                  0
                (what-where-item-score (cl-first what-where-items)))))
        (i 0)
        (remaining-items what-where-items))
    (while (and remaining-items
                (or (null threshold)
                    (>= (what-where-item-score (cl-first remaining-items))
                        threshold)))
      (let* ((item (cl-first remaining-items))
             (fmt (if (< (what-where-item-score item) 0)
                      #'(lambda (s)
                          (propertize s 'font-lock-face
                                      'what-where/report-filtered-item-face))
                    #'identity)))
        (push (list i (vector (funcall fmt (what-where-item-type item))
                              (funcall fmt (what-where-item-contents item))
                              (funcall fmt
                                       (number-to-string
                                        (what-where-item-score item)))))
              tabulated-list-entries)
        (incf i)
        (setf remaining-items (cl-rest remaining-items))))
    (when remaining-items
      (push (list 'ellipsis
                  (vector ""
                          (propertize "..." 'font-lock-face
                                      'what-where/report-filtered-item-face)
                          ""))
            tabulated-list-entries)))
  (setf tabulated-list-entries (nreverse tabulated-list-entries)))

(defun what-where/report-toggle-show-negative ()
  "Toggle the value of `what-where/report-show-negative' and refresh the
buffer."
  (interactive)
  (setf what-where/report-show-negative
        (not what-where/report-show-negative))
  (let ((current-row (1- (line-number-at-pos))))
    (what-where/report-refresh)
    (tabulated-list-print nil)
    (goto-char (point-min))
    (forward-line current-row)))

(defun what-where/report-execute-action (action item)
  "Execute the ACTION (which comes from ITEM)."
  (let ((callback (what-where-action-function action))
        (feedback (what-where-action-feedback action))
        (is-terminal-p (what-where-action-is-terminal-p action)))
    (cl-assert callback)
    (funcall callback)
    (when feedback
      (message feedback))
    (what-where-select-item item)
    (when is-terminal-p
      (quit-window))))

(defun what-where/report-shortcut ()
  "Execute the action denoted by the last pressed key on the current item."
  (interactive)
  (with-what-where/report-current-item item
      (message "No current item")
      (message "No current item")
    (let* ((key last-command-event)
           (actions (what-where-item-actions item))
           (action (cl-find-if #'(lambda (action)
                                   (eq (what-where-action-shortcut action) key))
                               actions)))
      (if (null action)
          (message "Action '%c' not defined in current item" key)
        (what-where/report-execute-action action item)))))

(defvar what-where/report-popup-menu nil
  "Currently displayed popup menu on the report screen, if any.")

(defvar what-where/report-popup-menu-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map popup-menu-keymap)
    (define-key map (kbd "<return>") 'popup-select)
    (define-key map (kbd "C-g") 'what-where/report-popup-abort)
    (define-key map (kbd "q") 'what-where/report-popup-abort)
    map)
  "Keymap for the popup menu on the `what-where/report-mode' buffer.")

(defun what-where/report-popup-abort ()
  "Abort the selection from the popup menu in `what-where/report-mode'."
  (interactive)
  (popup-delete what-where/report-popup-menu))

(defun what-where/report-popup-shortcut (keyseq binding)
  "Process KEYSEQ as a potential shortcut from the popup menu in
`what-where/report-mode'."
  (when (= (length keyseq) 1)
    (with-what-where/report-current-item* item
      (let* ((key (string-to-char keyseq))
             (actions (what-where-item-actions item))
             (action (cl-find-if
                      #'(lambda (action)
                          (eq (what-where-action-shortcut action) key))
                      actions)))
        (if (null action)
            (message "Action '%c' not defined in current item" key)
          (what-where/report-execute-action action item)
          (popup-delete what-where/report-popup-menu))))))

(defun what-where/report-popup ()
  "Pop up a menu with the actions on the current item in
`what-where/report-mode'."
  (interactive)
  (with-what-where/report-current-item item
      (what-where/report-toggle-show-negative)
      (message "No current item")
    (let* ((actions (what-where-item-actions item))
           (descriptions (mapcar #'what-where-action-description actions)))
      (if (null actions)
          (message "No actions defined on current item")
        (setf what-where/report-popup-menu
              (popup-menu* descriptions
                           :point (+ (line-beginning-position) 10)
                           :keymap what-where/report-popup-menu-keymap
                           :nowait t))
        (let* ((selected-description
                (unwind-protect
                    (popup-menu-event-loop what-where/report-popup-menu
                                           what-where/report-popup-menu-keymap
                                           #'what-where/report-popup-shortcut)
                  (progn
                    (popup-delete what-where/report-popup-menu)
                    (setf what-where/report-popup-menu nil))))
               (selected-action
                (and selected-description
                     (cl-find-if #'(lambda (action)
                                     (eq (what-where-action-description action)
                                         selected-description))
                                 actions))))
          (when selected-action
            (what-where/report-execute-action selected-action item)))))))

(defvar what-where/report-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "<return>") 'what-where/report-popup)
    (define-key map (kbd "<tab>") 'what-where/report-toggle-show-negative)
    (dotimes (i (1+ (- ?z ?a)))
      (let* ((key (+ ?a i))
             (sequence (string key)))
        (unless (lookup-key map sequence)
          (define-key map sequence 'what-where/report-shortcut))))
    map)
  "Local keymap for `what-where/report-mode'.")

(define-derived-mode what-where/report-mode
  tabulated-list-mode "What/Where"
  "Major mode for the results of a `what-where' query."
  :group 'what-where
  :syntax-table nil
  :abbrev-table nil
  (setq tabulated-list-format [("Type" 10 nil)
                               ("Contents" 50 nil)
                               ("Score" 10 nil)])
  (tabulated-list-init-header)
  (hl-line-mode)
  (add-hook 'pre-command-hook #'what-where/report-delete-focus-overlay nil t)
  (add-hook 'post-command-hook #'what-where/report-move-focus-overlay nil t)
  (add-hook 'change-major-mode-hook
            #'what-where/report-delete-focus-overlay nil t))

(defun what-where/report-render-items (nofocus)
  "Render the current set of `what-where-items' in a `what-where/report'."
  (let ((report-buffer (get-buffer-create "*What/Where*")))
    (with-current-buffer report-buffer
      (what-where/report-mode)
      (setf what-where/report-show-negative
            what-where/report-show-negative-by-default)
      (what-where/report-refresh)
      (tabulated-list-print nil)
      (if nofocus
          (display-buffer report-buffer)
        (pop-to-buffer report-buffer)
        (hl-line-highlight))
      (goto-char (point-min))
      (what-where/report-move-focus-overlay))))

(provide 'what-where/report)

;;; what-where/report.el ends here

;;; Local Variables:
;;; coding: utf-8
;;; End:
