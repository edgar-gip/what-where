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

(require 'cl-lib)
(require 'popup)

(defconst what-where-default-providers
  '(what-where/numbers-provider)
  "Default set of providers for `what-where'.")

(defgroup what-where ()
  "Customization options for `what-where-mode'.")

(defcustom what-where-providers what-where-default-providers
  "Set of providers for `what-where'."
  :type 'hook
  :options what-where-default-providers)

(defface what-where-focus-face
  '((t :background "green"))
  "Face to highlight the focus of the current `what-where' item."
  :group 'task-warrior)

(cl-defstruct what-where-action
  shortcut
  description
  function
  feedback
  is-terminal-p)

(cl-defstruct what-where-item
  focus-start
  focus-end
  type
  contents
  score
  actions)

(defvar what-where-items ()
  "Current set of items found by `what-where'.")

(defun what-where-clear-items ()
  "Clear the set of items in `what-where-items'."
  (setf what-where-items ()))

(defun what-where-add-item (item)
  "Add ITEM to `what-where-items'."
  (push item what-where-items))

(defvar what-where-source-buffer nil
  "Source buffer that `what-where' was called from.")

(defun what-where-set-source-buffer (buffer)
  "Set `what-where-source-buffer' to BUFFER."
  (setf what-where-source-buffer buffer))

(defvar what-where-focus-overlay nil
  "Overlay to highlight the focus of the current `what-where' item.")

(defun what-where-create-focus-overlay ()
  "Create an overlay for the focus of the current `what-where' item."
  (let ((overlay (make-overlay 0 0 what-where-source-buffer)))
    (overlay-put overlay 'face 'what-where-focus-face)
    overlay))

(defun what-where-delete-focus-overlay ()
  "Delete the overlay for the focus of the current `what-where' item."
  (when what-where-focus-overlay
    (delete-overlay what-where-focus-overlay)))

(defun what-where-move-focus-overlay ()
  "Move the overlay for the focus of the current `what-where' item."
  (when (buffer-live-p what-where-source-buffer)
    (unless what-where-focus-overlay
      (setf what-where-focus-overlay (what-where-create-focus-overlay)))
    (let ((id (tabulated-list-get-id)))
      (when id
        (let* ((item (nth id what-where-items))
               (focus-start (what-where-item-focus-start item))
               (focus-end (what-where-item-focus-end item)))
          (move-overlay what-where-focus-overlay
                        focus-start focus-end what-where-source-buffer))))))

(defmacro with-what-where-current-item (item-symbol else-clause &rest body)
  "Execute BODY with ITEM-SYMBOL bound to the currently selected item. If
unset, execute ELSE-CLAUSE."
  (declare (indent 2))
  (let ((id-symbol (gensym)))
    `(let ((,id-symbol (tabulated-list-get-id)))
       (if (null ,id-symbol)
           ,else-clause
         (let ((,item-symbol (nth ,id-symbol what-where-items)))
           ,@body)))))

(defmacro with-what-where-current-item* (item-symbol &rest body)
  "Execute BODY with ITEM-SYMBOL bound to the currently selected item, which is
assumed to exist."
  (declare (indent 1))
  (let ((id-symbol (gensym)))
    `(let ((,id-symbol (tabulated-list-get-id)))
       (cl-assert ,id-symbol)
       (let ((,item-symbol (nth ,id-symbol what-where-items)))
         ,@body))))

(defun what-where-report-refresh ()
  "Refresh the display in the `what-where-report-mode' window."
  (setq tabulated-list-entries nil)
  (let ((i 0))
    (dolist (item what-where-items)
      (push (list i (vector (what-where-item-type item)
                            (what-where-item-contents item)
                            (number-to-string (what-where-item-score item))))
            tabulated-list-entries)
      (incf i))))

(defun what-where-execute-action (action)
  "Execute the ACTION."
  (let ((callback (what-where-action-function action))
        (feedback (what-where-action-feedback action))
        (is-terminal-p (what-where-action-is-terminal-p action)))
    (cl-assert callback)
    (funcall callback)
    (when feedback
      (message feedback))
    (when is-terminal-p
      (quit-window))))

(defun what-where-report-shortcut ()
  "Execute the action denoted by the last pressed key on the current item."
  (interactive)
  (with-what-where-current-item item
      (message "No current item")
    (let* ((key last-command-event)
           (actions (what-where-item-actions item))
           (action (cl-find-if #'(lambda (action)
                                   (eq (what-where-action-shortcut action) key))
                               actions)))
      (if (null action)
          (message "Action '%c' not defined in current item" key)
        (what-where-execute-action action)))))

(defvar what-where-popup-menu nil
  "Currently displayed popup menu on the report screen, if any.")

(defvar what-where-popup-menu-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map popup-menu-keymap)
    (define-key map (kbd "<return>") 'popup-select)
    (define-key map (kbd "C-g") 'what-where-popup-abort)
    (define-key map (kbd "q") 'what-where-popup-abort)
    map)
  "Keymap for the popup menu on the `what-where-report-mode' buffer.")

(defun what-where-popup-abort ()
  "Abort the selection from the popup menu in `what-where-report-mode'."
  (interactive)
  (popup-delete what-where-popup-menu))

(defun what-where-popup-shortcut (keyseq binding)
  "Process KEYSEQ as a potential shortcut from the popup menu in
`what-where-report-mode'."
  (when (= (length keyseq) 1)
    (with-what-where-current-item* item
      (let* ((key (string-to-char keyseq))
             (actions (what-where-item-actions item))
             (action (cl-find-if
                      #'(lambda (action)
                          (eq (what-where-action-shortcut action) key))
                      actions)))
        (if (null action)
            (message "Action '%c' not defined in current item" key)
          (what-where-execute-action action)
          (popup-delete what-where-popup-menu))))))

(defun what-where-report-popup ()
  "Pop up a menu with the actions on the current item in
`what-where-report-mode'."
  (interactive)
  (with-what-where-current-item item
      (message "No current item")
    (let* ((actions (what-where-item-actions item))
           (descriptions (mapcar #'what-where-action-description actions)))
      (if (null actions)
          (message "No actions defined on current item")
        (setf what-where-popup-menu
              (popup-menu* descriptions
                           :point (+ (line-beginning-position) 10)
                           :keymap what-where-popup-menu-keymap
                           :nowait t))
        (let* ((selected-description
                (unwind-protect
                    (popup-menu-event-loop what-where-popup-menu
                                           what-where-popup-menu-keymap
                                           #'what-where-popup-shortcut)
                  (progn
                    (popup-delete what-where-popup-menu)
                    (setf what-where-popup-menu nil))))
               (selected-action
                (and selected-description
                     (cl-find-if #'(lambda (action)
                                     (eq (what-where-action-description action)
                                         selected-description))
                                 actions))))
          (when selected-action
            (what-where-execute-action selected-action)))))))

(defvar what-where-report-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "<return>") 'what-where-report-popup)
    (dotimes (i (1+ (- ?z ?a)))
      (let* ((key (+ ?a i))
             (sequence (string key)))
        (unless (lookup-key map sequence)
          (define-key map sequence 'what-where-report-shortcut))))
    map)
  "Local keymap for `what-where-report-mode'.")

(define-derived-mode what-where-report-mode
  tabulated-list-mode "what-where-report"
  "Major mode for the results of a `what-where' query."
  :group 'what-where
  :syntax-table nil
  :abbrev-table nil
  (setq tabulated-list-format [("Type" 10 t)
                               ("Contents" 50 nil)
                               ("Score" 10 t)])
  (setq tabulated-list-sort-key (cons "Score" t))
  (tabulated-list-init-header)
  (hl-line-mode)
  (add-hook 'pre-command-hook #'what-where-delete-focus-overlay nil t)
  (add-hook 'post-command-hook #'what-where-move-focus-overlay nil t)
  (add-hook 'change-major-mode-hook #'what-where-delete-focus-overlay nil t))

(defun what-where (nofocus)
  "Display what you look at and where you are."
  (interactive "P")
  (what-where-clear-items)
  (what-where-set-source-buffer (current-buffer))
  (run-hooks 'what-where-providers)
  (let ((report-buffer (get-buffer-create "*What/Where*")))
    (with-current-buffer report-buffer
      (what-where-report-mode)
      (what-where-report-refresh)
      (tabulated-list-print nil)
      (if nofocus
          (display-buffer report-buffer)
        (pop-to-buffer report-buffer)
        (hl-line-highlight))
      (goto-char (point-min))
      (what-where-move-focus-overlay))))

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

(require 'what-where/numbers)

(provide 'what-where)

;;; what-where.el ends here

;;; Local Variables:
;;; coding: utf-8
;;; End:
