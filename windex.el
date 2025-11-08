;;; windex.el --- Emacs Window Extension -*- lexical-binding: t -*-

;; Author: Alexander Chan
;; Maintainer: Alexander Chan
;; Version: 0.0.1
;; Package-Requires: (dependencies)


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;;; Code:

(require 'windex-utils)

(defgroup windex nil
  "Customization for `windex'."
  :group 'window
  :prefix "windex-")

(defcustom windex-window-filter-functions nil
  "Functions for filtering windows to be deleted in `windex-delete-windows'.
Window is passed as a function argument."
  :type '(repeat function)
  :group 'windex)

(defcustom windex-window-aw-filter-functions nil
  "Functions for filtering windows to be selected with `ace-window'.
Window is passed as a function argument."
  :type '(repeat function)
  :group 'windex)

;;;###autoload
(defun windex-delete-windows ()
  "Delete windows satisfying any of `windex-window-filter-functions'."
  (interactive)
  (windex-delete-windows-with-filter windex-window-filter-functions))

(with-eval-after-load 'ace-window
  (defun windex--check-aw-ignored-p (orig-func &rest args)
    "Function to limit windows that can acted on by `ace-window'.
Wraps ORIG-FUNC, `aw-ignored-p', with ARGS."
    ;; Ignore side-windows or popup-windows
    (let ((window (nth 0 args)))
      (if (not (seq-some (lambda (fn) (funcall fn window))
                         windex-window-aw-filter-functions))
          (apply orig-func args)
        t)))

  (defun windex--enable-ace-window ()
    "Enable windex `ace-window' features."
    (unless (featurep 'ace-window)
      (error "ace-window is required"))
    (require 'windex-windmove)

    (windex-windmove--enable-ace-window)
    (advice-add #'aw-ignored-p :around #'windex--check-aw-ignored-p))

  (defun windex--disable-ace-window ()
    "Disable winddex `ace-window' features."
    (windex-windmove--disable-ace-window)
    (advice-remove #'aw-ignored-p #'windex--check-aw-ignored-p))
  )

(defun windex--enable-windmove-in-direction-split ()
  "Modify `windmove' display in directions to be in split window."
  (require 'windex-windmove)
  (advice-add #'windmove-display-in-direction :around #'windex-windmove--display-in-direction-wrapper))

(defun windex--disable-windmove-in-direction-split ()
  "Modify `windmove' display in directions to be in split window."
  (advice-remove #'windmove-display-in-direction #'windex-windmove--display-in-direction-wrapper))

(provide 'windex)

;;; windex.el ends here
