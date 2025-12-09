;;; windex-posframe.el --- Windex Posframe -*- lexical-binding: t -*-

;; Author: Alexander Chan
;; Maintainer: Alexander Chan
;; Version: 0.0.1
;; Package-Requires: ((emacs "30.0) (posframe "1.5.0"))


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
(require 'posframe)

(defgroup windex-posframe nil
  "Customization for `windex' posframe."
  :group 'windex
  :prefix "windex-posframe-")

(defvar windex-posframe--buffer nil)
(defvar windex-posframe--frame nil)

(defcustom windex-posframe-parameters '()
  "Them frame parameters for `windex-posframe'."
  :type '(alist :key-type symbol :value-type sexp))

(defcustom windex-posframe-poshandler #'posframe-poshandler-frame-center
  "The posframe poshandler used by vertico-posframe."
  :type 'function)

(defcustom windex-posframe-min-width 180
  "Minimum `windex-posframe' width in characters."
  :type 'integer)

(defcustom windex-posframe-min-height 60
  "Minimum `windex-posframe' height in lines."
  :type 'integer)

(defcustom windex-posframe-dimension-function #'windex-posframe-get-dimensions
  "Function to calculate the dimension for `windex-posframe'."
  :type 'function)

(defcustom windex-posframe-border-width 1
  "`windex-posframe' border width."
  :type 'integer)

(defface windex-posframe-border
  '((t (:foreground "gray50" :background "gray50")))
  "Face used by the `windex-posframe' border."
  :group 'windex-posframe)

(defun windex-posframe-get-dimensions (buffer)
  "The default dimensions function used by `windex-posframe'."
  (list
   :max-width (min windex-posframe-min-width
               (ceiling (* (frame-width) (/ 2560.0 (display-pixel-width)))))
   :max-height (min windex-posframe-min-height
                (ceiling (* (frame-height) (/ 1440.0 (display-pixel-height)))))
   :min-width (max windex-posframe-min-width
               (ceiling (* (frame-width) (/ 1920.0 (display-pixel-width)))))
   :min-height (max windex-posframe-min-height
                (ceiling (* (frame-height) (/ 1080.0 (display-pixel-height)))))
   ))

(defun windex-posframe--show (buffer &optional alist)
  "Display BUFFER in `posframe' with optional ALIST."

  (setq windex-posframe--buffer buffer
        windex-posframe--frame
        (apply #'posframe-show
               buffer
               :cursor 'box
               :accept-focus t
               :poshandler windex-posframe-poshandler
               :hidehandler nil
               :border-width windex-posframe-border-width
               :border-color (face-attribute 'windex-posframe-border :background nil t)
               :respect-header-line nil
               :respect-mode-line t
               :respect-tab-line nil
               :lines-truncate t
               :override-parameters windex-posframe-parameters
               (funcall windex-posframe-dimension-function buffer)
               ))

  (select-frame-set-input-focus windex-posframe--frame t)
  (let ((window (select-window (window-main-window windex-posframe--frame))))
    (window--display-buffer buffer window 'reuse alist))
  )

(defun windex-posframe--hide ()
  "Hide the `windex-posframe' frame."
  (when (windex-live-visible-frame-p windex-posframe--frame)
    (let ((parent-frame (frame-parameter windex-posframe--frame 'parent-frame))
          (parent-buffer (cdr (frame-parameter windex-posframe--frame 'posframe-parent-buffer))))
      (posframe-delete-frame windex-posframe--buffer)
      (select-window (get-buffer-window parent-buffer))
      (select-frame-set-input-focus parent-frame t)
      )))

(defun windex-posframe--active-p ()
  "Returns t if `windex-posframe--frame' exists."
  (and windex-posframe--frame
       (frame-live-p windex-posframe--frame)))

(defun windex-posframe-toggle ()
  "Toggle visibility of `windex-posframe' if it exists."
  (interactive)
  (if (windex-live-visible-frame-p windex-posframe--frame)
      (windex-posframe--hide)
    (let ((buffer (get-buffer windex-posframe--buffer)))
      (when (and buffer (buffer-live-p buffer))
        (windex-posframe--show buffer))))
  )

;;;###autoload
(defun windex-posframe-display-buffer (buffer-or-name &optional alist)
  "Display BUFFER-OR-NAME in `windex-posfrrame' with optional window ALIST."
  (let ((buffer (get-buffer-create buffer-or-name)))
    (windex-posframe--show buffer)
    (with-current-buffer buffer
      nil)))

(provide 'windex-posframe)

;;; windex-posframe.el ends here
