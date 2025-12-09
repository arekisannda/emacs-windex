;;; windex-frame.el --- Windex frame methods -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'windex-utils)

(defvar windex-frame--default-name "windex-frame")
(defvar windex-frame--name "windex-frame-popup")
(defvar windex-frame--frame nil)

(defcustom windex-frame-min-width 180
  "Minimum `windex-frame' width in characters."
  :type 'integer)

(defcustom windex-frame-min-height 60
  "Minimum `windex-frame' height in lines."
  :type 'integer)

(defun windex-frame-get-dimensions ()
  "The default dimensions function used by `windex-frame'."
  `((width . ,(min windex-frame-min-width
                   (ceiling (* (frame-width) (/ 2560.0 (display-pixel-width))))))
    (height . ,(min windex-frame-min-height
                    (ceiling (* (frame-height) (/ 1440.0 (display-pixel-height))))))))

;;;###autoload
(defun windex-frame-display-buffer (buffer-or-name &optional alist)
  "Display BUFFER-OR-NAME in `frame' with optional window ALIST."
  (let ((parent-frame (selected-frame))
        (buffer (get-buffer-create buffer-or-name)))
    (unless (windex-live-visible-frame-p windex-frame--frame)
      (setq windex-frame--frame
            (make-frame
             (append
              `((delete-before . ,parent-frame)
                (no-other-frame . t)
                (left . 0.5)
                (top . 0.5))
              alist
              (windex-frame-get-dimensions)
              ))
            ))
    (with-selected-frame windex-frame--frame
      (window--display-buffer buffer (selected-window) 'window alist)
      )
    ))

(provide 'windex-frame)

;;; windex-frame.el ends here
