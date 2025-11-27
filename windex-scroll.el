;;; windex-scroll.el --- Window scroll methods  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(require 'windex-utils)

(defgroup windex-scroll nil
  "Customization for `windex' scroll."
  :group 'windex
  :prefix "windex-scroll-")

(defcustom windex-scroll-lines 5
  "Scroll line step."
  :type 'integer
  :group 'windex-scroll)

(defcustom windex-scroll-hlines 5
  "Horizontal scroll line step."
  :type 'integer
  :group 'windex-scroll)

(defcustom windex-scroll-left-function nil
  "Function used to scroll left."
  :type 'function
  :group 'windex-scroll)

(defcustom windex-scroll-right-function nil
  "Function used to scroll right."
  :type 'function
  :group 'windex-scroll)

(defcustom windex-scroll-up-function nil
  "Function used to scroll up."
  :type 'function
  :group 'windex-scroll)

(defcustom windex-scroll-down-function nil
  "Function used to scroll down."
  :type 'function
  :group 'windex-scroll)

(defun windex-scroll-left ()
  "Scroll window left."
  (interactive)
  (unless windex-scroll-left-function (error "Scroll function not set"))
  (funcall windex-scroll-left-function windex-scroll-hlines))

(defun windex-scroll-right ()
  "Scroll window right."
  (interactive)
  (unless windex-scroll-right-function (error "Scroll function not set"))
  (funcall windex-scroll-right-function windex-scroll-hlines))

(defun windex-scroll-down ()
  "Scroll window down."
  (interactive)
  (unless windex-scroll-down-function (error "Scroll function not set"))
  (funcall windex-scroll-down-function windex-scroll-lines))

(defun windex-scroll-up ()
  "Scroll window up."
  (interactive)
  (unless windex-scroll-up-function (error "Scroll function not set"))
  (funcall windex-scroll-up-function windex-scroll-lines))

(defcustom windex-scroll-frame-selector nil
  "Function used to select other window."
  :type 'function
  :group 'windex-scroll)

(defcustom windex-scroll-window-selector nil
  "Function used to select other window."
  :type 'function
  :group 'windex-scroll)

(defun windex-scroll-selector-down ()
  "Scroll other window down."
  (interactive)
  (windex-with-selector windex-scroll-frame-selector windex-scroll-window-selector
    (windex-scroll-down)))

(defun windex-scroll-selector-up ()
  "Scroll other window up."
  (interactive)
  (windex-with-selector windex-scroll-frame-selector windex-scroll-window-selector
    (windex-scroll-up)))

(defun windex-scroll-selector-left ()
  "Scroll other window left."
  (interactive)
  (windex-with-selector windex-scroll-frame-selector windex-scroll-window-selector
    (windex-scroll-left)))

(defun windex-scroll-selector-right ()
  "Scroll other window right."
  (interactive)
  (windex-with-selector windex-scroll-frame-selector windex-scroll-window-selector
    (windex-scroll-right)))

(defun windex-scroll-minibuffer-scroll-other-down ()
  "Scroll other window down."
  (interactive)
  (minibuffer-scroll-other-window windex-scroll-lines))

(defun windex-scroll-minibuffer-scroll-other-up ()
  "Scroll other window up."
  (interactive)
  (minibuffer-scroll-other-window-down windex-scroll-lines))

(defmacro windex-scroll-line-to (ppt)
  `(defun ,(intern (format "windex-scroll-line-to-%d-ppt" ppt)) ()
     ,(format "Scroll current to %d%% of window" ppt)
     (interactive)
     (when-let* ((buffer-height (count-lines (point-min) (point-max)))
                 (_ (> buffer-height (window-height)))
                 (window-ppt (/ ,ppt 100.0))
                 (window-start-line (line-number-at-pos (window-start)))
                 (target-line (+ window-start-line (ceiling (* (window-height) window-ppt))))
                 (current-line (line-number-at-pos))
                 (scroll-count (- target-line current-line)))
       (scroll-down-line scroll-count))))

(provide 'windex-scroll)

;;; windex-scroll.el ends here
