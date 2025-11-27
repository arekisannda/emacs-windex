;;; windex-utils.el --- Windex Utility Methods -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(defun windex-first-live-window (window &optional no-other)
  "Return the first live child window of WINDOW.
WINDOW can be any window."
  (let (child-window)
    (while (and window (not child-window))
      (if (window-live-p window)
          (setq child-window window
                window nil)
        (cond
         ((setq win (window-left-child window)) (setq child-window (windex-first-live-window win)))
         ((setq win (window-top-child window)) (setq child-window (windex-first-live-window win)))

         )))
    child-window))

(defun windex-delete-windows-with-filter (filter-functions)
  "Delete windows satisfying any of FILTER-FUNCTIONS."
  (walk-windows
   (lambda (window)
     (when (seq-some (lambda (fn) (funcall fn window)) filter-functions)
       (unless (window-parameter window 'no-delete-other-windows)
         (delete-window window))))
   'no-minibuf
   nil))

(defun windex-window-popup-p (&optional window)
  "Return t if WINDOW is a popup window."
  (if (window-parameter window 'window-popup) t nil))

(defun windex-window-side-p (&optional window)
  "Return t if WINDOW is a side window."
  (if (and (window-parameter window 'window-side)
           (window-parameter window 'window-slot))
      t nil))

(defun windex-window-main-p (&optional window)
  "Return t if WINDOW is a main window."
  (not (or (windex-window-side-p) (windex-window-popup-p))))

(defun windex-window-with-parameters (parameters &optional frame first)
  "Get windows in FRAME with PARAMETERS.
If FIRST is non-nil, return first window."
  (let* ((frame (window-normalize-frame frame)))
    (let (matching-windows)
      (walk-windows
       (lambda (window)
         (dolist (param parameters)
           (when (eq (window-parameter window (car param)) (cdr param))
             (setq matching-windows (append matching-windows (list window)))
             )))
       'no-minibuf)
      (if first
          (car matching-windows)
        matching-windows))))

(defun windex-delete-window-with-parameters (parameters &optional frame)
  "Delete windows in FRAME with PARAMETERS."
  (dolist (window (windex-window-with-parameters parameters frame))
    (delete-window window)))

(defun windex-get-mru-in-main (&optional all-frames dedicated not-selected no-other)
  "Get most recently used main window."
  (let (best-window best-time time)
    (dolist (window (window-list-1 nil 'nomini all-frames))
      (setq time (window-use-time window))
      (when (and (or dedicated (not (window-dedicated-p window)))
                 (or (not not-selected) (not (eq window (selected-window))))
                 (or (not no-other)
                     (not (window-parameter window 'no-other-window)))
                 (or (not best-time) (> time best-time))
                 (or (not (window-parameter window 'window-side)))
                 (or (not (window-parameter window 'window-popup))))
        (setq best-time time)
        (setq best-window window)))
    best-window))

(defmacro windex-with-selector (frame-selector window-selector &rest body)
  "Run BODY with window returned by SELECTOR-FN."
  (declare (indent 1) (debug t))
  `(let* ((frame (window-normalize-frame
                  (and (functionp ,frame-selector)
                       (funcall ,frame-selector))))
          (window (window-normalize-window
                   (and (functionp ,window-selector)
                        (funcall ,window-selector)))))
     (with-selected-frame frame
       (with-selected-window window
         ,@body))))

(provide 'windex-utils)

;;; windex-utils.el ends here
