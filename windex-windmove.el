;;; windex-windmove.el --- Addditional Windmove methods -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'windmove)

(defun windex-windmove--enable-ace-window ()
  "Add `ace-window' actions to `windmove'."
  (unless (featurep 'ace-window)
    (error "ace-window is required"))

  (unless (fboundp #'windex-windmove-display-aw)
    (defun windex-windmove-display-aw (&optional arg)
      "Display the next buffer in window to the specified with `ace-window'.

If prefix ARG is \\[universal-argument], reselect a previously selected old window.
If `windmove-display-no-select' is non-nil, the meaning of
the prefix argument is reversed and it selects the new window."
      (interactive "P")
      (let ((no-select (xor (consp arg) windmove-display-no-select)))
        (display-buffer-override-next-command
         (lambda (_buffer alist)
           (let* ((type 'window)
                  (window (ace-select-window)))
             (cons window type)))
         (lambda (old-window new-window)
           (when (and (not (eq windmove-display-no-select 'ignore))
                      (window-live-p (if no-select old-window new-window)))
             (select-window (if no-select old-window new-window))))
         (format "[display-ace]")
         ))))

  (unless (fboundp #'windex-windmove-display-aw-in-direction)
    (defun windex-windmove-display-aw-in-direction (dir &optional arg)
      (let ((no-select (xor (consp arg) windmove-display-no-select)))
        (display-buffer-override-next-command
         (lambda (_buffer alist)
           (let* ((type 'window)
                  (window (ace-select-window)))
             (setq window (split-window nil nil dir) type 'window)
             (balance-windows)
             (cons window type)))
         (lambda (old-window new-window)
           (when (and (not (eq windmove-display-no-select 'ignore))
                      (window-live-p (if no-select old-window new-window)))
             (select-window (if no-select old-window new-window))))
         (format "[display-ace-%s]" dir)
         ))))

  (unless (fboundp #'windex-windmove-display-aw-left)
    (defun windex-windmove-display-aw-left (&optional arg)
      (interactive "P")
      (windex-windmove-display-aw-in-direction 'left arg)))

  (unless (fboundp #'windex-windmove-display-aw-up)
    (defun windex-windmove-display-aw-up (&optional arg)
      (interactive "P")
      (windex-windmove-display-aw-in-direction 'up arg)))

  (unless (fboundp #'windex-windmove-display-aw-down)
    (defun windex-windmove-display-aw-down (&optional arg)
      (interactive "P")
      (windex-windmove-display-aw-in-direction 'down arg)))

  (unless (fboundp #'windex-windmove-display-aw-right)
    (defun windex-windmove-display-aw-right (&optional arg)
      (interactive "P")
      (windex-windmove-display-aw-in-direction 'right arg)))
  )

(defun windex-windmove--disable-ace-window ()
  "Remove `ace-window' actions to `windmove'."
  (when (fboundp #'windex-windmove-display-aw)
    (fmakunbound #'windex-windmove-display-aw))
  (when (fboundp #'windex-windmove-display-aw-in-direction)
    (fmakunbound #'windex-windmove-display-aw-in-direction))
  (when (fboundp #'windex-windmove-display-aw-left)
    (fmakunbound #'windex-windmove-display-aw-left))
  (when (fboundp #'windex-windmove-display-aw-up)
    (fmakunbound #'windex-windmove-display-aw-up))
  (when (fboundp #'windex-windmove-display-aw-down)
    (fmakunbound #'windex-windmove-display-aw-down))
  (when (fboundp #'windex-windmove-display-aw-right)
    (fmakunbound #'windex-windmove-display-aw-right))
  )

(defun windex-windmove-display-in-split (dir &optional arg)
  "Display the next buffer in the window at direction DIR.
The next buffer is the buffer displayed by the next command invoked
immediately after this command (ignoring reading from the minibuffer).
Create a new window if there is no window in that direction.

By default, select the new window with a displayed buffer.
If `windmove-display-no-select' is `ignore', then allow the next command
to decide what window it selects.  With other non-nil values of
`windmove-display-no-select', this function reselects
a previously selected old window.

If prefix ARG is \\[universal-argument], reselect a previously selected old window.
If `windmove-display-no-select' is non-nil, the meaning of
the prefix argument is reversed and it selects the new window.

When `switch-to-buffer-obey-display-actions' is non-nil,
`switch-to-buffer' commands are also supported."
  (let ((no-select (xor (consp arg) windmove-display-no-select)))
    (display-buffer-override-next-command
     (lambda (_buffer alist)
       (let* ((type 'window)
              (window (split-window nil nil dir)))
         (balance-windows (window-main-window))
         (cons window type)))
     (lambda (old-window new-window)
       (when (and (not (eq windmove-display-no-select 'ignore))
                  (window-live-p (if no-select old-window new-window)))
         (select-window (if no-select old-window new-window))))
     (format "[display-%s]" dir)
     )))

(defun windex-windmove--display-in-direction-wrapper (fn dir &optional args)
  "`windmove-display-in-direction' around wrapper.
Use custom `+windmove-display-in-direction' for cardinal DIR;
default to original FN for all others."
  (cond
   ((or (eq dir 'left)
        (eq dir 'up)
        (eq dir 'down)
        (eq dir 'right))
    (apply #'windex-windmove-display-in-split dir args))
   (t (apply fn dir args))))

(provide 'windex-windmove)

;;; windex-windmove.el ends here
