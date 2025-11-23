;;; windex-purpose.el --- Window purpose methods -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(defgroup windex-purpose nil
  "Customization for `windex' purpose."
  :group 'windex
  :prefix "windex-purpose-")

(defcustom windex-purpose-alist '()
  "List of window purposes."
  :type '(alist :key-type (symbol :tag "Purpose Identifier")
                :value-type (plist
                             :options
                             (((const :tag "Activate Function" :activate) function)
                              ((const :tag "Deactivate Function" :deactivate) function))))
  :group 'windex-purpose
  :group 'convenience)

(defun windex-purpose--set (&optional window purpose)
  "Set window parameter`window-purpose` for WINDOW to PURPOSE."
  (let* ((purpose-list (windex-purpose--list))
         (prompt (format "Set purpose: "))
         (purpose (or purpose (s-trim (completing-read prompt purpose-list (-const t) t))))
         (purpose (cond ((stringp purpose) (intern purpose))
                        ((symbolp purpose) purpose)))
         (activate-fn (plist-get :activate (cdr (assoc purpose windex-purpose-alist)))))

    (if (eq (window-parameter window 'window-purpose) purpose)
        (windex-purpose--unset window)
      (windex-purpose--unset window)
      (set-window-parameter window 'window-purpose purpose)
      (and (functionp activate-fn) (funcall activate-fn window))
      )))

(defun windex-purpose--list ()
  "Return list of window purposes."
  (mapcar
   (lambda (sym)
     (let ((name (symbol-name sym)))
       (if (eq (window-parameter nil 'window-purpose) sym)
           (propertize name 'face `(:foreground ,(doom-color 'green)))
         name)))
   (mapcar #'car windex-purpose-alist)))

(defun windex-purpose--unset (&optional window)
  "Unset window parameter`window-purpose` for WINDOW."
  (let* ((purpose (window-parameter window 'window-purpose))
         (deactivate-fn (plist-get :deactivate (cdr (assoc purpose windex-purpose-alist)))))
    (set-window-parameter window 'window-purpose nil)
    (and (functionp deactivate-fn) (funcall deactivate-fn window))))

(defun windex-purpose--get (&optional window)
  "Get value of window parameter `window-purpose.
If WINDOW is nil, use currently selected window."
  (window-parameter window 'window-purpose))

;;;###autoload
(defun windex-set-window-purpose (&optional prefix)
  "Set/unset window purpose.

With prefix PREFIX \\[universal-argument], remove window purpose.
With double-prefix PREFIX \\[universal-argument], echo window purpose."
  (interactive "p")
  (pcase prefix
    (4 (windex-purpose--unset))
    (_ (windex-purpose--set))))

;;;###autoload
(defun windex-window-with-purpose (&optional purpose frame)
  "Select window with PURPOSE on FRAME.
FRAME defaults to the selected frame.
If NOSELECT is nil, select and return window."
  (interactive)
  (let* ((purpose-list (windex-purpose--list))
         (prompt (format "Select purpose: "))
         (purpose (or purpose (s-trim (completing-read prompt purpose-list (-const t) t)))))
    (if-let ((window (window-with-parameter 'window-purpose
                                            (cond ((stringp purpose) (intern purpose))
                                                  ((symbolp purpose) purpose))
                                            frame)))
        (if (called-interactively-p) (select-window window) window))))

(provide 'windex-purpose)

;;; windex-purpose.el ends here
