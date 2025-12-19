;;; windex-layout.el --- Window layout methods -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(require 'windex-utils)

(defgroup windex-layout nil
  "Customization for `windex' layout."
  :group 'windex
  :prefix "windex-layout-")

(define-widget 'windex-layout--tree 'lazy
  "`windex-layout' tree node type."
  :tag "Tree"
  :type
  '(plist :options
          ((:type (choice (const nil)
                          (const buf)
                          (const row)
                          (const col)))
           (:select boolean)
           (:apply function)
           (:nodes (repeat windex-layout--tree))
           )))

(defcustom windex-layout-alist nil
  "List of window layouts."
  :type
  '(alist :key-type (symbol :tag "Name")
          :value-type (plist :options
                             ((:description string)
                              (:custom function)
                              (:tree windex-layout--tree)
                              )))
  :group 'windex-layout
  :group 'convenience)

(defcustom windex-layout-buffer-list-apply-function nil
  "Function that returns buffers to be assigned in layout."
  :type 'function
  :group 'windex-layout)

(defcustom windex-layout-buffer-list-restore-function nil
  "Function that returns buffers to be restored using `display-buffer'.
Used for displaying buffers that have been filtered by `windex-layout-buffer-list-apply-function'."
  :type 'function
  :group 'windex-layout)

(defvar windex-layout--blank-buffer-name " *windex-layout-blank*"
  "Windex layout blank buffer name.")

(defvar windex-layout--selected-window nil)

(defun windex-layout--create-candidate (cand)
  "Create layout candidate from CAND."
  (let* ((layout (car cand))
         (params (cdr cand))
         (layout-info (plist-get params :description)))
    (list (util/strings-pad-string layout 20) `(:info ,layout-info))))

(defun windex-layout--annotation-fn (layout)
  "Annotate LAYOUT option with description."
  (let* ((params (car (last (assoc layout minibuffer-completion-table))))
         (info (plist-get params :info)))
    (concat " " (util/strings-add-font-lock info 'font-lock-comment-face))))

(defun windex-layout--create-windows-1 (split-fn node buffers)
  "Helper method for `windex-layout--create-windows'.
Use SPLIT-FN to perform window split for NODE.
Display BUFFERS in newly created windows."
  (let ((window nil)
        (cnodes (plist-get node :nodes)))
    (dolist (c cnodes)
      (setq window (if window
                       (funcall split-fn nil window)
                     (selected-window)))
      (plist-put c :window window))
    (dolist (c cnodes)
      (let ((window (plist-get c :window)))
        (with-selected-window window
          (setq buffers (windex-layout--create-windows c buffers)))
        ))
    buffers))

(defun windex-layout--create-windows (node buffers)
  "Create layout defined in `windex-layout-alist'.
NODE is the root of the subtree.
Display BUFFERS in newly created windows."
  (if (not (plistp node))
      buffers
    (pcase (plist-get node :type)
      ('row
       (setq buffers (windex-layout--create-windows-1 #'split-window-below node buffers)))
      ('col
       (setq buffers (windex-layout--create-windows-1 #'split-window-right node buffers)))
      (_
       (let ((buffer (car buffers))
             (window (or (plist-get node :window) (selected-window)))
             (apply-fn (plist-get node :apply))
             (select (plist-get node :select)))
         (set-window-buffer window
                            (or (and (bufferp buffer) buffer)
                                (get-buffer-create windex-layout--blank-buffer-name)))
         (when (functionp apply-fn)
           (funcall apply-fn window))
         (when (and select (not windex-layout--selected-window))
           (setq windex-layout--selected-window window))
         (cdr buffers))
       ))))

(defun windex-layout--run-recipe (layout)
  "Create layout defined in `windex-layout-alist' for LAYOUT."
  (let* ((params (cdr (assoc layout windex-layout-alist)))
         (recipe-custom (plist-get params :custom))
         (recipe-tree (plist-get params :tree))
         (focused-buffer (window-buffer (selected-window)))
         (restore-buffers (and (functionp windex-layout-buffer-list-restore-function)
                               (funcall windex-layout-buffer-list-restore-function)))
         (buffers (and (functionp windex-layout-buffer-list-apply-function)
                       (funcall windex-layout-buffer-list-apply-function))))

    (delete-other-windows-internal)
    (cond
     ((functionp recipe-custom) (funcall recipe-custom))
     (t (windex-layout--create-windows recipe-tree buffers)))

    (balance-windows)
    (dolist (buf restore-buffers) (display-buffer buf))
    (select-window (or windex-layout--selected-window
                       (get-buffer-window focused-buffer)
                       (get-mru-window)))
    (setq windex-layout--selected-window nil)))

;;;###autoload
(defun windex-layout-apply ()
  "Select predefined layouts."
  (interactive)
  (let* ((layouts (mapcar #'windex-layout--create-candidate windex-layout-alist))
         (completion-extra-properties '(:annotation-function windex-layout--annotation-fn))
         (prompt (format "Choose layout: "))
         (layout (s-trim (completing-read prompt layouts (-const t) t))))

    (windex-layout--run-recipe (intern layout))))

(provide 'windex-layout)

;;; windex-layout.el ends here
