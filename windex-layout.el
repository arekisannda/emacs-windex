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

(defcustom windex-layout-restore-window-state-filter-function #'always
  "Function to filter windows states to be restored."
  :type 'function
  :group 'windex-layout)

(defcustom windex-layout-before-run-hook nil
  "Functions to run before applying layout recipe."
  :type 'function
  :group 'windex-layout)

(defcustom windex-layout-after-run-hook nil
  "Functions to run after applying layout recipe."
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

(defun windex-layout--create-windows-1 (split-fn node window-states)
  "Helper method for `windex-layout--create-windows'.
Use SPLIT-FN to perform window split for NODE.
Restore WINDOW-STATES in window layouts."
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
          (setq window-states (windex-layout--create-windows c window-states)))
        ))
    window-states))

(defun windex-layout--create-windows (node window-states)
  "Create layout defined in `windex-layout-alist'.
NODE is the root of the subtree.
Restore WINDOW-STATES in window layouts."
  (if (not (plistp node))
      window-states
    (pcase (plist-get node :type)
      ('row
       (setq window-states (windex-layout--create-windows-1 #'split-window-below node window-states)))
      ('col
       (setq window-states (windex-layout--create-windows-1 #'split-window-right node window-states)))
      (_
       (let ((ws (car window-states))
             (window (or (plist-get node :window) (selected-window)))
             (apply-fn (plist-get node :apply))
             (select (plist-get node :select)))

         (with-selected-window window
           (cond
            (ws (window-state-put ws window 'safe))
            (t (set-window-buffer window (get-buffer-create windex-layout--blank-buffer-name))
               (set-window-prev-buffers window nil)
               (set-window-next-buffers window nil)
               )))

         (when (functionp apply-fn)
           (funcall apply-fn window))
         (when (and select (not windex-layout--selected-window))
           (setq windex-layout--selected-window window))
         (cdr window-states))
       ))))

(defun windex-layout--retrieve-main-window-states ()
  (mapcar
   #'window-state-get
   (seq-filter
    windex-layout-restore-window-state-filter-function
    (window-list nil nil (frame-first-window)))))

(defun windex-layout--run-recipe (layout &optional window-states)
  "Create layout defined in `windex-layout-alist' for LAYOUT."
  (let* ((params (cdr (assoc layout windex-layout-alist)))
         (recipe-custom (plist-get params :custom))
         (recipe-tree (plist-get params :tree))
         (focused-buffer (window-buffer (selected-window))))
    (run-hooks windex-layout-before-run-hook)
    (delete-other-windows)
    (cond
     ((functionp recipe-custom) (funcall recipe-custom))
     (t (windex-layout--create-windows recipe-tree window-states)))
    (run-hooks windex-layout-after-run-hook)
    (balance-windows)
    (select-window (or windex-layout--selected-window
                       (get-buffer-window focused-buffer)
                       (get-mru-window)))
    (setq windex-layout--selected-window nil)))

;;;###autoload
(defun windex-layout-apply (&optional blankp)
  "Select predefined layouts.
The flag BLANKP indicates whether or not to use existing window states."
  (interactive (list current-prefix-arg))
  (let* ((layouts (mapcar #'windex-layout--create-candidate windex-layout-alist))
         (completion-extra-properties '(:annotation-function windex-layout--annotation-fn))
         (prompt (format "Choose layout: "))
         (layout (s-trim (completing-read prompt layouts (-const t) t)))
         (window-states (unless blankp (windex-layout--retrieve-main-window-states))))

    (windex-layout--run-recipe (intern layout) window-states)))

(provide 'windex-layout)

;;; windex-layout.el ends here
