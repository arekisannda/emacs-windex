;;; windoex-state.el --- Windex State Methods -*- lexical-binding: t -*-

;; Author: Alexander Chan
;; Maintainer: Alexander Chan

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
(require 'cl-lib)
(require 'subr-x)

(defgroup windex-state nil
  "Customization for windex state."
  :group 'window
  :prefix "windex-state-")

;;; buffer node
(cl-defstruct windex-state-node
  value
  prev
  next)

(cl-defun windex-state-node-clear (node)
  "Clear NODE."
  (setf (windex-state-node-value node) nil)
  (setf (windex-state-node-prev node) nil)
  (setf (windex-state-node-next node) nil))

(cl-defun windex-state-node-add-next (node next)
  "Set as NEXT as `next` of NODE."
  (when next
    (setf (windex-state-node-prev next) node))
  (setf (windex-state-node-next node) next)
  node)

(cl-defun windex-state-node-add-prev (node prev)
  "Set as PREV as `prev` of NODE."
  (when prev
    (setf (windex-state-node-next prev) node))
  (setf (windex-state-node-prev node) prev)
  node)

(cl-defun windex-state-node-eq (node1 node2)
  "Return t if NODE1 equals NODE2."
  (and node1 node2
       (eq node1 node2)))

;;; Window

(defcustom windex-state-window-add-buffer-custom nil
  "Function for user defined buffer order behavior.
The function takes 2 arguments, a window state object and a buffer."
  :type 'function
  :group 'windex-state)

(defcustom windex-state-window-buffer-order 'fixed
  "Determines the order of buffers that are tracked by a window state.

If the value is `fixed, the window state list will have the following behavior:
- when displaying a tracked buffer, its positions will not be modified in
the window state list.
- when displaying a new buffer, a new node will be inserted between the
previous buffer and its next buffer.

If the value is `last, the window state list will have the following behavior:
- when displaying a tracked buffer, its position will be moved to the end
of the window state list.
- when display a new buffer, a new node will be added to end of the list.

If the value is `custom, the window state list will follow the behavior defined
by `windex-state-window-add-buffer-custom`"
  :type '(choice (const :tag "fixed" fixed)
                 (const :tag "last" last)
                 (const :tag "custom" custom))
  :group 'windex-state)

(cl-defstruct windex-state-window
  (id nil :read-only t)
  (buffers (make-hash-table :test #'eq) :read~only t)
  active
  head
  tail)

(cl-defun windex-state-window--add-buffer-fixed (ws buffer)
  "Add BUFFER to window state, WS."
  (let* ((buffers (windex-state-window-buffers ws))
         (prev-active (windex-state-window-active ws))
         (node (gethash buffer buffers)))

    (when node
      (setf (windex-state-window-active ws) node)
      (cl-return-from windex-state-window--add-buffer-fixed node))

    ;; else, create and add buffer to window state
    (setq node (make-windex-state-node :value buffer
                                       :next nil
                                       :prev nil))
    (when (hash-table-empty-p buffers)
      ;; first buffer tracked by  window state
      (setf (windex-state-window-head ws) node)
      (setf (windex-state-window-tail ws) node)
      (setf (windex-state-window-active ws) node)
      (puthash buffer node buffers)
      (cl-return-from windex-state-window--add-buffer-fixed node))

    (unless prev-active
      ;; unlikely case of non-empty window state without an active buffer
      ;; in such a case, add buffer to tail of list
      (setf (windex-state-window-tail ws) node)
      (setf (windex-state-window-active ws) node)
      (puthash buffer node buffers)
      (cl-return-from windex-state-window--add-buffer-fixed node))

    (if (windex-state-node-eq prev-active (windex-state-window-tail ws))
        (setf (windex-state-window-tail ws) node))
    (windex-state-node-add-next node (windex-state-node-next prev-active))
    (windex-state-node-add-next prev-active node)
    (setf (windex-state-window-active ws) node)
    (puthash buffer node buffers)
    (cl-return-from windex-state-window--add-buffer-fixed node)))

(cl-defun windex-state-window--add-buffer-last (ws buffer)
  "Add BUFFER to window state, WS."
  (let* ((buffers (windex-state-window-buffers ws))
         (node (gethash buffer buffers))
         (head (windex-state-window-head ws))
         (tail (windex-state-window-tail ws)))

    (when node
      (setf (windex-state-window-active ws) node)

      (when (windex-state-node-eq tail node)
        (cl-return-from windex-state-window--add-buffer-last node))

      (when (windex-state-node-eq head node)
        (setf (windex-state-window-head ws) (windex-state-node-next node)))

      (windex-state-window-remove-node node)
      (windex-state-node-add-next tail node)
      (setf (windex-state-window-tail ws) node)
      (cl-return-from windex-state-window--add-buffer-last node))

    ;; else, create and add buffer to window state
    (setq node (make-windex-state-node :value buffer
                                       :next nil
                                       :prev nil))

    (when (hash-table-empty-p buffers)
      ;; add first buffer to window state
      (setf (windex-state-window-head ws) node)
      (setf (windex-state-window-tail ws) node)
      (setf (windex-state-window-active ws) node)
      (puthash buffer node buffers)
      (cl-return-from windex-state-window--add-buffer-last node))

    ;; add buffer end of list
    (windex-state-node-add-next tail node)
    (setf (windex-state-window-tail ws) node)
    (setf (windex-state-window-active ws) node)
    (puthash buffer node buffers)
    (cl-return-from windex-state-window--add-buffer-last node)))

(cl-defun windex-state-window-add-buffer (ws buffer)
  "Add BUFFER to window state, WS."
  (cond
   ((eq windex-state-window-buffer-order 'fixed)
    (windex-state-window--add-buffer-fixed ws buffer))

   ((eq windex-state-window-buffer-order 'last)
    (windex-state-window--add-buffer-last ws buffer))

   ((and (eq windex-state-window-buffer-order 'custom)
         (functionp windex-state-window-add-buffer-custom))
    (funcall windex-state-window-add-buffer-custom ws buffer))))

(cl-defun windex-state-window-remove-node (node)
  "Remove NODE from bidirectional list."
  (let* ((prev (windex-state-node-prev node))
         (next (windex-state-node-next node)))
    (cond
     ((and prev next)
      (setf (windex-state-node-next prev) next)
      (setf (windex-state-node-prev next) prev))
     (next
      (setf (windex-state-node-prev next) nil))
     (prev
      (setf (windex-state-node-next prev) nil))))
  (setf (windex-state-node-prev node) nil)
  (setf (windex-state-node-next node) nil)
  node)

(defcustom windex-state-window-set-active-buffer-after-delete 'previous
  "Determines which buffer should be set as active after a deletion operation.

If the value is `next, the deleted node's `next` will be set as active; if the
deleted node does not have a `next`, the `head` node will be set as active.

If the value is `previous, the deleted node's `prev` will be set as active; if
the deleted node does not have a `next`, the `tail` node will be set as active."
  :type '(choice (const :tag "next" next)
                 (const :tag "previous" previous))
  :group 'windex-state)

(cl-defun windex-state-window-delete-buffer--set-next-active (ws node)
  "`windex-state-window-delete-buffer` auxiliary method for setting active.
Set NODE's `next` as active before it is removed from window state, WS."
  (when (windex-state-node-eq (windex-state-window-active ws) node)
    (if-let ((next (windex-state-node-next node)))
        (setf (windex-state-window-active ws) next)
      (setf (windex-state-window-active ws) (windex-state-window-head ws)))))


(cl-defun windex-state-window-delete-buffer--set-prev-active (ws node)
  "`windex-state-window-delete-buffer` auxiliary method for setting active.
Set NODE's `prev` as active before it is removed from window state, WS."
  (when (windex-state-node-eq (windex-state-window-active ws) node)
    (if-let ((prev (windex-state-node-prev node)))
        (setf (windex-state-window-active ws) prev)
      (setf (windex-state-window-active ws) (windex-state-window-tail ws)))))

(cl-defun windex-state-window-delete-buffer (ws buffer)
  "Remove BUFFER from window state, WS."
  (let* ((buffers (windex-state-window-buffers ws))
         (head (windex-state-window-head ws))
         (tail (windex-state-window-tail ws))
         (node (gethash buffer buffers)))

    (unless node
      (cl-return-from windex-state-window-delete-buffer))

    (when (windex-state-node-eq node tail)
      (setf (windex-state-window-tail ws) (windex-state-node-prev node)))

    (when (windex-state-node-eq node head)
      (setf (windex-state-window-head ws) (windex-state-node-next node)))

    (cond
     ((eq windex-state-window-set-active-buffer-after-delete 'previous)
      (windex-state-window-delete-buffer--set-prev-active ws node))
     ((eq windex-state-window-set-active-buffer-after-delete 'next)
      (windex-state-window-delete-buffer--set-next-active ws node)))

    (windex-state-window-remove-node node)
    (remhash buffer buffers)
    (cl-return-from windex-state-window-delete-buffer)))

(cl-defun windex-state-window-get-next-buffer (ws buffer)
  "Get next buffer of BUFFER of window state, WS."
  (let* ((buffers (windex-state-window-buffers ws))
         (node (gethash buffer buffers)))
    (unless node
      (cl-return-from windex-state-window-get-next-buffer nil))
    (cl-return-from windex-state-window-get-next-buffer
      (windex-state-node-next node))))

(cl-defun windex-state-window-get-prev-buffer (ws buffer)
  "Get prev buffer of BUFFER of window state, WS."
  (let* ((buffers (windex-state-window-buffers ws))
         (node (gethash buffer buffers)))
    (unless node
      (cl-return-from windex-state-window-get-prev-buffer nil))
    (cl-return-from windex-state-window-get-prev-buffer
      (windex-state-node-prev node))))

(defcustom windex-state-window-auto-add-active nil
  "Determine if `windex-state-window-set-active-buffer` should auto add buffer.

If the value is `auto and if the buffer is not tracked by the window state,
automatically add the buffer to the state.

If the value is nil, do not automatically add the buffer to the window state."
  :type '(choice (const :tag "Auto" auto)
                 (const :tag "None" nil))
  :group 'windex-state)

(cl-defun windex-state-window-set-active-buffer (ws buffer)
  "Set BUFFER as the active in window state, WS."
  (let* ((buffers (windex-state-window-buffers ws))
         (node (gethash buffer buffers)))

    (unless node
      (cond
       ((eq windex-state-window-auto-add-active 'auto)
        (setq node (windex-state-window-add-buffer ws buffer))
        (cl-return-from windex-state-window-set-active-buffer node))
       (t
        (user-error "Buffer is not tracked in window state")
        (cl-return-from windex-state-window-set-active-buffer))))

    (setf (windex-state-window-active ws) node)
    (cl-return-from windex-state-window-set-active-buffer node)))

(cl-defun windex-state-window-clear (ws)
  "Clear window state, WS."
  (let ((it (windex-state-window-head ws)))
    (while it
      (let ((curr it))
        (setq it (windex-state-node-next it))
        (windex-state-node-clear curr)))
    (setf (windex-state-window-head ws) nil)
    (setf (windex-state-window-tail ws) nil)
    (clrhash (windex-state-window-buffers ws))
    (cl-return-from windex-state-window-clear ws)))

;;; Workspace
(cl-defstruct windex-state-workspace
  name
  (windows (make-hash-table :test #'equal) :read-only t))

(cl-defun windex-state-workspace-create-window (ws window-id)
  "Create window state with WINDOW-ID to workspace state, WS."
  (let* ((windows (windex-state-workspace-windows ws))
         (window (gethash window-id windows)))

    (unless window
      (setq window (make-windex-state-window :id window-id))
      (puthash window-id window windows))

    (cl-return-from windex-state-workspace-create-window window)))

(cl-defun windex-state-workspace-delete-window (ws window-id)
  "Remove window state with WINDOW-ID from workspace state, WS."
  (let* ((windows (windex-state-workspace-windows ws))
         (window (gethash window-id windows)))

    (unless window
      (cl-return-from windex-state-workspace-delete-window))

    ;; clear window state
    (windex-state-window-clear window)
    (remhash window-id windows)

    (cl-return-from windex-state-workspace-delete-window)))

(cl-defun windex-state-workspace-get-window (ws window-id)
  "Get window state with WINDOW-ID from workspace state, WS."
  (let* ((windows (windex-state-workspace-windows ws)))
    (gethash window-id windows)))

(cl-defun windex-state-workspace-clear (ws)
  "Clear workspace state, WS."
  (let ((windows (windex-state-workspace-windows ws)))
    (cl-loop for window being the hash-values of windows do
             (windex-state-window-clear window))
    (clrhash windows)))

(defvar windex-state (make-hash-table :test #'equal))

(cl-defun windex-state-create (workspace-id)
  "Create workspace state with WORKSPACE-ID."
  (puthash workspace-id (make-windex-state-workspace :name workspace-id)
           windex-state))

(cl-defun windex-state-delete (workspace-id)
  "Remove workspace state with WORKSPACE-ID."
  (let* ((workspace (gethash workspace-id windex-state)))
    (when workspace
      (windex-state-workspace-clear workspace)
      (remhash workspace-id windex-state))))

(cl-defun windex-state-delete-all ()
  "Clear `windex` state."
  (cl-loop for workspace being the hash-keys of windex-state do
           (windex-state-delete workspace)))

(cl-defun windex-state-get (workspace-id)
  "Get workspace state with WORKSPACE-ID."
  (gethash workspace-id windex-state))

(provide 'windex-state)

;;; windex-state.el ends here
