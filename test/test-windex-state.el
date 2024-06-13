;;; test-windex-state.el --- Tests for windex-state -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'windex-state)
(require 'ert)

(ert-deftest windex-state-node ()
  "Test `windex-state-node` operations."
  (let ((node1 (make-windex-state-node :value 1 :prev nil :next nil))
        (node2 (make-windex-state-node :value 2 :prev nil :next nil))
        (node3 (make-windex-state-node :value 3 :prev nil :next nil)))
    (windex-state-node-add-next node1 node2)
    (windex-state-node-add-prev node3 node2)
    (should (windex-state-node-eq (windex-state-node-next node1) node2))
    (should (windex-state-node-eq (windex-state-node-prev node2) node1))
    (should (windex-state-node-eq (windex-state-node-next node2) node3))
    (should (windex-state-node-eq (windex-state-node-prev node3) node2))))

(defmacro windex-test-with-window-order (ws order &rest body)
  "Run BODY with `windex-state-window-buffer-order` set to ORDER for window state, WS."
  (declare (indent 2))
  `(progn
     (let ((,ws (make-windex-state-window :id "test-window"))
           (windex-state-window-buffer-order ,order))
       (unwind-protect
           ,@body
         ))))

(ert-deftest windex-state-window--add-buffer-fixed ()
  "Test `windex-state-window--add-buffer-fixed` with multiple adds."
  (windex-test-with-window-order ws 'fixed
    (let (node1 node2 node3)
      (setq node1 (windex-state-window-add-buffer ws 1))
      (setq node2 (windex-state-window-add-buffer ws 2))
      (setq node3 (windex-state-window-add-buffer ws 3))

      (should (eq (windex-state-node-next node1) node2))
      (should (eq (windex-state-node-next node2) node3))
      (should (eq (windex-state-node-next node3) nil))

      (should (eq (windex-state-window-active ws) node3))
      (should (eq (windex-state-window-head ws) node1))
      (should (eq (windex-state-window-tail ws) node3)))))

(ert-deftest windex-state-window--add-buffer-fixed-add-between ()
  "Test `windex-state-window--add-buffer-fixed` with add between nodes."
  (windex-test-with-window-order ws 'fixed
    (let (node1 node2 node3 node4)
      (setq node1 (windex-state-window-add-buffer ws 1))
      (setq node2 (windex-state-window-add-buffer ws 2))
      (setq node3 (windex-state-window-add-buffer ws 3))

      ;; with 1 as active, node 4 should be placed
      ;; between 1 and 2
      (setf (windex-state-window-active ws) node1)
      (setq node4 (windex-state-window-add-buffer ws 4))

      (should (eq (windex-state-node-next node1) node4))
      (should (eq (windex-state-node-next node4) node2))
      (should (eq (windex-state-node-next node2) node3))
      (should (eq (windex-state-node-next node3) nil))

      (should (eq (windex-state-window-active ws) node4))
      (should (eq (windex-state-window-head ws) node1))
      (should (eq (windex-state-window-tail ws) node3)))))

(ert-deftest windex-state-window--add-buffer-fixed-add-existing ()
  "Test `windex-state-window--add-buffer-fixed` with add of an existing node."
  (windex-test-with-window-order ws 'fixed
    (let (node1 node2 node3)
      (setq node1 (windex-state-window-add-buffer ws 1))
      (setq node2 (windex-state-window-add-buffer ws 2))
      (setq node3 (windex-state-window-add-buffer ws 3))

      ;; add existing node 1
      (windex-state-window-add-buffer ws 1)

      (should (eq (windex-state-node-next node1) node2))
      (should (eq (windex-state-node-next node2) node3))
      (should (eq (windex-state-node-next node3) nil))

      (should (eq (windex-state-window-active ws) node1))
      (should (eq (windex-state-window-head ws) node1))
      (should (eq (windex-state-window-tail ws) node3)))))

(ert-deftest windex-state-window--add-buffer-last ()
  "Test `windex-state-window--add-buffer-last` with multiple adds."
  (windex-test-with-window-order ws 'last
    (let (node1 node2 node3)
      (setq node1 (windex-state-window-add-buffer ws 1))
      (setq node2 (windex-state-window-add-buffer ws 2))
      (setq node3 (windex-state-window-add-buffer ws 3))

      (should (eq (windex-state-node-next node1) node2))
      (should (eq (windex-state-node-next node2) node3))
      (should (eq (windex-state-node-next node3) nil))

      (should (eq (windex-state-window-active ws) node3))
      (should (eq (windex-state-window-head ws) node1))
      (should (eq (windex-state-window-tail ws) node3)))))

(ert-deftest windex-state-window--add-buffer-last-add-existing-between ()
  "Test `windex-state-window--add-buffer-last` of existing middle buffer."
  (windex-test-with-window-order ws 'last
    (let (node1 node2 node3)
      (setq node1 (windex-state-window-add-buffer ws 1))
      (setq node2 (windex-state-window-add-buffer ws 2))
      (setq node3 (windex-state-window-add-buffer ws 3))
      ;; add existing middle object
      (windex-state-window-add-buffer ws 2)

      (should (eq (windex-state-node-next node1) node3))
      (should (eq (windex-state-node-next node2) nil))
      (should (eq (windex-state-node-next node3) node2))

      (should (eq (windex-state-window-active ws) node2))
      (should (eq (windex-state-window-head ws) node1))
      (should (eq (windex-state-window-tail ws) node2)))))

(ert-deftest windex-state-window--add-buffer-last-add-existing-head ()
  "Test `windex-state-window--add-buffer-last` of existing head buffer."
  (windex-test-with-window-order ws 'last
    (let (node1 node2 node3)
      (setq node1 (windex-state-window-add-buffer ws 1))
      (setq node2 (windex-state-window-add-buffer ws 2))
      (setq node3 (windex-state-window-add-buffer ws 3))
      ;; add existing middle object
      (windex-state-window-add-buffer ws 1)

      (should (eq (windex-state-node-next node1) nil))
      (should (eq (windex-state-node-next node2) node3))
      (should (eq (windex-state-node-next node3) node1))

      (should (eq (windex-state-window-active ws) node1))
      (should (eq (windex-state-window-head ws) node2))
      (should (eq (windex-state-window-tail ws) node1)))))

(ert-deftest windex-state-window--add-buffer-last-add-existing-tail ()
  "Test `windex-state-window--add-buffer-last` of existing tail buffer."
  (windex-test-with-window-order ws 'last
    (let (node1 node2 node3)
      (setq node1 (windex-state-window-add-buffer ws 1))
      (setq node2 (windex-state-window-add-buffer ws 2))
      (setq node3 (windex-state-window-add-buffer ws 3))
      ;; add existing middle object
      (windex-state-window-add-buffer ws 3)

      (should (eq (windex-state-node-next node1) node2))
      (should (eq (windex-state-node-next node2) node3))
      (should (eq (windex-state-node-next node3) nil))

      (should (eq (windex-state-window-active ws) node3))
      (should (eq (windex-state-window-head ws) node1))
      (should (eq (windex-state-window-tail ws) node3)))))


(defmacro windex-test-with-active-after-delete (set-active &rest body)
  "Run BODY with `windex-state-window-set-active-buffer-after-delete` set \
to SET-ACTIVE for window state, WS."
  (declare (indent 2))
  `(progn
     (let ((windex-state-window-set-active-buffer-after-delete ,set-active))
       (unwind-protect
           ,@body
         ))))

(ert-deftest windex-state-window-delete-buffer-with-next-rule-single-node ()
  "Test `windex-state-window-delete` on last node with `next rule."
  (windex-test-with-window-order ws 'last
    (windex-test-with-active-after-delete 'next
        (let (node1)
          (setq node1 (windex-state-window-add-buffer ws 1))
          (should (equal (hash-table-count (windex-state-window-buffers ws)) 1))

          (should (eq (windex-state-window-head ws) node1))
          (should (eq (windex-state-window-tail ws) node1))
          (should (eq (windex-state-window-active ws) node1))

          (windex-state-window-delete-buffer ws 1)

          (should (equal (hash-table-count (windex-state-window-buffers ws)) 0))

          (should (eq (windex-state-window-head ws) nil))
          (should (eq (windex-state-window-tail ws) nil))
          (should (eq (windex-state-window-active ws) nil))))))

(ert-deftest windex-state-window-delete-buffer-with-prev-rule-single-node ()
  "Test `windex-state-window-delete` on last node with `previous rule."
  (windex-test-with-window-order ws 'last
    (windex-test-with-active-after-delete 'previous
        (let (node1)
          (setq node1 (windex-state-window-add-buffer ws 1))
          (should (equal (hash-table-count (windex-state-window-buffers ws)) 1))

          (should (eq (windex-state-window-head ws) node1))
          (should (eq (windex-state-window-tail ws) node1))
          (should (eq (windex-state-window-active ws) node1))

          (windex-state-window-delete-buffer ws 1)

          (should (equal (hash-table-count (windex-state-window-buffers ws)) 0))

          (should (eq (windex-state-window-head ws) nil))
          (should (eq (windex-state-window-tail ws) nil))
          (should (eq (windex-state-window-active ws) nil))))))

(ert-deftest windex-state-window-delete-buffer-non-active-node ()
  "Test `windex-state-window-delete` on non-active middle node."
  (windex-test-with-window-order ws 'last
    (windex-test-with-active-after-delete 'next
        (let (node1 node2 node3)
          (setq node1 (windex-state-window-add-buffer ws 1))
          (setq node2 (windex-state-window-add-buffer ws 2))
          (setq node3 (windex-state-window-add-buffer ws 3))
          (should (equal (hash-table-count (windex-state-window-buffers ws)) 3))

          (windex-state-window-set-active-buffer ws 3)
          (windex-state-window-delete-buffer ws 2)

          (should (equal (hash-table-count (windex-state-window-buffers ws)) 2))

          (should (eq (windex-state-window-head ws) node1))
          (should (eq (windex-state-window-tail ws) node3))
          (should (eq (windex-state-window-active ws) node3))

          (should (eq (windex-state-node-prev node1) nil))
          (should (eq (windex-state-node-next node1) node3))

          (should (eq (windex-state-node-prev node3) node1))
          (should (eq (windex-state-node-next node3) nil))))))

(ert-deftest windex-state-window-delete-buffer-non-active-head-node ()
  "Test `windex-state-window-delete` on non-active head node."
  (windex-test-with-window-order ws 'last
    (windex-test-with-active-after-delete 'next
        (let (node1 node2 node3)
          (setq node1 (windex-state-window-add-buffer ws 1))
          (setq node2 (windex-state-window-add-buffer ws 2))
          (setq node3 (windex-state-window-add-buffer ws 3))
          (should (equal (hash-table-count (windex-state-window-buffers ws)) 3))

          (windex-state-window-set-active-buffer ws 3)
          (windex-state-window-delete-buffer ws 1)

          (should (equal (hash-table-count (windex-state-window-buffers ws)) 2))

          (should (eq (windex-state-window-head ws) node2))
          (should (eq (windex-state-window-tail ws) node3))
          (should (eq (windex-state-window-active ws) node3))

          (should (eq (windex-state-node-prev node2) nil))
          (should (eq (windex-state-node-next node2) node3))

          (should (eq (windex-state-node-prev node3) node2))
          (should (eq (windex-state-node-next node3) nil))))))

(ert-deftest windex-state-window-delete-buffer-non-active-tail-node ()
  "Test `windex-state-window-delete` on non-active tail node."
  (windex-test-with-window-order ws 'last
    (windex-test-with-active-after-delete 'next
        (let (node1 node2 node3)
          (setq node1 (windex-state-window-add-buffer ws 1))
          (setq node2 (windex-state-window-add-buffer ws 2))
          (setq node3 (windex-state-window-add-buffer ws 3))
          (should (equal (hash-table-count (windex-state-window-buffers ws)) 3))

          (windex-state-window-set-active-buffer ws 1)
          (windex-state-window-delete-buffer ws 3)

          (should (equal (hash-table-count (windex-state-window-buffers ws)) 2))

          (should (eq (windex-state-window-head ws) node1))
          (should (eq (windex-state-window-tail ws) node2))
          (should (eq (windex-state-window-active ws) node1))

          (should (eq (windex-state-node-prev node1) nil))
          (should (eq (windex-state-node-next node1) node2))

          (should (eq (windex-state-node-prev node2) node1))
          (should (eq (windex-state-node-next node2) nil))))))

(ert-deftest windex-state-window-delete-buffer-with-next-rule-active-node ()
  "Test `windex-state-window-delete` on active middle node with `next rule."
  (windex-test-with-window-order ws 'last
    (windex-test-with-active-after-delete 'next
        (let (node1 node2 node3)
          (setq node1 (windex-state-window-add-buffer ws 1))
          (setq node2 (windex-state-window-add-buffer ws 2))
          (setq node3 (windex-state-window-add-buffer ws 3))
          (should (equal (hash-table-count (windex-state-window-buffers ws)) 3))

          (windex-state-window-set-active-buffer ws 2)
          (windex-state-window-delete-buffer ws 2)

          (should (equal (hash-table-count (windex-state-window-buffers ws)) 2))

          (should (eq (windex-state-window-head ws) node1))
          (should (eq (windex-state-window-tail ws) node3))
          (should (eq (windex-state-window-active ws) node3))

          (should (eq (windex-state-node-prev node1) nil))
          (should (eq (windex-state-node-next node1) node3))

          (should (eq (windex-state-node-prev node3) node1))
          (should (eq (windex-state-node-next node3) nil))))))

(ert-deftest windex-state-window-delete-with-next-rule-active-head-node ()
  "Test `windex-state-window-delete` on active head node with `next rule."
  (windex-test-with-window-order ws 'last
    (windex-test-with-active-after-delete 'next
        (let (node1 node2 node3)
          (setq node1 (windex-state-window-add-buffer ws 1))
          (setq node2 (windex-state-window-add-buffer ws 2))
          (setq node3 (windex-state-window-add-buffer ws 3))
          (should (equal (hash-table-count (windex-state-window-buffers ws)) 3))

          (windex-state-window-set-active-buffer ws 1)
          (windex-state-window-delete-buffer ws 1)

          (should (equal (hash-table-count (windex-state-window-buffers ws)) 2))

          (should (eq (windex-state-window-head ws) node2))
          (should (eq (windex-state-window-tail ws) node3))
          (should (eq (windex-state-window-active ws) node2))

          (should (eq (windex-state-node-prev node2) nil))
          (should (eq (windex-state-node-next node2) node3))

          (should (eq (windex-state-node-prev node3) node2))
          (should (eq (windex-state-node-next node3) nil))))))

(ert-deftest windex-state-window-delete-with-next-rule-active-tail-node ()
  "Test `windex-state-window-delete` on active tail node with `next rule."
  (windex-test-with-window-order ws 'last
    (windex-test-with-active-after-delete 'next
        (let (node1 node2 node3)
          (setq node1 (windex-state-window-add-buffer ws 1))
          (setq node2 (windex-state-window-add-buffer ws 2))
          (setq node3 (windex-state-window-add-buffer ws 3))
          (should (equal (hash-table-count (windex-state-window-buffers ws)) 3))

          (windex-state-window-set-active-buffer ws 3)
          (windex-state-window-delete-buffer ws 3)

          (should (equal (hash-table-count (windex-state-window-buffers ws)) 2))

          (should (eq (windex-state-window-head ws) node1))
          (should (eq (windex-state-window-tail ws) node2))
          (should (eq (windex-state-window-active ws) node1))

          (should (eq (windex-state-node-prev node1) nil))
          (should (eq (windex-state-node-next node1) node2))

          (should (eq (windex-state-node-prev node2) node1))
          (should (eq (windex-state-node-next node2) nil))))))

(ert-deftest windex-state-window-delete-with-prev-rule-active-node ()
  "Test `windex-state-window-delete` on active middle node with `previous rule."
  (windex-test-with-window-order ws 'last
    (windex-test-with-active-after-delete 'previous
        (let (node1 node2 node3)
          (setq node1 (windex-state-window-add-buffer ws 1))
          (setq node2 (windex-state-window-add-buffer ws 2))
          (setq node3 (windex-state-window-add-buffer ws 3))
          (should (equal (hash-table-count (windex-state-window-buffers ws)) 3))

          (windex-state-window-set-active-buffer ws 2)
          (windex-state-window-delete-buffer ws 2)

          (should (equal (hash-table-count (windex-state-window-buffers ws)) 2))

          (should (eq (windex-state-window-head ws) node1))
          (should (eq (windex-state-window-tail ws) node3))
          (should (eq (windex-state-window-active ws) node1))

          (should (eq (windex-state-node-prev node1) nil))
          (should (eq (windex-state-node-next node1) node3))

          (should (eq (windex-state-node-prev node3) node1))
          (should (eq (windex-state-node-next node3) nil))))))

(ert-deftest windex-state-window-delete-with-prev-rule-active-head-node ()
  "Test `windex-state-window-delete` on active head node with `previous rule."
  (windex-test-with-window-order ws 'last
    (windex-test-with-active-after-delete 'previous
        (let (node1 node2 node3)
          (setq node1 (windex-state-window-add-buffer ws 1))
          (setq node2 (windex-state-window-add-buffer ws 2))
          (setq node3 (windex-state-window-add-buffer ws 3))
          (should (equal (hash-table-count (windex-state-window-buffers ws)) 3))

          (windex-state-window-set-active-buffer ws 1)
          (windex-state-window-delete-buffer ws 1)

          (should (equal (hash-table-count (windex-state-window-buffers ws)) 2))

          (should (eq (windex-state-window-head ws) node2))
          (should (eq (windex-state-window-tail ws) node3))
          (should (eq (windex-state-window-active ws) node3))

          (should (eq (windex-state-node-prev node2) nil))
          (should (eq (windex-state-node-next node2) node3))

          (should (eq (windex-state-node-prev node3) node2))
          (should (eq (windex-state-node-next node3) nil))))))

(ert-deftest windex-state-window-delete-with-prev-rule-active-tail-node ()
  "Test `windex-state-window-delete` on active tail node with `previous rule."
  (windex-test-with-window-order ws 'last
    (windex-test-with-active-after-delete 'previous
        (let (node1 node2 node3)
          (setq node1 (windex-state-window-add-buffer ws 1))
          (setq node2 (windex-state-window-add-buffer ws 2))
          (setq node3 (windex-state-window-add-buffer ws 3))
          (should (equal (hash-table-count (windex-state-window-buffers ws)) 3))

          (windex-state-window-set-active-buffer ws 3)
          (windex-state-window-delete-buffer ws 3)

          (should (equal (hash-table-count (windex-state-window-buffers ws)) 2))

          (should (eq (windex-state-window-head ws) node1))
          (should (eq (windex-state-window-tail ws) node2))
          (should (eq (windex-state-window-active ws) node2))

          (should (eq (windex-state-node-prev node1) nil))
          (should (eq (windex-state-node-next node1) node2))

          (should (eq (windex-state-node-prev node2) node1))
          (should (eq (windex-state-node-next node2) nil))))))

(defmacro windex-test-with-test-state (windex-state &rest body)
  "Run BODY with test `windex-state`, WINDEX-STATE."
  (declare (indent 1))
  `(progn
     (let* ((,windex-state (make-windex-state :id "windex-test-state")))
       (unwind-protect
           ,@body
         ))))

(ert-deftest windex-state-buffer-single-window ()
  "Test `windex-state` add/delete buffer from single window."
  (windex-test-with-test-state windex-state
    (let* ((buffer1 (get-buffer-create "buffer-1"))
           (window1 "window-1")
           (windows (windex-state-windows windex-state))
           (buffers (windex-state-buffers windex-state))
           window1-state
           buffer1-windows)
      (windex-state-add-buffer windex-state window1 buffer1)
      (should (equal (hash-table-count windows) 1))
      (should (equal (hash-table-count buffers) 1))

      (setq window1-state (gethash window1 windows))
      (should (equal (hash-table-count (windex-state-window-buffers window1-state)) 1))

      (setq buffer1-windows (gethash buffer1 buffers))
      (should buffer1-windows)
      (should (gethash window1 buffer1-windows))

      (windex-state-remove-buffer windex-state window1 buffer1)
      (should (equal (hash-table-count windows) 1))
      (should (equal (hash-table-count buffers) 0))

      (should (equal (hash-table-count (windex-state-window-buffers window1-state)) 0))
      (setq buffer1-windows (gethash buffer1 buffers))
      (should-not buffer1-windows)
      )))

(ert-deftest windex-state-buffer-multiple-windows ()
  "Test `windex-state` add/delete buffer with multiple windows."
  (windex-test-with-test-state windex-state
    (let* ((buffer1 (get-buffer-create "buffer-1"))
           (window1 "window-1")
           (window2 "window-2")
           (windows (windex-state-windows windex-state))
           (buffers (windex-state-buffers windex-state))
           window1-state
           window2-state
           buffer1-windows)
      (windex-state-add-buffer windex-state window1 buffer1)
      (windex-state-add-buffer windex-state window2 buffer1)

      (setq window1-state (gethash window1 windows))
      (should (equal (hash-table-count (windex-state-window-buffers window1-state)) 1))

      (setq window2-state (gethash window2 windows))
      (should (equal (hash-table-count (windex-state-window-buffers window2-state)) 1))

      (setq buffer1-windows (gethash buffer1 buffers))
      (should buffer1-windows)
      (should (gethash window1 buffer1-windows))
      (should (gethash window2 buffer1-windows))

      (windex-state-remove-buffer windex-state window1 buffer1)
      (should (equal (hash-table-count windows) 2))
      (should (equal (hash-table-count buffers) 1))

      (should (equal (hash-table-count (windex-state-window-buffers window1-state)) 0))
      (should (equal (hash-table-count (windex-state-window-buffers window2-state)) 1))
      (setq buffer1-windows (gethash buffer1 buffers))
      (should buffer1-windows)
      (should-not (gethash window1 buffer1-windows))
      (should (gethash window2 buffer1-windows))
      )))

(ert-deftest windex-state-buffer-multiple-windows-by-id ()
  "Test `windex-state` add/delete buffer with multiple windows by ids."
  (windex-test-with-test-state windex-state
    (let* ((buffer1 (get-buffer-create "buffer-1"))
           (window1 "window-1")
           (window2 "window-2")
           (window3 "window-3")
           (windows (windex-state-windows windex-state))
           (buffers (windex-state-buffers windex-state))
           window1-state window2-state window3-state
           buffer1-windows)
      (windex-state-add-buffer windex-state window1 buffer1)
      (windex-state-add-buffer windex-state window2 buffer1)
      (windex-state-add-buffer windex-state window3 buffer1)

      (setq window1-state (gethash window1 windows))
      (should (equal (hash-table-count (windex-state-window-buffers window1-state)) 1))

      (setq window2-state (gethash window2 windows))
      (should (equal (hash-table-count (windex-state-window-buffers window2-state)) 1))

      (setq window3-state (gethash window3 windows))
      (should (equal (hash-table-count (windex-state-window-buffers window3-state)) 1))

      (setq buffer1-windows (gethash buffer1 buffers))
      (should buffer1-windows)
      (should (gethash window1 buffer1-windows))
      (should (gethash window2 buffer1-windows))
      (should (gethash window3 buffer1-windows))

      (windex-state-remove-buffer-from-windows
       windex-state
       buffer1
       (list window1))

      (should (equal (hash-table-count windows) 3))
      (should (equal (hash-table-count buffers) 1))

      (should (equal (hash-table-count (windex-state-window-buffers window1-state)) 0))
      (should (equal (hash-table-count (windex-state-window-buffers window2-state)) 1))
      (should (equal (hash-table-count (windex-state-window-buffers window3-state)) 1))
      (setq buffer1-windows (gethash buffer1 buffers))
      (should buffer1-windows)
      (should-not (gethash window1 buffer1-windows))
      (should (gethash window2 buffer1-windows))
      (should (gethash window3 buffer1-windows))
      )))

(ert-deftest windex-state-buffer-multiple-windows-by-id-all ()
  "Test `windex-state` add/delete buffer from all windows."
  (windex-test-with-test-state windex-state
    (let* ((buffer1 (get-buffer-create "buffer-1"))
           (window1 "window-1")
           (window2 "window-2")
           (window3 "window-3")
           (windows (windex-state-windows windex-state))
           (buffers (windex-state-buffers windex-state))
           window1-state window2-state window3-state
           buffer1-windows)
      (windex-state-add-buffer windex-state window1 buffer1)
      (windex-state-add-buffer windex-state window2 buffer1)
      (windex-state-add-buffer windex-state window3 buffer1)

      (setq window1-state (gethash window1 windows))
      (should (equal (hash-table-count (windex-state-window-buffers window1-state)) 1))

      (setq window2-state (gethash window2 windows))
      (should (equal (hash-table-count (windex-state-window-buffers window2-state)) 1))

      (setq window3-state (gethash window3 windows))
      (should (equal (hash-table-count (windex-state-window-buffers window3-state)) 1))

      (setq buffer1-windows (gethash buffer1 buffers))
      (should buffer1-windows)
      (should (gethash window1 buffer1-windows))
      (should (gethash window2 buffer1-windows))
      (should (gethash window3 buffer1-windows))

      (windex-state-remove-buffer-from-windows windex-state buffer1)
      (should (equal (hash-table-count windows) 3))
      (should (equal (hash-table-count buffers) 0))

      (should (equal (hash-table-count (windex-state-window-buffers window1-state)) 0))
      (should (equal (hash-table-count (windex-state-window-buffers window2-state)) 0))
      (should (equal (hash-table-count (windex-state-window-buffers window3-state)) 0))
      (setq buffer1-windows (gethash buffer1 buffers))
      (should-not buffer1-windows)
      )))

(ert-deftest windex-state-clear ()
  "Test `windex-state` clear."
  (windex-test-with-test-state windex-state
    (let* ((buffer1 (get-buffer-create "buffer-1"))
           (window1 "window-1")
           (window2 "window-2")
           (window3 "window-3")
           (windows (windex-state-windows windex-state))
           (buffers (windex-state-buffers windex-state)))

      (windex-state-add-buffer windex-state window1 buffer1)
      (windex-state-add-buffer windex-state window2 buffer1)
      (windex-state-add-buffer windex-state window3 buffer1)

      (should (equal (hash-table-count windows) 3))
      (setq buffer1-windows (gethash buffer1 buffers))
      (should (equal (hash-table-count buffer1-windows) 3))

      (windex-state-clear windex-state)
      (should (equal (hash-table-count windows) 0))
      (should (equal (hash-table-count buffers) 0))
      )))

;;; test-windex-state.el ends here
