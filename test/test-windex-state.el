;;; test-windex-state.el --- Tests for windex-state -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'windex-state)
(require 'ert)

(defmacro windex-test-with-test-state (&rest body)
  "Run BODY with test `windex-state`."
  (declare (indent 0))
  `(progn
     (let* ((old-state windex-state))
       (setq windex-state (make-hash-table :test #'equal))
       ,@body
       (setq windex-state old-state)
       )))

(ert-deftest windex-state ()
  "Test `windex-state`create/get/delete operations."
  (windex-test-with-test-state
    (let ((workspace1 "test-workspace-1")
          (workspace2 "test-workspace-2"))
      (should (hash-table-empty-p windex-state))
      (windex-state-create workspace1)
      (windex-state-create workspace2)
      (should (windex-state-workspace-p (windex-state-get workspace1)))
      (should (windex-state-workspace-p (windex-state-get workspace2)))
      (windex-state-delete workspace1)
      (should-not (windex-state-workspace-p (gethash workspace1 windex-state)))
      (should (windex-state-workspace-p (gethash workspace2 windex-state)))
      (windex-state-delete-all)
      (should (hash-table-empty-p windex-state)))))

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

(ert-deftest windex-state-workspace-create-new-window ()
  "Test `windex-state-workspace-create-window` with new window."
  (let ((ws (make-windex-state-workspace))
        (window-id "test-window")
        window)
    (should-not (windex-state-workspace-get-window ws window-id))
    (setq window (windex-state-workspace-create-window ws window-id))
    (should (windex-state-workspace-get-window ws window-id))))

(ert-deftest windex-state-workspace-create-existing-window ()
  "Test `windex-state-workspace-create-window` with existing window."
  (let ((ws (make-windex-state-workspace))
        (window-id "test-window")
        existing-window
        window)
    (setq existing-window (windex-state-workspace-create-window ws window-id))
    (should (windex-state-workspace-get-window ws window-id))
    (setq window (windex-state-workspace-create-window ws window-id))
    (should (eq existing-window window))))

(ert-deftest window-state-workspace-delete-window ()
  "Test `windex-state-workspace-delete-window`."
  (let ((ws (make-windex-state-workspace))
        (window-id "test-window")
        (window-id-to-delete "test-delete-window"))
    (windex-state-workspace-create-window ws window-id)
    (windex-state-workspace-create-window ws window-id-to-delete)
    (should (equal (hash-table-count (windex-state-workspace-windows ws)) 2))
    (windex-state-workspace-delete-window ws window-id-to-delete)
    (should (equal (hash-table-count (windex-state-workspace-windows ws)) 1))
    (should-not (windex-state-workspace-get-window ws window-id-to-delete))
    (should (windex-state-workspace-get-window ws window-id))))

(ert-deftest window-state-workspace-clear ()
  "Test `windex-state-workspace-clear`."
  (let ((ws (make-windex-state-workspace)))
    (windex-state-workspace-create-window ws "window1")
    (windex-state-workspace-create-window ws "window2")
    (windex-state-workspace-create-window ws "window3")
    (should (equal (hash-table-count (windex-state-workspace-windows ws)) 3))
    (windex-state-workspace-clear ws)
    (should (equal (hash-table-count (windex-state-workspace-windows ws)) 0))))

;;; test-windex-state.el ends here
