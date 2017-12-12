(defun day-twelve (input-file)
  (let* ((programs (parse-file input-file))
         (adjacency-list (build-adjacency-list programs))
         (seen (make-hash-table)))
    (mark-connecting-nodes-seen 0 adjacency-list seen)
    (list (hash-table-count seen)
          (count-groups programs adjacency-list (make-hash-table)))))

(defun parse-file (input-file)
  (with-open-file (f input-file :direction :input
                     :if-does-not-exist :error)
    (loop for line = (read-line f nil)
          while line
          collect (parse-line line))))

(defun parse-line (line)
  (with-open-stream (s (make-string-input-stream line))
    (let ((program-id (read s))
          (skip (repeat 3 #'read-char s nil))
          (children (loop for child = (read s nil)
                          for comma = (read-char s nil)
                          while child
                          collect child)))
      (declare (ignore skip))
      (list program-id children))))

(defun build-adjacency-list (nodes)
  (let ((lookup (make-hash-table)))
    (loop for node in nodes
          do (setf (gethash (first node) lookup) (second node))
          finally (return lookup))))

(defun repeat (times fn &rest args)
  (loop for i from 0 below times
        for res = (apply fn args)
        finally (return res)))

(defun mark-connecting-nodes-seen (node-id adjacency-list &optional (seen (make-hash-table)))
  (labels ((dfs (current-node-id)
             (unless (gethash current-node-id seen)
               (setf (gethash current-node-id seen) t)
               (let ((children (gethash current-node-id adjacency-list nil)))
                 (dolist (child children) (dfs child))))))
    (dfs node-id)))

(defun count-groups (programs adjacency-list seen)
  (loop for program in programs
        for program-id = (first program)
        with group-count = 0
        unless (gethash program-id seen)
        do (progn
             (mark-connecting-nodes-seen program-id adjacency-list seen)
             (incf group-count))
        finally (return group-count)))

(print (day-twelve "d12-test.txt"))
(print (day-twelve "d12.txt"))
nil

