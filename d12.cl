(defun day-twelve (input-file)
  (let ((adjacency-list (build-adjacency-list (parse-file input-file))))
    (count-nodes-from 0 adjacency-list)))

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

(defun count-nodes-from (node-id adjacency-list)
  (let ((seen (make-hash-table)))
    (labels ((dfs (current-node-id)
              (unless (gethash current-node-id seen)
                (setf (gethash current-node-id seen) t)
                (let ((children (gethash current-node-id adjacency-list nil)))
                  (dolist (child children) (dfs child))))))
    (dfs node-id))
    (hash-table-count seen)))

(print (day-twelve "d12-test.txt"))
(print (day-twelve "d12.txt"))
nil

