(defun day-seven-part-one (tree program-lookup)
  (declare (ignore program-lookup))
  (first tree))

(defun day-seven-part-two (tree program-lookup)
  (let ((w (memoize #'weight)))
    (labels ((walk-tree (program cur)
               (if (null program)
                   cur
                   (multiple-value-bind (res weights uniq) (unbalanced-p program program-lookup w)
                     (if res
                         (let ((ans (required-weight weights 
                                                     (nth uniq
                                                          (program-desc-children
                                                            (gethash program program-lookup)))
                                                     program-lookup))
                               (children (program-desc-children (gethash program program-lookup))))
                           (if (null children)
                               ans
                               (loop for child in (program-desc-children (gethash program program-lookup))
                                     for walked = (walk-tree child ans) then (walk-tree child walked)
                                     do (when walked (return walked))
                                     finally (return walked))))
                         (let ((children (program-desc-children (gethash program program-lookup))))
                           (if (null children)
                               cur
                               (loop for child in children
                                     for walked = (walk-tree child cur) then (walk-tree child walked)
                                     do (when walked (return walked))
                                     finally (return walked)))))))))

      (walk-tree (first tree) 0))))


(defun unbalanced-p (program program-lookup weight-fn)
  (let* ((children (program-desc-children (gethash program program-lookup)))
         (weights (mapcar (lambda (child) (funcall weight-fn child program-lookup)) children))
         (filtered-weights (remove-if #'null (remove-duplicates weights))))
    (if (or (null filtered-weights) (= (length filtered-weights) 1))
        (values nil weights nil)
        (values t weights (find-unique-element weights)))))

(defun weight (program program-lookup)
  (if (null program)
      0
      (let ((weights (list
                       (program-desc-weight (gethash program program-lookup))
                       (mapcar (lambda (child)
                                 (weight child program-lookup))
                               (program-desc-children (gethash program program-lookup))))))
        (apply #'+ (remove-if #'null (flatten weights))))))

(defun required-weight (sibling-weights program program-lookup)
  (let ((diff (abs (apply #'- (remove-duplicates sibling-weights)))))
    (- (program-desc-weight (gethash program program-lookup)) diff)))

(defun solve (input-file fn)
  (let* ((programs (parse-input-file input-file))
         (program-lookup (make-program-lookup programs))
         (subtrees (make-hash-table)))
    (loop for program in programs
          do (build-partial-tree program program-lookup subtrees))

    (let ((lengths (mapcar (lambda (program)
                             (let ((name (program-desc-name program)))
                               (list name (length (flatten (gethash name subtrees))))))
                           programs)))
      (funcall fn
               (gethash (first (first (sort lengths #'> :key (lambda (l) (second l))))) subtrees)
               program-lookup))))

(defun make-program-lookup (programs)
  (let ((lookup (make-hash-table)))
    (loop for program in programs
          do (setf (gethash (program-desc-name program) lookup) program))
    lookup))

(defun build-partial-tree (program program-lookup subtrees)
  (let* ((name (program-desc-name program))
         (subtree (gethash name subtrees nil)))
    (if subtree
        subtree
        (setf (gethash name subtrees)
              (list (program-desc-name program)
                    (mapcar (lambda (child)
                              (build-partial-tree (gethash child program-lookup)
                                                  program-lookup
                                                  subtrees))
                            (program-desc-children program)))))))

(defstruct program-desc name weight children)

(defun parse-input-file (input-file)
  (with-open-file (f input-file 
                     :direction :input
                     :if-does-not-exist :error)
    (loop for line = (read-line f nil)
          while line
          collect (parse-line line))))

(defun parse-line (line)
  (with-open-stream (s (make-string-input-stream line))
    (let ((name (read s))
          (open-bracket (read-char s))
          (weight (read s))
          (close-bracket (read-char s))
          (children-string (read-line s nil)))
      (declare (ignore open-bracket close-bracket))
      (make-program-desc :name name :weight weight :children (parse-children children-string)))))

(defun parse-children (line)
  (when line
    (with-open-stream (s (make-string-input-stream line))
      (loop for i from 1 to 4 do (read-char s)) ; swallow ->
      (loop for child = (read s nil)
            for comma = (read-char s nil) ; swallow ,
            while child
            collect child))))

(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    (lambda (&rest args)
      (let ((cached (gethash args cache)))
        (if cached
            cached
            (let ((result (apply fn args)))
              (setf (gethash args cache) result)))))))

(defun flatten (lst)
  (labels ((mklist (x)
             (if (listp x)
                 x 
                 (list x))))
    (mapcan #'(lambda (x)
                (if (atom x)
                    (mklist x)
                    (flatten x)))
            lst)))

(defun find-unique-element (lst &key (test #'eql))
  (let ((hash (make-hash-table :test test)))
    (loop for elem in lst
          do (incf (gethash elem hash 0))
          finally (return (loop for key being the hash-keys of hash
                                using (hash-value value)
                                until (= value 1)
                                finally (return (values (position key lst :test test) key)))))))

(assert (eql (solve "d7-test.txt" #'day-seven-part-one) 'tknk))
(assert (eql (solve "d7-test.txt" #'day-seven-part-two) 60))

(print (solve "d7.txt" #'day-seven-part-one))
(print (solve "d7.txt" #'day-seven-part-two))
nil
