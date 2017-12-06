(defun day-six-part-one (input-file)
  (solve input-file (lambda (hash banks cycle)
                      (declare (ignore hash banks))
                      cycle)))

(defun solve (input-file return-fn)
  (let ((banks (parse-input-file input-file))
        (hash (make-hash-table :test #'equal)))
    (loop 
      for c = 0 then (1+ c)
      while (not (seen-before-p hash banks))
      do (progn
           (seen hash banks c)
           (redistribute banks))
      finally (return (funcall return-fn hash banks c)))))


(defun parse-input-file (input-file)
  (with-open-file (f input-file 
                     :direction :input
                     :if-does-not-exist :error)
    (let ((banks (loop for i = (read f nil)
                       while i
                       collect i)))
      (make-array (length banks) :initial-contents banks))))

(defun redistribute (banks)
  (let* ((hi (apply #'max (array-as-list banks)))
        (idx (position hi banks)))
    (setf (aref banks idx) 0)
    (distribute-memory banks hi idx)))

(defun distribute-memory (banks hi idx)
  (let ((len (array-dimension banks 0)))
    (multiple-value-bind (all remaining) (truncate hi len)
      (loop for i from 0 below len do (incf (aref banks i) all))
      (loop for i from 0 below remaining do (incf (aref banks (rem (+ (1+ idx) i) len))))
      banks)))

(defun array-as-list (arr)
  (map 'list #'identity arr))

(defun seen-before-p (hash banks)
  (gethash (array-as-list banks) hash nil))

(defun seen (hash banks cycle)
  (setf (gethash (array-as-list banks) hash nil) cycle))

(defun day-six-part-two (input-file)
  (solve input-file (lambda (hash banks cycle)
                      (- cycle (gethash (array-as-list banks) hash)))))

(assert (= (day-six-part-one "day-six-test.txt") 5))
(assert (= (day-six-part-two "day-six-test.txt") 4))

(print (day-six-part-one "day-six.txt"))
(print (day-six-part-two "day-six.txt"))
nil
