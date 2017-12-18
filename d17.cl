(defun day-seventeen-part-one (steps skip-size)
  (let ((spinlock (vector 0)))
    (loop for i from 1 to steps
          for current-position = 0 then (mod (+ 1 current-position skip-size) i)
          do (setf spinlock (insert-after 'vector current-position i spinlock))
          finally (return (part-one-value spinlock (1+ i) (1+ current-position))))))

(defun day-seventeen-part-two (steps skip-size)
  (let ((value-after-zero nil))
    (loop for i from 1 to steps
          for current-position = 0 then (mod (+ 1 current-position skip-size) i)
          when (zerop current-position) maximize i)))

(defun insert-before (result-type index element seq)
  (concatenate result-type (subseq seq 0 index) (vector element) (subseq seq index)))

(defun insert-after (result-type index element seq)
  (insert-before result-type (1+ index) element seq))

(defun part-one-value (spinlock len last-position)
  (aref spinlock (mod (1+ last-position) len)))

(print (day-seventeen-part-one 2017 3))
(print (day-seventeen-part-one 2017 386))
(print (day-seventeen-part-two 9 3))
(print (day-seventeen-part-two 50000000 386))
nil
