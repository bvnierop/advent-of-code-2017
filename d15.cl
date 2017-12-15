(defun day-fifteen (input-file predicate-a predicate-b limit)
  (let ((inputs (parse-input input-file)))
    (judge (make-generator (first inputs) 16807 predicate-a)
            (make-generator (second inputs) 48271 predicate-b)
            limit)))

(defun judge (gen-a gen-b limit)
  (loop for i from 0 below limit
        counting (= (logand (funcall gen-a) #xffff)
                    (logand (funcall gen-b) #xffff))))

(defun make-generator (starts-with mul predicate)
  (let ((cur starts-with)
        (div (1- (ash 1 31))))
    (lambda ()
      (loop until (funcall predicate (setf cur (rem (* cur mul) div)))
            finally (return cur)))))

(defun parse-input (input-file)
  (with-open-file (f input-file :direction :input
                     :if-does-not-exist :error)
    (loop for line = (read-line f nil)
          while line
          collect (parse-line line))))

(defun parse-line (line)
  (with-open-stream (s (make-string-input-stream line))
    (loop for i from 0 below 4 do (read s)) ; ignore garbage
    (read s)))

(print (day-fifteen "d15-test.txt" #'identity #'identity 40000000))
(print (day-fifteen "d15.txt" #'identity #'identity 40000000))
(print (day-fifteen "d15-test.txt"
                    (lambda (n) (zerop (rem n 4)))
                    (lambda (n) (zerop (rem n 8)))
                    5000000))
(print (day-fifteen "d15.txt"
                    (lambda (n) (zerop (rem n 4)))
                    (lambda (n) (zerop (rem n 8)))
                    5000000))
nil
