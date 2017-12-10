(defun day-ten (input-file)
  (let* ((parsed-input (parse-file input-file))
         (len (first parsed-input))
         (steps (second parsed-input))
         (arr (build-array len)))
    (do-steps steps arr)
    (* (aref arr 0) (aref arr 1))))

(defun parse-file (input-file)
  (with-open-file (f input-file :direction :input
                     :if-does-not-exist :error)
    (list (read f) (parse-lengths f))))

(defun parse-lengths (input-stream)
  (loop for len = (read input-stream nil)
        for skip-comma = (read-char input-stream nil)
        while len
        collect len))

(defun build-array (len)
  (make-array len :initial-contents (loop for i from 0 below len collect i)))

(defun do-steps (steps arr)
  (labels ((do-steps-recursive (remaining-steps index skip-size)
             ; (format t "~&(do-steps-recursive ~a ~a ~a)~&" remaining-steps index skip-size)
             (if (null remaining-steps)
                 arr
                 (let ((next-step (first remaining-steps)))
                   (do-step next-step arr index)
                   (do-steps-recursive
                     (rest remaining-steps)
                     (+ index next-step skip-size)
                     (1+ skip-size))))))
    (do-steps-recursive steps 0 0)))

(defun do-step (step-size arr index)
  ; (format t "~&(do-step ~a ~a ~a)~&" step-size arr index)
  (loop for i from 0 below (truncate step-size 2)
        with end = (1- (+ index step-size))
        for x-index = (array-index arr (+ index i))
        for y-index = (array-index arr (- end i))
        for x = (aref arr x-index)
        for y = (aref arr y-index)
        ; do (format t "~&  x-index: ~a, y-index: ~a~&" x-index y-index)
        do (setf (aref arr x-index) y)
        do (setf (aref arr y-index) x))
  arr)

(defun array-index (arr index &optional (axis-number 0))
  (mod index (array-dimension arr axis-number)))

(print (day-ten "d10-test.txt"))
(print (day-ten "d10.txt"))
nil
