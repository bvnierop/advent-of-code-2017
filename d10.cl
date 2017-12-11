(defun day-ten-part-one (input-file)
  (let* ((parsed-input (parse-file input-file))
         (len (first parsed-input))
         (steps (second parsed-input))
         (arr (build-array len)))
    (do-steps steps arr)
    (* (aref arr 0) (aref arr 1))))

(defun day-ten-part-two (input-file)
  (let* ((parsed-input (parse-file input-file #'parse-lengths-as-ascii))
         (len (first parsed-input))
         (steps (second parsed-input))
         (arr (build-array len)))
    (make-hash arr steps)))

(defun parse-file (input-file &optional (parse-lengths #'parse-lengths))
  (with-open-file (f input-file :direction :input
                     :if-does-not-exist :error)
    (list (read f) (funcall parse-lengths f))))

(defun parse-lengths (input-stream)
  (loop for len = (read input-stream nil)
        for skip-comma = (read-char input-stream nil)
        while len
        collect len))

(defun build-array (len)
  (make-array len :initial-contents (loop for i from 0 below len collect i)))

(defun do-steps (steps arr &optional (pos 0) (skp 0))
  ; (format t "~&(do-steps ~a arr ~a ~a~&" steps pos skp)
  (loop for stp in steps
        for index = pos then (+ index prvstep skip)
        for skip = skp then (1+ skip)
        for prvstep = stp
        do (do-step stp arr index)
        finally (return (values arr (+ index prvstep skip) (1+ skip)))))

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

(defun do-steps-often (times steps arr)
  ; (format t "~&(do-steps-often ~a ~a)~&" times steps)
  (loop for i from 0 below times
        for (hashed index skip) = (multiple-value-list (do-steps steps arr))
          then (multiple-value-list (do-steps steps hashed index skip))
        ; do (format t "~& arr: ~a~&" hashed)
        ; do (format t "~& index: ~a, skip: ~a~&" (array-index hashed index) skip)
        finally (return hashed)))

(defun parse-lengths-as-ascii (input-stream)
  (append (butlast (loop for chr = (read-char input-stream nil)
                         while chr
                         collect (char-code chr)))
          (list 17 31 73 47 23)))

(defun make-sparse-hash (arr)
  (loop for block-index from 0 below 256 by 16
        collect (apply #'logxor (array-range arr block-index 16))))

(defun make-hash (arr steps)
  (do-steps-often 64 steps arr)
  (format nil "~{~a~}" 
          (mapcar (lambda (num)
                    (string-downcase (format nil "~2,'0X" num)))
                  (make-sparse-hash arr))))

(defun array-index (arr index &optional (axis-number 0))
  (mod index (array-dimension arr axis-number)))

(defun array-range (arr index size)
  (loop for i from index below (+ index size)
        collect (aref arr i)))

(print (day-ten-part-one "d10-test.txt"))
(print (day-ten-part-one "d10.txt"))
(do-steps-often 2 (list 3 4 1 5) (build-array 5))
(print (day-ten-part-two "d10-test2.txt"))

;; Underscores are added because when reading from a file, there's a newline added
;; even when one does not exist. That newline is dropped by parse-lengths-as-ascii.
;; Here, it drops the underscore.
(print (make-hash (build-array 256) (parse-lengths-as-ascii (make-string-input-stream "_"))))
(print (make-hash (build-array 256) (parse-lengths-as-ascii (make-string-input-stream "AoC 2017_"))))
(print (make-hash (build-array 256) (parse-lengths-as-ascii (make-string-input-stream "1,2,3_"))))
(print (make-hash (build-array 256) (parse-lengths-as-ascii (make-string-input-stream "1,2,4_"))))
(print (day-ten-part-two "d10.txt"))
nil
