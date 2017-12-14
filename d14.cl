(defun day-fourteen (input)
  (let ((inputs (loop for i from 0 below 128
                      for id = (concatenate 'string input "-" (write-to-string i))
                      collect id)))
    (apply #'+
           (mapcar #'count-bits
                   (flatten (mapcar #'make-hash
                                    inputs))))))

(defun make-hash (str)
  (let ((arr (build-array 256))
        (steps (build-steps (make-string-input-stream str))))
    (do-steps-often 64 steps arr)
    (make-sparse-hash arr)))

(defun build-array (len)
  (make-array len :initial-contents (loop for i from 0 below len collect i)))

(defun build-steps (input-stream)
  (append (loop for chr = (read-char input-stream nil)
                while chr
                collect (char-code chr))
          (list 17 31 73 47 23)))

(defun do-steps-often (times steps arr)
  (loop for i from 0 below times
        for (hashed index skip) = (multiple-value-list (do-steps steps arr))
          then (multiple-value-list (do-steps steps hashed index skip))
        finally (return hashed)))

(defun do-steps (steps arr &optional (pos 0) (skp 0))
  (loop for stp in steps
        for index = pos then (+ index prvstep skip)
        for skip = skp then (1+ skip)
        for prvstep = stp
        do (do-step stp arr index)
        finally (return (values arr (+ index prvstep skip) (1+ skip)))))

(defun do-step (step-size arr index)
  (loop for i from 0 below (truncate step-size 2)
        with end = (1- (+ index step-size))
        for x-index = (array-index arr (+ index i))
        for y-index = (array-index arr (- end i))
        for x = (aref arr x-index)
        for y = (aref arr y-index)
        do (setf (aref arr x-index) y)
        do (setf (aref arr y-index) x))
  arr)

(defun make-sparse-hash (arr)
  (loop for block-index from 0 below 256 by 16
        collect (apply #'logxor (array-range arr block-index 16))))

(defun array-index (arr index &optional (axis-number 0))
  (mod index (array-dimension arr axis-number)))

(defun array-range (arr index size)
  (loop for i from index below (+ index size)
        collect (aref arr i)))

(defun count-bits (int)
  (loop for i from 0 to (integer-length int)
        counting (logbitp i int)))

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

(print (make-hash ""))
nil

(print (day-fourteen "flqrgnkx"))
(print (day-fourteen "nbysizxe"))
nil
