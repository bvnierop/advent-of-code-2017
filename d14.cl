(defun day-fourteen-part-one (input)
  (apply #'+ (mapcar #'count-bits
                     (flatten (make-disk input)))))

(defun day-fourteen-part-two (input)
  (let ((disk (make-array (list 128 16) :initial-contents (make-disk input))))
    (count-regions disk)))

(defun count-regions (disk)
  (let ((seen (make-hash-table)))
    (loop for row from 0 below 128
          sum (loop for column from 0 below 128
                    when (used-p row column disk)
                      counting (discover-region disk row column seen)))))

(defun wat (disk)
  (loop for row from 0 below 128
        sum (loop for column from 0 below 128
                  counting (used-p row column disk))))

(defun cell-id (row column)
  (+ (* row 1000) column))

(defun discover-region (disk row column seen)
  (unless (gethash (cell-id row column) seen)
    (setf (gethash (cell-id row column) seen) t)
    (loop for offset in '((1 0) (-1 0) (0 1) (0 -1))
          for new-row = (+ row (first offset))
          for new-column = (+ column (second offset))
          when (and (>= new-row 0) (>= new-column 0) (< new-row 128) (< new-column 128)
                    (used-p new-row new-column disk))
            do (discover-region disk new-row new-column seen)
          finally (return t))))

(defun make-disk (key-string)
  (let ((row-keys (loop for i from 0 below 128
                        for id = (concatenate 'string key-string "-" (write-to-string i))
                        collect id)))
    (mapcar #'make-hash row-keys)))

(defun used-p (row column disk)
  (multiple-value-bind (part-index bit-index) (truncate column 8)
    (let ((part (aref disk row part-index)))
      (logbitp (- 8 bit-index 1) part))))

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

(print (day-fourteen-part-one "flqrgnkx"))
(print (day-fourteen-part-two "flqrgnkx"))
(print (day-fourteen-part-one "nbysizxe"))
(print (day-fourteen-part-two "nbysizxe"))
nil
