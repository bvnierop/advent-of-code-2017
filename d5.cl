(defstruct maze arr ptr)

(defun in-maze-p (maze)
  (and (<= 0 (maze-ptr maze)) (< (maze-ptr maze) (array-dimension (maze-arr maze) 0))))

(defun day-five-part-one (input-file)
  (solve input-file #'increment-jump))

(defun solve (input-file update-jump-fn &optional dbg)
  (let* ((instruction-set (parse-file input-file))
         (maze (make-maze :arr instruction-set :ptr 0)))
    (loop for p = (maze-ptr maze) then (maze-ptr maze)
          for c = 0 then (1+ c)
          while (in-maze-p maze)
          do (execute-instruction maze :update-jump-fn update-jump-fn :dbg dbg)
          finally (return c))))

(defun execute-instruction (maze &key update-jump-fn dbg)
  (let* ((ptr (maze-ptr maze))
         (jump (aref (maze-arr maze) ptr)))
    (incf (maze-ptr maze) jump)
    (funcall update-jump-fn maze ptr)
    (when dbg
      (print maze))))

(defun increment-jump (maze ptr)
  (incf (aref (maze-arr maze) ptr)))

(defun parse-file (input-file)
  (with-open-file (f input-file
                     :direction :input
                     :if-does-not-exist :error)
    (let ((instructions (loop for i = (read f nil)
                              while i
                              collect i)))
      (make-array (length instructions) :initial-contents instructions))))

(defun day-five-part-two (input-file)
  (solve input-file #'strange-jump-update))

(defun strange-jump-update (maze ptr)
  (let ((jump (aref (maze-arr maze) ptr)))
    (if (<= 3 jump)
        (decf (aref (maze-arr maze) ptr))
        (incf (aref (maze-arr maze) ptr)))))

(assert (= (day-five-part-one "d5-test.txt") 5))
(assert (= (day-five-part-two "d5-test.txt") 10))

(print (day-five-part-one "d5.txt"))
(print (day-five-part-two "d5.txt"))
nil
