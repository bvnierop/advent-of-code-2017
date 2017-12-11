(defun day-eight-part-one (input-file)
  (let ((instructions (parse-file input-file))
        (hash (make-hash-table)))
    (dolist (instruction instructions (max-value hash))
      (funcall (first instruction) hash (second instruction)))))

(defun day-eight-part-two (input-file)
  (let ((instructions (parse-file input-file))
        (highest 0)
        (hash (make-hash-table)))
    (dolist (instruction instructions highest)
      (let ((value (funcall (first instruction) hash (second instruction))))
        (when value (setf highest (max highest value)))))))

(defun parse-file (input-file)
  (with-open-file (f input-file :direction :input
                     :if-does-not-exist :error)
    (loop for line = (read-line f nil)
          while line
          collect (parse-line line))))

(defun parse-line (line)
  (with-open-stream (s (make-string-input-stream line))
    (let ((reg (read s))
          (op (read s))
          (amt (read s))
          (_ (read s)) ;; swallow if
          (cond-reg (read s))
          (cond-op (read s))
          (cond-val (read s)))
      (declare (ignore _))
      (list 
        (make-register-op reg op amt)
        (make-conditional-op cond-reg cond-op cond-val)))))

(defun make-register-op (reg op amt)
  (lambda (hash conditional)
    (when (funcall conditional hash)
      (if (eq op 'inc)
          (incf (gethash reg hash 0) amt)
          (decf (gethash reg hash 0) amt)))))

(defun make-conditional-op (reg op val)
  (lambda (hash)
    (funcall op (gethash reg hash 0) val)))

(defun == (num &rest more)
  (apply #'= num more))

(defun != (num &rest more)
  (not (apply #'= num more)))

(defun max-value (hash)
  (loop for key being the hash-keys in hash
        using (hash-value value)
        maximizing value))

(print (day-eight-part-one "d8-test.txt"))
(print (day-eight-part-one "d8.txt"))
(print (day-eight-part-two "d8-test.txt"))
(print (day-eight-part-two "d8.txt"))
nil
