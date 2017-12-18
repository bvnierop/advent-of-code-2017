(defun day-eighteen (input-file)
  (run-duet (parse-input input-file)))


(defun run-duet (instructions)
  (let ((registers (make-hash-table)))
    (labels ((run-duet-recursive (instruction-pointer)
               (if (>= instruction-pointer (length instructions))
                   (gethash 'rcv registers)
                   (run-duet-recursive
                     (+ instruction-pointer
                        (perform-instruction (elt instructions instruction-pointer) registers))))))
      (run-duet-recursive 0))))

(defun perform-instruction (instruction registers)
  (let ((operands (rest instruction)))
    (case (first instruction)
      (snd (snd-instruction operands registers))
      (set (set-instruction operands registers))
      (add (add-instruction operands registers))
      (mul (mul-instruction operands registers))
      (mod (mod-instruction operands registers))
      (rcv (rcv-instruction operands registers))
      (jgz (jgz-instruction operands registers)))))

(defun snd-instruction (operands registers)
  (setf (gethash 'ps registers) (make-value (first operands) registers))
  1)

(defun set-instruction (operands registers)
  (setf (gethash (first operands) registers) (make-value (second operands) registers))
  1)

(defun add-instruction (operands registers)
  (incf (gethash (first operands) registers) (make-value (second operands) registers))
  1)

(defun mul-instruction (operands registers)
  (setf (gethash (first operands) registers)
        (* (make-value (first operands) registers)
           (make-value (second operands) registers)))
  1)

(defun mod-instruction (operands registers)
  (setf (gethash (first operands) registers)
        (mod (make-value (first operands) registers)
           (make-value (second operands) registers)))
  1)

(defun rcv-instruction (operands registers)
  (let ((operand (make-value (first operands) registers)))
    (if (zerop operand)
        1
        (progn
          (setf (gethash 'rcv registers) (gethash 'ps registers))
          1000))))

(defun jgz-instruction (operands registers)
  (if (zerop (make-value (first operands) registers))
      1
      (make-value (second operands) registers)))

(defun make-value (operand registers)
  (if (numberp operand)
      operand
      (gethash operand registers 0)))

(defun parse-input (input-file)
  (with-open-file (f input-file :direction :input
                     :if-does-not-exist :error)
    (apply #'vector
           (loop for instruction = (parse-instruction f)
                 while instruction
                 collect instruction))))

(defun parse-instruction (fstream)
  (let ((opcode (read fstream nil)))
    (when opcode
      (cons opcode (parse-operands fstream opcode)))))

(defun parse-operands (fstream opcode)
  (let ((first-operand (read fstream)))
    (if (or (eq opcode 'snd) (eq opcode 'rcv))
        (list first-operand)
        (list first-operand (read fstream)))))

(print (day-eighteen "d18-test.txt"))
(print (day-eighteen "d18.txt"))
nil
