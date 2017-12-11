(defun day-nine (input-file)
  (with-open-file (f input-file :direction :input
                     :if-does-not-exist :error)
    (read-char f) ; swallow first {
    (parse-group f 1)))

(defun parse-group (token-stream depth &optional (current-group-score 0) (current-garbage-score 0))
  (loop for next = (read-char token-stream nil)
        until (char= next #\}) ; stop when group ends
        if (char= next #\{) do (let ((scores (parse-group token-stream (1+ depth)
                                                  current-group-score current-garbage-score)))
                                 (setf current-group-score (first scores))
                                 (setf current-garbage-score (second scores)))
        if (char= next #\<) do (incf current-garbage-score (parse-garbage token-stream))
        finally (return (list (+ depth current-group-score) current-garbage-score))))

(defun parse-garbage (token-stream)
  (loop for next = (read-char token-stream nil)
        until  (char= next #\>) ; stop when garbage ends
        if (char= next #\!) do (read-char token-stream) ; ignore the next character
        count (count-char-p next)))

(defun count-char-p (chr)
  (not (find chr "!>")))

(print (day-nine "d9-test.txt"))
(print (day-nine "d9.txt"))
nil
