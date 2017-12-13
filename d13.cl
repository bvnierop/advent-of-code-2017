(defun day-thirteen (input-file)
  (let* ((input (parse-input input-file))
         (len (first input))
         (layers (second input)))
    (loop for wait = 0 then (1+ wait)
          for (trip-severity times-caught) = (run wait len layers)
          for first-trip = trip-severity then (identity first-trip)
          ; do (format t "~&start at: ~a, end at: ~a~&" wait (+ wait len))
          ; do (format t "~&trip-severity: ~a times-caught: ~a~&" trip-severity times-caught)
          until (zerop times-caught)
          finally (return (list first-trip wait)))))

(defun run (wait len layers)
  (loop for timestamp from wait to (+ wait len)
        summing (cost timestamp (- timestamp wait) layers) into trip-severity
        counting (caught-p timestamp (- timestamp wait) layers) into times-caught
        finally (return (list trip-severity times-caught))))

(defun parse-input (input-file)
  (with-open-file (f input-file :direction :input
                     :if-does-not-exist :error)
    (let ((layers (make-hash-table)))
          (loop for line = (read-line f nil)
                while line
                for parsed = (parse-line line)
                maximize (first parsed) into end
                do (setf (gethash (first parsed) layers) (second parsed))
                finally (return (list end layers))))))
          
(defun parse-line (line)
  (with-open-stream (s (make-string-input-stream (substitute #\, #\: line)))
    (let ((depth (read s nil))
          (comma (read-char s nil))
          (range (read s nil)))
      (declare (ignore comma))
      (list depth range))))

(defun scanner-position (timestamp range)
  (multiple-value-bind (cycles remainder) (truncate timestamp (1- range))
    (if (evenp cycles)
        remainder
        (- range 1 remainder))))

(defun caught-p (timestamp layer layers)
  (let ((range (gethash layer layers)))
    (and range (zerop (scanner-position timestamp range)))))

(defun severity (layer layers)
  (* (gethash layer layers 0) layer))

(defun cost (timestamp layer layers)
  (if (caught-p timestamp layer layers)
      (severity layer layers)
      0))

(print (day-thirteen "d13-test.txt"))
(print (day-thirteen "d13.txt"))
nil
