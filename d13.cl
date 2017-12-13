(defun day-thirteen (input-file)
  (let ((layers (parse-input input-file)))
    (loop for wait = 0 then (1+ wait)
          for (trip-severity times-caught) = (run wait layers)
          for first-trip = trip-severity then (identity first-trip)
          until (zerop times-caught)
          finally (return (list first-trip wait)))))

(defun run (wait layers)
  (loop for (depth range) in layers
        summing (cost (+ wait depth) depth range) into trip-severity
        counting (caught-p (+ wait depth) range) into times-caught
        finally (return (list trip-severity times-caught))))

(defun parse-input (input-file)
  (with-open-file (f input-file :direction :input
                     :if-does-not-exist :error)
    (loop for line = (read-line f nil)
          while line
          collect (parse-line line))))

(defun parse-line (line)
  (with-open-stream (s (make-string-input-stream (substitute #\, #\: line)))
    (let ((depth (read s nil))
          (comma (read-char s nil))
          (range (read s nil)))
      (declare (ignore comma))
      (list depth range))))

(defun caught-p (timestamp range)
  (zerop (mod timestamp (* 2 (1- range)))))

(defun severity (depth range)
  (* depth range))

(defun cost (timestamp depth range)
  (if (caught-p timestamp range)
      (severity depth range)
      0))

(print (day-thirteen "d13-test.txt"))
(print (day-thirteen "d13.txt"))
nil
