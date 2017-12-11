(defun day-eleven (input-file)
  (let* ((directions (parse-file input-file))
         (result (walk-path directions))
         (final-coords (first result))
         (max-distance (second result)))
    (list (distance final-coords) max-distance)))

(defun parse-file (input-file)
  (with-open-file (f input-file :direction :input
                     :if-does-not-exist :error)
    (loop for direction = (read f nil)
          while direction
          do (read-char f nil) ; swallow comma
          collect direction)))

(defun walk-path (path)
  (let ((pos (list 0 0))
        (max-distance 0))
    (dolist (direction path (list pos max-distance))
      (setf pos (take-step pos direction))
      (setf max-distance (max max-distance (distance pos))))))


(defun take-step (current-coords direction)
  (case direction
    (n  (list (+ (first current-coords) 0) (+ (second current-coords) 2)))
    (s  (list (+ (first current-coords) 0) (+ (second current-coords) -2)))
    (ne (list (+ (first current-coords) 1) (+ (second current-coords) 1)))
    (se (list (+ (first current-coords) 1) (+ (second current-coords) -1)))
    (nw (list (+ (first current-coords) -1) (+ (second current-coords) 1)))
    (sw (list (+ (first current-coords) -1) (+ (second current-coords) -1)))))

(defun distance (coords)
  (let ((x (abs (first coords)))
         (y (abs (second coords))))
    (+ x
      (/ (max 0 (- y x)) 2))))

(print (day-eleven "d11-test.txt"))
(print (day-eleven "d11.txt"))
nil
