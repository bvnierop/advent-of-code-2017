(defun day-twenty (input-file)
  (let* ((particles (read-file input-file))
         (smallest (apply #'min (mapcar #'particle-acceleration-sum particles)))
         (collection-of-smallest (remove-if-not (lambda (particle)
                                                  (= (particle-acceleration-sum particle) smallest))
                                                particles)))
    (mapcar #'particle-velocity-sum collection-of-smallest)
    (position '((-1015 -3583 -651) (-36 -105 -25) (0 -2 0))
              particles
              :test #'equal)))

(defun particle-acceleration-sum (particle)
  (apply #'+ (mapcar #'abs (third particle))))

(defun particle-velocity-sum (particle)
  (apply #'+ (mapcar #'abs (second particle))))

(defun particle-distance-from-origin (particle)
  (apply #'+ (mapcar #'abs (first particle))))

; (defun closer-to-zero-in-the-long-run (particle other)
;   (let ((accel-sum-a (particle-acceleration-sum particle))
;         (accel-sum-b (particle-acceleration-sum other))
;         (velo-sum-a (particle-velocity-sum particle))
;         (velo-sum-b (particle-velocity-sum other))
;         (distance-a (particle-distance-from-origin particle))
;         (distance-b (particle-distance-from-origin other)))
;     (cond
;       ((< accel-sum-a accel-sum-b) t)
;       ((> accel-sum-a accel-sum-b) nil)
;       (t (cond
;            ((< velo-sum-a velo-sum-b) t)
;            ((> velo-sum-a velo-sum-b) nil)
;            (t (cond
;                 ((< distance-a distance-b) t)
;                 ((> distance-a distance-b) nil)
;                 (t (< (apply #'* (first particle)) (apply #'* (first other)))))))))))

(defun read-file (input-file)
  (with-open-file (f input-file :direction :input
                     :if-does-not-exist :error)
    (loop for particle = (read-particle f)
          while particle
          collect particle)))

(defun read-particle (input-stream)
  (let ((pos (read-part input-stream))
        (velocity (read-part input-stream))
        (accel (read-part input-stream)))
    (when (and pos velocity accel)
      (list pos velocity accel))))

(defun read-part (input-stream)
  ; p=<x,y,z>,?
  (loop while (swallow-optional-chr input-stream #\space))
  (loop while (swallow-optional-chr input-stream #\newline))
  (let ((garbage-at-start (loop for i from 0 below 3
                                for chr = (read-char input-stream nil)
                                finally (return chr))))
    (when garbage-at-start
      (let ((x (read-and-swallow-comma input-stream #'read-integer))
            (y (read-and-swallow-comma input-stream #'read-integer))
            (z (read-and-swallow-comma input-stream #'read-integer)))
        (swallow-optional-chr input-stream #\>)
        (swallow-optional-chr input-stream #\,)
        (loop while (swallow-optional-chr input-stream #\space))
        (list x y z)))))

(defun read-and-swallow-comma (input-stream &optional (read-fn #'read))
  (let ((retval (funcall read-fn input-stream))
        (trailing (peek-char nil input-stream nil)))
    (when (and trailing (char= trailing #\,))
      (read-char input-stream))
    retval))

(defun swallow-optional-chr (input-stream chr)
  (let ((trailing (peek-char nil input-stream nil)))
    (when (and trailing (char= trailing chr))
      (read-char input-stream))))

(defun read-integer (&optional (stream *standard-input*) (eof-error-p t) eof-value recursive-p)
  (let ((minus (peek-char nil stream nil nil recursive-p)))
    (when (and minus (char= minus #\-))
      (read-char stream eof-error-p eof-value recursive-p))
    (* 
      (if minus -1 1)
      (reduce (lambda (a b)
                (+ (* a 10) b))
              (loop for chr = (peek-char nil stream nil eof-value recursive-p)
                    while (and chr (digit-char-p chr))
                    collect (digit-char-p (read-char stream eof-error-p eof-value recursive-p)))
              :initial-value 0))))


(print (day-twenty "d20-test.txt"))
(print (day-twenty "d20.txt"))
nil
