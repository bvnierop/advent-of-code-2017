(defun day-three-part-one (input)
  ;; Strategy:
  ;; First, determine the spiral layer
  ;; Then, determine the location in the layer
  ;; If we're on the nth layer, we have to move at least n steps in
  ;; And then the offset from 1
  (let ((layer (spiral-layer input)))
    (+ layer (abs (offset-from-center input)))))

;;; Now we know the spiral number. How to get the offset?
;;; Well, the dimensions of a spiral are its (number * 2 + 1)
;;; 0*2 + 1 = 1
;;; 1*2 + 1 = 3
;;; 2*2 + 1 = 5
;;; So how do we find the distance to, say, 22?
;;; Well, its layer is 2.
;;; How do we find its offset?
;;; Lowest number in this spiral is 10 (AND THAT NUMBER IS NOT IN THE CORNER!)
;;; Without the +1, we can go up to every corner.
;;; In case of 22, its offset _from the lowest number_ is 22-10 = 12
;;; Its side = 12 / (2*2) = 12 / 4 = 3 (zero-indexed)
;;; On its side, the offset is 12 % 4 = 0 (do we need the side?)
;;; The index of the center square is ((2*2) / 2)  - 1 (or, layer_idx - 1)
;;; So the offset is (abs (sq_offset - center_offset))


(defun offset-from-center (square)
  (let ((layer (spiral-layer square)))
    (- (square-offset square) (center-offset layer))))


(defun square-offset (square)
  (let* ((layer (spiral-layer square))
         (layer-dimension (* 2 layer))
         (offset-in-layer (- square (lowest-square-in-layer layer))))
    (if (zerop layer)
        0
        (rem offset-in-layer layer-dimension))))

(defun center-offset (layer)
  (if (zerop layer)
      0
      (1- layer)))

(defun spiral-layer (square)
  "Gives the number of the layer, zero-indexed"
  (labels ((recurse (n)
             (if (in-layer-p square n)
                 n
                 (recurse (1+ n)))))
    (recurse 0)))

(defun in-layer-p (num n)
  (and (>= num (lowest-square-in-layer n)) (>= (highest-square-in-layer n) num)))

(defun highest-square-in-layer (n)
  (1+ (* (sum-1-n n) 8)))

(defun lowest-square-in-layer (n)
  (if (zerop n)
      1
      (1+ (highest-square-in-layer (1- n)))))

(defun sum-1-n (n)
  (truncate (* n (1+ n)) 2))

(assert (eq (in-layer-p 1 0) t))
(assert (eq (in-layer-p 2 1) t))
(assert (eq (in-layer-p 9 1) t))

(assert (= (highest-square-in-layer 0) 1))
(assert (= (highest-square-in-layer 1) 9))
(assert (= (highest-square-in-layer 2) 25))

(assert (= (lowest-square-in-layer 0) 1))
(assert (= (lowest-square-in-layer 1) 2))
(assert (= (lowest-square-in-layer 2) 10))

(assert (= (square-offset 21) 3))
(assert (= (square-offset 22) 0))
(assert (= (square-offset 23) 1))

(assert (= (spiral-layer 1) 0))
(assert (= (spiral-layer 2) 1))
(assert (= (spiral-layer 9) 1))
(assert (= (spiral-layer 10) 2))
(assert (= (spiral-layer 25) 2))

(assert (= (day-three-part-one 12) 3))
(assert (= (day-three-part-one 23) 2))
(assert (= (day-three-part-one 1024) 31))
(assert (= (day-three-part-one 1) 0))


(defun day-three-part-two (input)
  (labels ((recurse (n hash)
             (let ((score (square-score n hash)))
               (if (> score input)
                   score
                   (recurse (1+ n) hash)))))
    (let ((h (make-hash-table :test #'equal)))
      (setf (gethash (list 0 0) h) 1)
      (recurse 1 h))))

(defun square-coordinates (square)
  (let ((layer (spiral-layer square))
        (offset (offset-from-center square))
        (side (square-side square)))
    (case side
      (0 (list layer offset))
      (1 (list (- offset) layer))
      (2 (list (- layer) (- offset)))
      (3 (list offset (- layer))))))

(defun square-side (square)
  (let* ((layer (spiral-layer square))
         (layer-dimension (* 2 layer))
         (offset-in-layer (- square (lowest-square-in-layer layer))))
    (if (zerop layer)
        0
        (truncate offset-in-layer layer-dimension))))

(defun square-score (square hash)
  (let ((coords (square-coordinates square)))
    (setf 
      (gethash coords hash)
      (apply #'+
             (mapcar (lambda (offset)
                       (gethash (list (+ (first coords) (first offset))
                                      (+ (second coords) (second offset)))
                                hash 0))
                     (score-offsets))))))

(defun score-offsets ()
  (let ((offsets (list -1 0 1)))
    (loop for x in offsets
          nconc (loop for y in offsets collect (list x y)))))

(assert (equal (square-coordinates 1) '(0 0)))
(assert (equal (square-coordinates 2) '(1 0)))
(assert (equal (square-coordinates 3) '(1 1)))
(assert (equal (square-coordinates 4) '(0 1)))
(assert (equal (square-coordinates 5) '(-1 1)))
(assert (equal (square-coordinates 6) '(-1 0)))
(assert (equal (square-coordinates 7) '(-1 -1)))
(assert (equal (square-coordinates 8) '(0 -1)))
(assert (equal (square-coordinates 9) '(1 -1)))
(assert (equal (square-coordinates 10) '(2 -1)))
(assert (equal (square-coordinates 11) '(2 0)))
(assert (equal (square-coordinates 21) '(-2 -2)))


(assert (= (day-three-part-two 1) 2))
(assert (= (day-three-part-two 4) 5))
(assert (= (day-three-part-two 122) 133))
(assert (= (day-three-part-two 805) 806))

(defparameter *input* 277678)

(print (day-three-part-one *input*))
(print (day-three-part-two *input*))
nil
