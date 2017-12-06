(defun day-two-part-one (input)
  (apply #'+
         (mapcar (lambda (lst)
                   (difference-by #'min-max #'- lst))
                 input)))

(defun difference-by (pair-fn result-fn lst) 
  (let ((lo-hi (funcall pair-fn lst)))
  (funcall result-fn (second lo-hi) (first lo-hi))))

(defun min-max (lst)
  (list (apply #'min lst) (apply #'max lst)))

(assert (= (difference-by #'min-max #'- '(1 2 3)) 2))
(assert (= (difference-by #'min-max #'- '(3 2 1)) 2))
(assert (= (day-two-part-one *input-test*) 18))


(defun day-two-part-two (input)
  (apply #'+
         (mapcar (lambda (lst)
                   (difference-by #'evenly-divisible-numbers #'truncate lst))
                 input)))

(defun evenly-divisible-p (a b)
  (multiple-value-bind (aa bb)
    (truncate (max a b) (min a b))
    (declare (ignore aa))
    (zerop bb)))

(defun evenly-divisible-numbers (lst)
  (sort (first (remove-if-not (lambda (pair)
                   (apply #'evenly-divisible-p pair))
                 (all-pairs lst)))
        #'<))

(defun all-pairs (lst)
  (loop for (a1 . r1) on lst
        nconc (loop for a2 in r1 collect (list a1 a2))))

(assert (eq (evenly-divisible-p 8 2) t))
(assert (eq (evenly-divisible-p 8 3) nil))
(assert (equal (evenly-divisible-numbers '(5 6 7 8 9 10)) '(5 10)))
(assert (equal (evenly-divisible-numbers '(10 9 8 7 6 5)) '(5 10)))
(assert (= (day-two-part-two *input-test-two*) 9))

(defparameter *input-test* (list
                             (list 5 1 9 5)
                             (list 7 5 3)
                             (list 2 4 6 8)))

(defparameter *input-test-two* (list
                                 (list 5 9 2 8)
                                 (list 9 4 7 3)
                                 (list 3 8 6 5)))

(defparameter *input* (list
                        (list 493 458 321 120 49 432 433 92 54 452 41 461 388 409 263 58)
                        (list 961 98 518 188 958 114 1044 881 948 590 972 398 115 116 451 492)
                        (list 76 783 709 489 617 72 824 452 748 737 691 90 94 77 84 756)
                        (list 204 217 90 335 220 127 302 205 242 202 259 110 118 111 200 112)
                        (list 249 679 4015 106 3358 1642 228 4559 307 193 4407 3984 3546 2635 3858 924)
                        (list 1151 1060 2002 168 3635 3515 3158 141 4009 3725 996 142 3672 153 134 1438)
                        (list 95 600 1171 1896 174 1852 1616 928 79 1308 2016 88 80 1559 1183 107)
                        (list 187 567 432 553 69 38 131 166 93 132 498 153 441 451 172 575)
                        (list 216 599 480 208 224 240 349 593 516 450 385 188 482 461 635 220)
                        (list 788 1263 1119 1391 1464 179 1200 621 1304 55 700 1275 226 57 43 51)
                        (list 1571 58 1331 1253 60 1496 1261 1298 1500 1303 201 73 1023 582 69 339)
                        (list 80 438 467 512 381 74 259 73 88 448 386 509 346 61 447 435)
                        (list 215 679 117 645 137 426 195 619 268 223 792 200 720 260 303 603)
                        (list 631 481 185 135 665 641 492 408 164 132 478 188 444 378 633 516)
                        (list 1165 1119 194 280 223 1181 267 898 1108 124 618 1135 817 997 129 227)
                        (list 404 1757 358 2293 2626 87 613 95 1658 147 75 930 2394 2349 86 385)))

(print (day-two-part-one *input*))
(print (day-two-part-two *input*))
nil
