(in-package #:mylib/tests/number)

(deftest numeric-helpers
  (ok (= 9 (square 3)))
  (ok (= 3 (clamp 3 0 5)))
  (ok (= 0 (clamp -1 0 5)))
  (ok (= 5 (clamp 8 0 5)))
  (ok (maxp 5 1 2 3))
  (ok (minp 1 2 3 4))
  (ok (= 5 (lerp 0 10 0.5)))
  (ok (approx= 1.0d0 (+ 1.0d0 1.0d-13)))
  (ok (approx-zero-p 1.0d-13))
  (ok (approx<= 1.0d0 (+ 1.0d0 1.0d-13)))
  (ok (approx>= 1.0d0 (- 1.0d0 1.0d-13))))

(deftest modifying-macros
  (let ((x 3))
    (maxf x 10 4)
    (ok (= 10 x)))
  (let ((x 3))
    (minf x 10 1)
    (ok (= 1 x))))
