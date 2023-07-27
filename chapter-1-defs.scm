;; math functions
(define (identity a) a)
(define (inc n) (+ n 1))
(define (square x) (* x x))
(define (cube x) (* x x x))

;; test functions
(define (display-test-fail expected actual)
  (display "fail! expected: ")
  (display expected)
  (display "; actual: ")
  (display actual)
  (display ";")
  (newline))

(define (test-assert expected)
  (if (not expected)
      (display-test-fail expected actual)))


(define (test-equal expected actual)
  (if (not (= expected actual))
      (display-test-fail expected actual)))

(define (test-approximate expected actual error)
  (if (< error
         (abs (- expected actual)))
      (display-test-fail expected actual)))
