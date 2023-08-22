;; math functions
(define nil '())
(define (inc n) (+ n 1))
(define (dec n) (- n 1))
(define (square x) (* x x))
(define (cube x) (* x x x))
(define (average x y)
  (/ (+ x y) 2))

;; test functions
(define (display-test-fail expected actual)
  (display "fail! expected: ")
  (display expected)
  (display "; actual: ")
  (display actual)
  (display ";")
  (newline))

(define (test-assert result)
  (if (not result)
      (display-test-fail #t result)))

(define (test-equal expected actual)
  (if (not (equal? expected actual))
      (display-test-fail expected actual)))

(define (test-approximate expected actual error)
  (if (< error
         (abs (- expected actual)))
      (display-test-fail expected actual)))
