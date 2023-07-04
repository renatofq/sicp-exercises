(define-module (sicp-test)
  #:export (assert
            assert-equal
            assert-approximation))

(define (assert predicate)
  (if (not predicate)
      (throw 'assertion-failed  "predicate failed")))

(define (assert-equal a b)
  (assert (= a b)))

(define (assert-approximation expected actual)
  (assert (< (abs (- 1 (/ expected actual))) 0.0001)))

(define (abs x)
  (if (< x 0) (- x) x))
