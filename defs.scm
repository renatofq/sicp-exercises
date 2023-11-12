;; guile
(define (runtime)
  (get-internal-run-time))

(define nil '())
(define true #t)
(define false #f)

;; Chapter 1
(define (inc n) (+ n 1))
(define (dec n) (- n 1))
(define (square x) (* x x))
(define (cube x) (* x x x))
(define (average x y)
  (/ (+ x y) 2))
(define (divides? a b)
  (= (remainder b a) 0))

(define (smallest-divisor n)
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n)
           n)
          ((divides? test-divisor n)
           test-divisor)
          (else
           (find-divisor n (+ test-divisor 2)))))
  (if (divides? 2 n)
      2
      (find-divisor n 3)))

(define (prime? n)
  (= n (smallest-divisor n)))


;; Chapter 2
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))
(define fold-right accumulate)
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (=number? a n)
  (and (number? a) (number? n) (= a n)))

(define (count list)
  (define (iter n list)
    (if (null? list)
        n
        (iter (inc n) (cdr list))))
  (iter 0 list))

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

