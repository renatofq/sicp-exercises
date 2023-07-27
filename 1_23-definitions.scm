(define (runtime)
  (get-internal-run-time))

(define (smallest-divisor n)
  (if (divides? 2 n)
      2
      (find-uneven-divisor n 3)))

(define (find-uneven-divisor n test-divisor)
  (cond ((> (square test-divisor) n)
         n)
        ((divides? test-divisor n)
         test-divisor)
        (else (find-uneven-divisor
               n
               (+ test-divisor 2)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (let ((start-time (runtime))
        (is-prime (prime? n)))
    (if is-prime
        (report-prime n (- (runtime) start-time)))
    is-prime))

(define (report-prime n elapsed-time)
  (display n)
  (display " | ")
  (display elapsed-time)
  (newline))

(define (search-for-primes start)
  (define (search-for-primes-iter n count)
    (if (> count 0)
        (search-for-primes-iter
         (+ n 2)
         (if (timed-prime-test n) (- count 1) count))))
  (search-for-primes-iter
   (if (divides? 2 start) (+ start 1) start)
   3))
