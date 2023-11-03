(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n)
         n)
        ((divides? test-divisor n)
         test-divisor)
        (else (find-divisor
               n
               (+ test-divisor 1)))))

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
