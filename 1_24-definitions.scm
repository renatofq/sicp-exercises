(define (runtime)
  (get-internal-run-time))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n)
         (fast-prime? n (- times 1)))
        (else #f)))

(define (report-prime n elapsed-time)
  (display n)
  (display " | ")
  (display elapsed-time)
  (newline))

(define (timed-prime-test n)
  (let ((start-time (runtime))
        (is-prime (fast-prime? n 1)))
    (if is-prime
        (report-prime n (- (runtime) start-time)))
    is-prime))
