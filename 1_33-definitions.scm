(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (smallest-divisor n)
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n)
           n)
          ((divides? test-divisor n)
           test-divisor)
          (else (find-uneven-divisor
                 n
                 (+ test-divisor 2)))))
  (if (divides? 2 n)
      2
      (find-divisor n 3)))

(define (prime? n)
  (= n (smallest-divisor n)))
