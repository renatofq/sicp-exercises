;; Testing for primality
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

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

;;
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low
            (enumerate-interval
             (+ low 1)
             high))))

;; prime-sum
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair)
        (cadr pair)
        (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter
        prime-sum?
        (flatmap
         (lambda (i)
           (map (lambda (j)
                  (list i j))
                (enumerate-interval
                 1
                 (- i 1))))
         (enumerate-interval 1 n)))))

;; Permutations
(define (permutations s)
  (if (null? s)                        ; empty set?
      (list nil)                       ; sequence containing empty set
      (flatmap (lambda (x)
                 (map (lambda (p)
                        (cons x p))
                      (permutations
                       (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

;; 2.40
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j)
                    (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter
        prime-sum?
        (unique-pairs n))))
;; 2.41
(define (unique-triplets n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k)
                               (list i j k))
                             (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))



(define (triplet-sum t)
  (+ (car t) (cadr t) (caddr t)))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (unique-triplets n)
  (flatmap (lambda (i)
             (map (lambda (p) (cons i p))
                  (unique-pairs (- i 1))))
           (enumerate-interval 1 n)))

(define (unique-triplets-whose-sum-is s n)
  (filter (lambda (t)
            (= s (triplet-sum t)))
          (unique-triplets n)))


(define-syntax ->>
  (syntax-rules ()
    [(_ x) x]
    [(_ x (y ...) rest ...)
     (->> (y ... x) rest ...)]))



(define (unique-triplets-whose-sum-is s n)
  (->> (enumerate-interval 1 n)
       (flatmap (lambda (i)
                  (->> (enumerate-interval 1 (- i 1))
                       (flatmap (lambda (j)
                                  (->> (enumerate-interval 1 (- j 1))
                                       (map (lambda (k) (list j k))))))
                       (map (lambda (p) (cons i p))))))
       (filter (lambda (t) (= s (triplet-sum t))))))


