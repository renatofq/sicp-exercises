;; (define-syntax delay
;;   (syntax-rules ()
;;     ((delay exp)
;;      (lambda () exp))))

;; (define (force delayed)
;;   (delayed))

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream x y)
     (cons x (delay y)))))

(define stream-null? null?)

(define the-empty-stream '())

(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (force (cdr stream)))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream
       (proc (stream-car s))
       (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin
        (proc (stream-car s))
        (stream-for-each proc
                         (stream-cdr s)))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream)
         the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream
          (stream-car stream)
          (stream-filter
           pred
           (stream-cdr stream))))
        (else (stream-filter
               pred
               (stream-cdr stream)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1)
                                  high))))

;;;;
(define (display-line obj)
  (display obj)
  (newline))

(define (show x)
  (display-line x)
  x)

(define (display-stream s)
  (stream-for-each display-line s))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc
                    (map stream-cdr
                         argstreams))))))

(define (stream-take s n)
  (if (= n 0)
      '()
      (cons (stream-car s)
            (stream-take (stream-cdr s) (- n 1)))))

;; (define sum 0)

;; (define (accum x)
;;   (set! sum (+ x sum))
;;   sum)

;; (define seq
;;   (stream-map
;;    accum
;;    (stream-enumerate-interval 1 20)))

;; (define y (stream-filter even? seq))

;; (define z
;;   (stream-filter
;;    (lambda (x)
;;      (= (remainder x 5) 0)) seq))

;; (stream-ref y 7)

;; (display-stream z)

(define (divisible? x y)
  (= (remainder x y) 0))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define (sieve stream)
  (cons-stream (stream-car stream)
               (sieve (stream-filter
                       (lambda (x)
                         (not (divisible? x (stream-car stream))))
                       (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))


(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define integers (cons-stream 1 (add-streams ones integers)))

(define fibs
  (cons-stream
   0
   (cons-stream
    1 (add-streams
       (stream-cdr fibs) fibs))))


(define primes
  (cons-stream
   2 (stream-filter
      prime? (integers-starting-from 3))))

(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) #t)
          ((divisible? n (stream-car ps)) #f)
          (else (iter (stream-cdr ps)))))
  (iter primes))


;;; srqt
(define sqrt-improve
  (make-counted
   (lambda (guess x)
     (average guess (/ x guess)))))

(define (sqrt-stream x)
  (cons-stream
   1.0
   (stream-map (lambda (guess)
                 (sqrt-improve guess x))
               (sqrt-stream x))))

(define (sqrt-stream x)
  (define guesses
     (cons-stream
      1.0
      (stream-map (lambda (guess)
                    (sqrt-improve guess x))
                  guesses)))
  guesses)

;; stream of pairs
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream
       (stream-car s1)
       (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x)
                  (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define int-pairs (pairs integers integers))



(define (cart-product s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (interleave
     (stream-map (lambda (x)
                   (list (stream-car s) x))
                 (stream-cdr t))
     (stream-map (lambda (x)
                   (list x (stream-car t)))
                 (stream-cdr s)))
    (cart-product (stream-cdr s) (stream-cdr t)))))

(define (triplets s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (interleave
     (stream-map (lambda (pair) (cons (stream-car s) pair))
                 (stream-map (lambda (x) (list (stream-car t) x))
                             (stream-cdr u)))
     (stream-map (lambda (pair) (cons (stream-car s) pair))
                 (pairs (stream-cdr t) (stream-cdr u))))
    (triplets (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define pythagorean-triplets
  (stream-filter (lambda (triplet)
                   (let ((a (car   triplet))
                         (b (cadr  triplet))
                         (c (caddr triplet)))
                     (= (+ (* a a) (* b b)) (* c c))))
   (triplets integers integers integers)))
