(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(gcd 206 40)

(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))


(assert-equal 5 (fib 5))

(define (count-change amount)
    (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0)
             (= kinds-of-coins 0))
         0)
        (else
         (+ (cc amount (- kinds-of-coins 1))
            (cc (- amount (first-denomination
                           kinds-of-coins))
                kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(assert-equal 292 (count-change 100))

(define (sum term a b next)
  (define (iter a acc)
    (if (> a b)
      acc
      (sum-iter (next a) (+ acc (term a)))))
  (sum-iter term a b next 0))

(define (pi-sum a b)
  (define (pi-term x) (/ 1.0 (* x (+ x 2))))
  (define (pi-next x) (+ x 4))
  (sum pi-term a b pi-next))

(* 8 (pi-sum 1 1000000))

(define (cube x) (* x x x))
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) b add-dx) dx))

(integral cube 0 1 0.0001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       0.00001))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average x y)
  (/ (+ x y) 2.0))

(define (average-damp f)
  (lambda (x)
    (average x (f x))))


(define (sqrt x)
  (fixed-point
   (average-damp
    (lambda (y) (/ x y)))
   1.0))

(define (square x) (* x x))
(define (cube-root x)
  (fixed-point
   (average-damp
    (lambda (y)
      (/ x (square y))))
   1.0))

(define (deriv g dx)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (cube x) (* x x x))
((deriv cube 0.00001) 5.0)

(sqrt 25)
(cube-root 125)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x)
            ((deriv g 0.0001) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g)
               guess))
(define (sqrt x)
  (newtons-method
   (lambda (y)
     (- (square y) x))
   1.0))

(define z (* (expt 2 7) (expt 3 5)))

(/ z (expt 3 5))

(/ (log z) (log 3))

;; factoring
(define (factored-by z div)
  (define (iter z count)
    (if (not (= (remainder z div) 0))
        count
        (iter (/ z div) (+ count 1))))
  (iter z 0))

(define (car z)
  (factored-by z 2))

(define (cdr z)
  (factored-by z 3))

(car z)
(car 3)

(cdr z)

(sqrt 25)

;; Church's numerals 
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
(define one (add-1 zero))
(define two (add-1 one))

(define (inc n) (+ n 1))

((zero inc) 0)
((one inc) 0)
((two inc) 0)

(define (add n m)
  (lambda (f)
    (lambda (x)
      ((n f) ((m f) x)))))

(((add two (add two two)) inc) 0)

(define (double n) (* 2 n))
(((add two (add two two)) double) 1)

(define one (lambda (f) (lambda (x) (f x))))
((one inc) 0)

(define two (lambda (f) (lambda (x) (f (f x)))))
((two inc) 0)

;; map/reduce/filter
(define (add a b)
  (+ a b))

(define (range n)
  (define (range-iter count result)
    (if (< count 0)
        result
        (range-iter (dec count) (cons count result))))
  (range-iter (dec n) nil))

(define (map fn ls)
  (if (null? ls)
      nil
      (cons (fn (car ls))
            (map fn (cdr ls)))))

(define (filter predicate ls)
  (cond ((null? ls)
         nil)
        ((predicate (car ls))
         (cons (car ls) (filter predicate (cdr ls))))
        (else
         (filter predicate (cdr ls)))))

(define (reduce f acc l)
  (if (null? l)
      acc
      (reduce f (f acc (car l)) (cdr l))))


(filter even? (list 1 2 3 4 5 6))

(reduce + 0 (map square
                 (filter even?
                         (map inc (range 10)))))

(map square (list 1 2 3 4 5))

;; threading macros
(define-syntax ->>
  (syntax-rules ()
    [(_ x) x]
    [(_ x (y ...) rest ...)
     (->> (y ... x) rest ...)]))


(reduce add 0 (map square
                   (filter even?
                           (map inc (range 10)))))

(->> (range 10)
     (map inc)
     (filter even?)
     (map square)
     (reduce add 0))

(list 1 (list 2 (list 3 4)))
;; Local Variables:
;; eval: (olivetti-mode 1)
;; End:
