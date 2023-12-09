;; guile
(define (runtime)
  (get-internal-run-time))

(define nil '())
(define true #t)
(define false #f)
(define pi 3.14159)

;; Chapter 1
(define (inc n) (+ n 1))
(define (dec n) (- n 1))
(define (square x) (* x x))
(define (cube x) (* x x x))
(define (average x y)
  (/ (+ x y) 2))
(define (divides? a b)
  (= (remainder b a) 0))

(define (timed-prime-test n prime?)
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

;; binary-trees
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list
         (left-branch tree)
         (cons (entry tree)
               (copy-to-list
                (right-branch tree)
                result-list)))))
  (copy-to-list tree '()))

(define (list->tree elements)
  (car (partial-tree
        elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size
             (quotient (- n 1) 2)))
        (let ((left-result
               (partial-tree
                elts left-size)))
          (let ((left-tree
                 (car left-result))
                (non-left-elts
                 (cdr left-result))
                (right-size
                 (- n (+ left-size 1))))
            (let ((this-entry
                   (car non-left-elts))
                  (right-result
                   (partial-tree
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree
                     (car right-result))
                    (remaining-elts
                     (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))

;; Chapter 3
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1)
                 trials-passed))))
  (iter trials 0))

;; table
(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records))
         (car records))
        (else (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value)
                        (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))

;; memoization
(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result
             (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))


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
