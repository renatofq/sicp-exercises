;; Differentiation
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product
           (deriv (multiplier exp) var)
           (multiplicand exp))))
        (else (error "unknown expression
                      type: DERIV" exp))))

;; Variables
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

;; Sums
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))

;; Products
(define (make-product m1 m2)
  (cond ((or (=number? m1 0)
             (=number? m2 0))
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list '* m1 m2))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

;; Exponents
(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent))
         (expt base exponent))
        (else (list '** base exponent))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))
(define (base e) (cadr e))
(define (exponent e) (caddr e))

(define (deriv exp var)
  (format #t "deriv ~a~%" exp)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product
           (deriv (multiplier exp) var)
           (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (exponent exp)
          (deriv (make-exponentiation
                  (base exp)
                  (make-sum (exponent exp) (- 1)))
                 var)))
        (else (error "unknown expression
                      type: DERIV" exp))))

;; sums
(define (make-sum a1 a2)
  (format #t "make-sum ~a ~a~%" a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s)
  (cadr s))
(define (augend s)
  (let ((agnd (cddr s)))
    (if (nil? (cdr agnd))
        (car agnd)
        (cons '+ agnd))))

;; products
(define (make-product m1 m2)
  (format #t "make-product ~a ~a~%" m1 m2)
  (cond ((or (=number? m1 0)
             (=number? m2 0))
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list '* m1 m2))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p)
  (cadr p))

(define (multiplicand p)
  (let ((multcnd (cddr p)))
    (if (nil? (cdr multcnd))
        (car multcnd)
        (cons '* multcnd))))

;;;; Algebraic notation ---------------------------
(define (deriv exp var)
  (format #t "deriv ~a~%" exp)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product
           (deriv (multiplier exp) var)
           (multiplicand exp))))
        (else (error "unknown expression
                      type: DERIV" exp))))

;; Variables
(define (variable? exp)
  (and (nil? (cdr exp))
       (symbol? (car exp))))
(define (same-variable? exp1 exp2)
  (and (variable? exp1)
       (variable? exp2)
       (eq? (car exp1) (car exp2))))

(define (operator exp)
  (cadr exp))

(define (first-operand op exp)
  (let ((head (car exp)))
    (if (eq? op head)
        nil
        (cons head (first-operand op (cdr exp))))))

(define (second-operand exp)
  (let ((oprnd (cddr exp)))
    (if (nil? (cdr oprnd))
        (car oprnd)
        (cons (car oprnd) (cons (operator exp) (cdr oprnd))))))

(define (is-operation? op exp)
  (and (pair? exp)
       (eq? op (operator exp))))

(define (find-operation op exp)
  (cond ((nil? (cdr exp)) nil)
        ((is-operation? op exp) exp)
        (else (find-operation op (cddr exp)))))

;; Solution
;; Sums
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list a1 '+ a2))))

(define (sum? exp)
  (and (is-operation? '+ exp)
               (nil? (find-operation '* (cddr exp)))))
(define (addend exp) (first-operand '+ exp))
(define (augend exp) (second-operand (find-operation '+ exp)))

;; Products
(define (make-product m1 m2)
  (cond ((or (=number? m1 0)
             (=number? m2 0))
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list m1 '* m2))))

(define (product? exp)
  (not (nil? (find-operation '* exp))))
(define (multiplier exp)
  (first-operand '* exp))
(define (multiplicand exp) (second-operand (find-operation '* exp)))
