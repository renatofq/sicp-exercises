;; dependencies --------------------------------------------------------
(define (square x) (* x x))

(define (sublist head ls)
  (if (or (null? ls)
          (eq? head (car ls)))
      ls
      (sublist head (cdr ls))))

(define (maximum greater? ls)
  (define (iter result ls)
    (if (null? ls)
        result
        (let ((head (car ls))
              (rest (cdr ls)))
          (if (greater? head result)
              (iter head rest)
              (iter result rest)))))
  (if (null? ls)
      '()
      (iter (car ls) (cdr ls))))

;; put/get -------------------------------------------------------------
(define op-table '())

(define (get op type-tags)
  (assoc-ref op-table (list op type-tags)))

(define (put op type-tags item)
  (set! op-table
        (assoc-set! op-table
                    (list op type-tags)
                    item)))

(define coercion-table '())

(define (get-coercion op type-tags)
  (assoc-ref coercion-table (cons op type-tags)))

(define (put-coercion op type-tags item)
  (set! coercion-table
        (assoc-set! coercion-table
                    (cons op type-tags)
                    item)))


;; tagged data ---------------------------------------------------------
(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum)
         (car datum))
        (else
         (error "Bad tagged datum: TYPE-TAG"
                datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum)
         (cdr datum))
        (else
         (error "Bad tagged datum: CONTENTS"
                datum))))

;; Numeric tower -------------------------------------------------------
(define (apply-coercion type arg)
  (let ((coercion (get-coercion type (type-tag arg))))
    (if coercion
        (coercion arg)
        arg)))

(define (raise x)
  (apply-coercion 'raise x))

(define (drop x)
  (apply-coercion 'drop x))

(define (full-drop x)
  (if (pair? x)
      (let ((droped (drop x)))
        (if (and (not (eq? x droped))
                 (equ? x (raise droped)))
            (full-drop droped)
            x))
      x))

(define (higher-type? t1 t2)
  (let ((t2-tower (sublist t2 '(integer rational real complex))))
    (and (pair? t2-tower)
         (pair? (sublist t1 (cdr t2-tower))))))

(define (raise-all-to target args)
  (define (raise-while-target-is-higher arg)
    (if (higher-type? target (type-tag arg))
        (raise-while-target-is-higher (raise arg))
        arg))
  (map raise-while-target-is-higher args))

(define (level-up args)
  (raise-all-to
   (maximum higher-type? (map type-tag args))
   args))

(define (apply-generic op . args)
  (define (apply-generic-helper op args)
    (let ((type-tags (map type-tag args)))
      (let ((proc (get op type-tags)))
        (if proc
            (full-drop (apply proc (map contents args)))
            (let ((raised (level-up args)))
              (if (equal? raised args)
                  (error "No method for these types: APPLY-GENERICS"
                         (list op type-tags))
                  (apply-generic-helper op raised)))))))
  (apply-generic-helper op args))

;; generic operations --------------------------------------------------
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

;; integer package -----------------------------------------------
(define (install-integer-package)
  (define (tag x)
    (attach-tag 'integer x))
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'integer
       (lambda (x) (tag x)))
  'done)

(define (make-integer n)
  ((get 'make 'integer) n))

;; rational number package ---------------------------------------------
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'numerator '(rational)
       (lambda (x) (numer x)))
  (put 'denominator '(rational)
       (lambda (x) (denom x)))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (numerator x)
  (apply-generic 'numerator x))

(define (denominator x)
  (apply-generic 'denominator x))

;; real package -----------------------------------------------
(define (install-real-package)
  (define (tag x)
    (attach-tag 'real x))
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'real
       (lambda (x) (tag x)))
  'done)

(define (make-real n)
  ((get 'make 'real) n))

;; complex number package ----------------------------------------------
;; retangular complex numbers
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y)
    (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x)
    (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a)
         (tag (make-from-mag-ang r a))))
  'done)

;; polar complex numbers
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a)
         (tag (make-from-mag-ang r a))))
  'done)

;; generic complex numbers
(define (install-complex-package)
  ;; imported procedures from rectangular
  ;; and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag
          'rectangular)
     x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar)
     r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag
     (+ (real-part z1) (real-part z2))
     (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag
     (- (real-part z1) (real-part z2))
     (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang
     (* (magnitude z1) (magnitude z2))
     (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang
     (/ (magnitude z1) (magnitude z2))
     (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2)
         (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2)
         (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2)
         (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2)
         (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a)
         (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (real-part z)
  (apply-generic 'real-part z))
(define (imag-part z)
  (apply-generic 'imag-part z))
(define (magnitude z)
  (apply-generic 'magnitude z))
(define (angle z)
  (apply-generic 'angle z))

;; equality package ----------------------------------------------------
(define (install-equality-package)
  (put 'equ?
       '(integer integer)
       (lambda (x y)
         (= x y)))
  (put 'equ?
       '(rational rational)
       (lambda (x y)
         (and (= (car x) (car y))
              (= (cdr x) (cdr y)))))
  (put 'equ?
       '(real real)
       (lambda (x y)
         (= x y)))
  (put 'equ?
     '(complex complex)
     (lambda (x y)
       (and (= (real-part x) (real-part y))
            (= (imag-part x) (imag-part y)))))
  'done)

(define (equ? x y)
  (apply-generic 'equ? x y))

;; zero check package --------------------------------------------------
(define (install-zero-predicate-package)
  (put '=zero?
       '(integer)
       (lambda (x)
         (= 0 x)))
  (put '=zero?
       '(rational)
       (lambda (x)
         (= 0 (numerator x))))
  (put '=zero?
       '(real)
       (lambda (x)
         (= 0.0 x)))
  (put '=zero?
       '(complex)
       (lambda (x)
         (and (= 0.0 (real-part x))
              (= 0.0 (imag-part x)))))
  'done)

(define (=zero? x)
  (apply-generic '=zero? x))

;; raise/drop package --------------------------------------------------
(define (install-raise-package)
  (define (integer->rational i)
    (make-rational (contents i) 1))
  (define (rational->real r)
    (make-real (/ (numerator r) (denominator r))))
  (define (real->complex x)
    (make-complex-from-real-imag (contents x) 0))
  (define (rational->integer r)
    (make-integer (numerator r)))
  (define (real->rational x)
    (make-rational (contents x) 1))
  (define (complex->real z)
    (make-real (real-part z)))
  ;;
  (put-coercion 'raise 'integer  integer->rational)
  (put-coercion 'raise 'rational rational->real)
  (put-coercion 'raise 'real     real->complex)
  (put-coercion 'drop  'rational rational->integer)
  (put-coercion 'drop  'real     real->rational)
  (put-coercion 'drop  'complex  complex->real))

;; Setup
(install-integer-package)
(install-rational-package)
(install-real-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)
(install-equality-package)
(install-raise-package)

;; test-data
(define i (make-integer 1))
(define r (make-rational 1 4))
(define x (make-real (sqrt 2)))
(define z (make-complex-from-mag-ang (sqrt 2) 0.25))
(define z1 (make-complex-from-real-imag 1 0))
(define z2 (make-complex-from-real-imag 2 0))
(add z1 z2)

(add z i)
