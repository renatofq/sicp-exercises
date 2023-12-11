(define (after-delay delay f)
  (f))

;; wire
(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin ((car procedures))
             (call-each (cdr procedures)))))

(define (make-wire)
  (let ((signal-value 0)
        (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each
                  action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures
            (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal)
             signal-value)
            ((eq? m 'set-signal!)
             set-my-signal!)
            ((eq? m 'add-action!)
             accept-action-procedure!)
            (else (error "Unknown operation: WIRE" m))))
    dispatch))

(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

;; inverter
(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define inverter-delay 1)
(define (inverter input output)
  (define (invert-input)
    (let ((new-value
           (logical-not (get-signal input))))
      (after-delay
       inverter-delay
       (lambda ()
         (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)


;; and-gate
(define (logical-and s1 s2)
  (cond ((not (or (= s1 1)
                  (= s1 0)))
         (error "Invalid s1 signal" s1))
        ((not (or (= s2 1)
                  (= s2 0)))
         (error "Invalid s2 signal" s2))
        ((and (= s1 1) (= s2 1))
         1)
        (else 0)))

(define and-gate-delay 2)
(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1)
                        (get-signal a2))))
      (after-delay
       and-gate-delay
       (lambda ()
         (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)


;; or-gate
(define (logical-or s1 s2)
  (cond ((not (or (= s1 1)
                  (= s1 0)))
         (error "Invalid s1 signal" s1))
        ((not (or (= s2 1)
                  (= s2 0)))
         (error "Invalid s2 signal" s2))
        ((and (= s1 0) (= s2 0))
         0)
        (else 1)))
(define or-gate-delay 2)
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1)
                       (get-signal a2))))
      (after-delay
       or-gate-delay
       (lambda ()
         (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

;; half-adder
(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

;; full-adder
(define (full-adder a b c-in sum c-out)
  (let ((c1 (make-wire))
        (c2 (make-wire))
        (s  (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

;; ripple-carry-adder
(define (ripple-carry-adder A B S c-out)
  (define (adder-builder A B S c-out)
    (if (not (null? A))
        (let ((c-in (make-wire)))
          (adder-builder (cdr A) (cdr B) (cdr S) c-in)
          (full-adder (car A) (car B) c-in (car S) c-out))))
  (adder-builder A B S c-out)
  'ok)
