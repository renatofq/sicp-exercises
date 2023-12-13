(load "./scratch/queue.scm")

;; agenda --------------------------------------------------------------
(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda)
  (car (segments agenda)))
(define (rest-segments agenda)
  (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time
           (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue!
         (segment-queue (car segments))
         action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr!
               segments
               (cons (make-new-time-segment
                      time
                      action)
                     (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment
                time
                action)
               segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue
            (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments!
         agenda
         (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty:
              FIRST-AGENDA-ITEM")
      (let ((first-seg
             (first-segment agenda)))
        (set-current-time!
         agenda
         (segment-time first-seg))
        (front-queue
         (segment-queue first-seg)))))

;; definitions ---------------------------------------------------------
(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define (after-delay delay action)
  (add-to-agenda!
   (+ delay (current-time the-agenda))
   action
   the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item
             (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

;; wire ----------------------------------------------------------------
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

;; probe ---------------------------------------------------------------
(define (probe name wire)
  (add-action!
   wire
   (lambda ()
     (display name)
     (display " ")
     (display (current-time the-agenda))
     (display "  New-value = ")
     (display (get-signal wire))
     (newline))))

;; inverter ------------------------------------------------------------
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


;; and-gate ------------------------------------------------------------
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


;; or-gate -------------------------------------------------------------
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

;; half-adder ----------------------------------------------------------
(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

;; full-adder ----------------------------------------------------------
(define (full-adder a b c-in sum c-out)
  (let ((c1 (make-wire))
        (c2 (make-wire))
        (s  (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

;; ripple-carry-adder --------------------------------------------------
(define (ripple-carry-adder A B S c-out)
  (define (adder-builder A B S c-out)
    (if (not (null? A))
        (let ((c-in (make-wire)))
          (adder-builder (cdr A) (cdr B) (cdr S) c-in)
          (full-adder (car A) (car B) c-in (car S) c-out))))
  (adder-builder A B S c-out)
  'ok)

;; simulation ----------------------------------------------------------
;; (define input-1 (make-wire))
;; (define input-2 (make-wire))
;; (define sum (make-wire))
;; (define carry (make-wire))


;; ;; install probes
;; (probe 'sum sum)
;; (probe 'carry carry)

;; ;; make half-adder
;; (half-adder input-1 input-2 sum carry)

;; ;; change signal
;; (set-signal! input-1 1)

;; ;; propagate
;; (propagate)


(define (make-bus width)
  (map (lambda (n)
         (make-wire))
       (enumerate-interval 1 width)))

(define (set-bus-signal! bus values)
  (for-each (lambda (wire value)
              (set-signal! wire value))
            bus
            values))

(define adder-input-1 (make-bus 8))
(define adder-input-2 (make-bus 8))
(define adder-output (make-bus 8))
(define adder-carry-on (make-wire))

(probe 'carry-on adder-carry-on)
(for-each (lambda (wire)
            (probe 'output wire))
          adder-output)

(ripple-carry-adder adder-input-1 adder-input-2 adder-output adder-carry-on)

(set-bus-signal! adder-input-1 (list 1 1 1 1 1 1 1 1))

;;(propagate)

(set-bus-signal! adder-input-2 (list 1 1 1 1 1 1 1 1))

(propagate)
