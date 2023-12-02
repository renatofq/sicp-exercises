;; element of the deque ------------------------------------------------
(define (make-element item)
  (cons item (cons '() '())))

(define (element-item element)
  (car element))

(define (next-element element)
  (cddr element))

(define (previous-element element)
  (cadr element))

(define (link-elements! prev next)
  (set-cdr! (cdr prev) next)
  (set-car! (cdr next) prev))

(define (unlink-elements! prev next)
  (set-cdr! (cdr prev) '())
  (set-car! (cdr next) '()))


;; tests
(define e0 (make-element 'a))

(test-assert (eq? '()
                  (next-element e0)))

(test-assert (eq? '()
                  (previous-element e0)))

(test-assert (eq? 'a
                 (element-item e0)))

(define e1 (make-element 'b))
(link-elements! e0 e1)

(test-assert (eq? e1
                  (next-element e0)))

(test-assert (eq? e0
                  (previous-element e1)))

(unlink-elements! e0 e1)

(test-assert (eq? '()
                  (next-element e0)))

(test-assert (eq? '()
                  (previous-element e1)))


;; deque ---------------------------------------------------------------
(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))
(define (set-front-ptr! deque item)
  (set-car! deque item))
(define (set-rear-ptr! deque item)
  (set-cdr! deque item))

(define (make-deque) (cons '() '()))
(define (empty-deque? deque)
  (null? (front-ptr deque)))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (car (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty deque" deque)
      (car (rear-ptr deque))))

(define (front-insert-deque! deque item)
  (let ((new-pair (make-element item)))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           deque)
          (else (link-elements! new-pair (front-ptr deque))
                (set-front-ptr! deque new-pair)
                deque))))

(define (rear-insert-deque! deque item)
  (let ((new-pair (make-element item)))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           deque)
          (else (link-elements! (rear-ptr deque) new-pair)
                (set-rear-ptr! deque new-pair)
                deque))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque" deque))
        (else
         (let ((next (next-element (front-ptr deque))))
           (cond ((null? next)
                  (set-front-ptr! deque '())
                  (set-rear-ptr! deque '()))
                 (else (unlink-elements! (front-ptr deque) next)
                       (set-front-ptr! deque next)))
           deque))))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque" deque))
        (else
         (let ((prev (previous-element (rear-ptr deque))))
           (cond ((null? prev)
                  (set-front-ptr! deque '())
                  (set-rear-ptr! deque '()))
                 (else (unlink-elements! prev (rear-ptr deque))
                       (set-rear-ptr! deque prev)))
           deque))))

;; tests
(define q (make-deque))
(test-assert (empty-deque? q))

(front-insert-deque! q 'b)
(front-insert-deque! q 'a)
(rear-insert-deque! q 'c)

(test-assert (not (empty-deque? q)))

(test-assert (eq? 'a
                  (front-deque q)))
(test-assert (eq? 'c
                  (rear-deque q)))

(front-delete-deque! q)

(test-assert (eq? 'b
                  (front-deque q)))
(test-assert (eq? 'c
                  (rear-deque q)))

(rear-delete-deque! q)
(test-assert (eq? 'b
                  (front-deque q)))
(test-assert (eq? 'b
                  (rear-deque q)))

(rear-delete-deque! q)
(test-assert (empty-deque? q))
