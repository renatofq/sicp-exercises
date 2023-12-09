(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records))
         (car records))
        (else (assoc key (cdr records)))))

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        #f)))

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

(define tbl (make-table))

(define (table? table)
  (and (pair? table)
       (eq? '*table* (car table))))

(define (deep-lookup keys value)
  (if (null? keys)
      value
      (let ((record (lookup (car keys) value)))
        (display record)
        (newline)
        (if record
            (deep-lookup (cdr keys) record)
            #f))))

(define (deep-insert! keys value table)
  (define (deep-insert-helper! key keys table)
    (let ((record (lookup key table)))
      (cond ((not record)
             (if (null? keys)
                 (insert! key value table)
                 (let ((new-table (make-table)))
                   (insert! key new-table table)
                   (deep-insert-helper! (car keys) (cdr keys) new-table))))
            ((table? record)
             (if (null? keys)
                 (insert! key value record)
                 (deep-insert-helper! (car keys) (cdr keys) record)))
            (else (error "INSERT into a not table element")))))
  (if (null? keys)
        (error "INSERT cannot insert null keys")
        (deep-insert-helper! (car keys) (cdr keys) table)))

(define tbld (list '*table*
                   (cons 'a (list '*table*
                                  (cons 'b 1)
                                  (cons 'c 2)))
                   (cons 'd 3)))

(define tbld0 (make-table))

(deep-insert! '(a b) 1 tbld0)
