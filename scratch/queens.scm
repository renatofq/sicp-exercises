(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define empty-board
  '())

(define (adjoin-position new-row k rest-of-queens)
  (cons (cons k new-row) rest-of-queens))

(define (queen-check? queen k r)
  (let ((queen-k (car queen))
        (queen-r (cdr queen)))
    (or (= r queen-r)
        (= (- k queen-k) (abs (- r queen-r))))))

(define (safe? k positions)
  (define r (cdar positions))
  (define (safe-iter rest)
    (cond ((nil? rest) #t)
          ((queen-check? (car rest) k r) #f)
          (else (safe-iter (cdr rest)))))
  (safe-iter (cdr positions)))


(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter (lambda (positions)
                  (safe? k positions))
                (flatmap (lambda (rest-of-queens)
                           (map (lambda (new-row)
                                  (adjoin-position new-row k rest-of-queens))
                                (enumerate-interval 1 board-size)))
                         (queen-cols (- k 1))))))
  (queen-cols board-size))

(queens 2)

;; (((1 . 1))
