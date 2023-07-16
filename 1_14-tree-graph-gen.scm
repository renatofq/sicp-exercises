(define (count-change amount)
  (display-node amount 5 "" "r")
  (cc amount 5 "r"))

(define (cc amount kinds-of-coins node-name)
  (cond ((= amount 0)
         (display-leaf node-name "1")
         1)
        ((or (< amount 0)
             (= kinds-of-coins 0))
         (display-leaf node-name "0")
         0)
        (else
         (+ (display-node-cc
             amount
             (- kinds-of-coins 1)
             node-name
             "d")
            (display-node-cc
             (- amount (first-denomination kinds-of-coins))
             kinds-of-coins
             node-name
             "e")))))

(define (display-node amount kinds-of-coins node-name terminator)
  (display "  ")
  (display node-name)
  (display terminator)
  (display "[label=\"")
  (display "(cc ")
  (display amount)
  (display " ")
  (display kinds-of-coins)
  (display ")\"];")
  (newline))

(define (display-link node-name terminator)
  (display "  ")
  (display node-name)
  (display " -> ")
  (display node-name)
  (display terminator)
  (display ";")
  (newline))

(define (display-leaf node-name result)
  (display "  ")
  (display node-name)
  (display result)
  (display "[label=\"")
  (display result)
  (display "\"];")
  (newline)
  (display-link node-name result))


(define (display-node-cc amount kinds-of-coins node-name direction)
         (display-node amount kinds-of-coins node-name direction)
         (display-link node-name direction)
         (cc amount kinds-of-coins (string-append node-name direction)))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(display "digraph {")
(newline)
(count-change 11)
(display "}")
(newline)
