(import (srfi 28))
(import (srfi 64))

(define (sicp-babel-runner)
  (let ((runner (test-runner-null))
        (num-passed 0)
        (num-failed 0))
    (test-runner-on-test-end! runner
      (lambda (runner)
        (case (test-result-kind runner)
          ((pass xpass) (set! num-passed (+ num-passed 1)))
          ((fail xfail) (set! num-failed (+ num-failed 1)))
          (else #t))))
    (test-runner-on-final! runner
      (lambda (runner)
        (if (> num-failed 0)
            (display (format "~a failed: ~a tests failed.~%"
                    (test-runner-test-name runner)
                    num-failed))
            (display (format "~a Ok: ~a tests passed.~%"
                    (test-runner-test-name runner)
                    num-passed)))))
    runner))

(test-runner-factory
 (lambda () (sicp-babel-runner)))
