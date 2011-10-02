#lang racket/base
(require racket/contract
         rackunit
         racket/file
         racket/port
         "compiler.rkt")

(provide
 (contract-out
  [test-program
   (-> symbol? any/c
       byte-regexp? byte-regexp? exact-nonnegative-integer?
       void)]))

(define (run pth)
  (define stdout (open-output-bytes 'stdout))
  (define stderr (open-output-bytes 'stderr))
  (define-values
    (sp stdoutp stdin stderrp)
    (subprocess #f #f #f
                pth))
  (define stdoutt (thread (lambda () (copy-port stdoutp stdout))))  
  (define stderrt (thread (lambda () (copy-port stderrp stderr))))
  (subprocess-wait sp)
  (thread-wait stdoutt)
  (thread-wait stderrt)
  (values (get-output-bytes stdout)
          (get-output-bytes stderr)
          (subprocess-status sp)))

(define (test-program language program
                      expected-stdout-rx expected-stderr-rx expected-exit-code)
  (define p-pth (make-temporary-file "~a.src"))
  (define a-pth (make-temporary-file "~a.S"))
  (define b-pth (make-temporary-file "~a.bin"))
  (test-case
   (format "~a > ~e" language program)
   (after
    (with-output-to-file
        p-pth #:exists 'replace
      (lambda ()
        (write program)))
    (check-not-exn
     (lambda ()
       (compile 'day0 p-pth
                'asm a-pth))
     "Compiling")
    (link a-pth b-pth)
    (define-values (actual-stdout actual-stderr actual-exit-code)
      (run b-pth))
    (check-equal? expected-exit-code actual-exit-code "Exit Code")
    (check-not-false (regexp-match expected-stdout-rx actual-stdout) "Standard Output")
    (check-not-false (regexp-match expected-stderr-rx actual-stderr) "Standard Error")
    (void)
    (begin (delete-file p-pth)
           (delete-file a-pth)
           (delete-file b-pth)))))
