#lang racket/base
(require racket/contract
         racket/runtime-path
         racket/match
         racket/list
         rackunit
         racket/file
         racket/port
         "compiler.rkt")

(provide
 tests
 (contract-out
  [test-program
   (-> symbol? any/c
       byte-regexp? byte-regexp? exact-nonnegative-integer?
       void)]))

(define (run pth)
  (define stdout (open-output-bytes 'stdout))
  (define stderr (open-output-bytes 'stderr))
  (define-values
    (sp stdoutp stdinp stderrp)
    (subprocess #f #f #f
                pth))
  (define stdoutt (thread (lambda () (copy-port stdoutp stdout))))  
  (define stderrt (thread (lambda () (copy-port stderrp stderr))))
  (subprocess-wait sp)
  (thread-wait stdoutt)
  (thread-wait stderrt)
  (close-input-port stdoutp)
  (close-input-port stderrp)
  (close-output-port stdinp)
  (values (get-output-bytes stdout)
          (get-output-bytes stderr)
          (subprocess-status sp)))

(define (test-program language program
                      expected-stdout expected-stderr expected-exit-code)
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
       (compile language p-pth
                'asm a-pth))
     "Compiling")
    (link a-pth b-pth)
    (define-values (actual-stdout actual-stderr actual-exit-code)
      (run b-pth))
    (unless (equal? actual-exit-code expected-exit-code)
      (displayln (file->string a-pth)))
    (check-equal? actual-exit-code expected-exit-code "Exit Code")
    (check-equal? actual-stdout expected-stdout "Standard Output")
    (check-equal? actual-stderr expected-stderr "Standard Error")    
    (void)
    (begin (delete-file p-pth)
           (delete-file a-pth)
           (delete-file b-pth)))))

(define-syntax-rule (tests [(lang) program ...] ...)
  (tests* (list (list 'lang 'program ...)
                ...)))

(define-runtime-path langs "langs")
(define (interp-for in-lang t)
  (define in-parse
    (dynamic-require (build-path langs (format "~a.rkt" in-lang))
                     'parse))
  (define in-interp
    (dynamic-require (build-path langs (format "~a.rkt" in-lang))
                     'interp))
  (define e (in-parse t))
  (define stdout-bs (open-output-bytes))
  (define stderr-bs (open-output-bytes))
  (define exit-code
    (parameterize ([current-output-port stdout-bs]
                   [current-error-port stderr-bs])
      (in-interp e)))
  (values (get-output-bytes stdout-bs)
          (get-output-bytes stderr-bs)
          exit-code))

(define (tests* t)
  (define just
    (match (current-command-line-arguments)
      [(vector) #f]
      [(vector day-s)
       (string->symbol day-s)]))
  (for/fold
      ([earlier-tests empty])
      ([l*ps (in-list t)])
    (match-define (list lang  ps ...) l*ps)
    (define all-tests (append earlier-tests ps))
    (when (or (not just) (equal? just lang))
      (test-case
       (format "~a" lang)
       (for ([t* (in-list all-tests)])
            (test-case
             (format "~a" t*)
             (match t*
               ;; Skips the interpreter, because it is busted on this example
               [(list '! t '=> stdout stderr exit-code)
                (test-program lang t stdout stderr exit-code)]
               [(list t '=> ans)
                (define-values (stdout stderr exit-code) (interp-for lang t))
                (check-equal? exit-code ans (format "Interpreter for ~a on ~e" lang t))
                (test-program lang t stdout stderr exit-code)]
               [t
                (define-values (stdout stderr exit-code) (interp-for lang t))
                (test-program lang t stdout stderr exit-code)])))))
    all-tests)
  (void))
       
