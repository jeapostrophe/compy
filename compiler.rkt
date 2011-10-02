#lang racket/base
(require racket/runtime-path
         racket/contract
         racket/match
         racket/file
         racket/system)

(provide
 (contract-out
  [compile (-> language? path-string?
               language? path-string?
               void)]
  [link (-> path-string? path-string?
            void)]))

(define language?
  (symbols 'asm 'day0))

(define nasm-pth (find-executable-path "nasm"))
(define ld-pth (find-executable-path "ld"))
(define (link a-pth b-pth)
  (define o-pth (make-temporary-file "~a.o"))
  (system* nasm-pth "-f" "macho" a-pth "-o" o-pth)
  (case (system-type)
    [(macosx)
     (system* ld-pth "-macosx_version_min" "10.7" "-lSystem" "-o" b-pth o-pth)]
    [(unix)
     (system* ld-pth "-o" b-pth o-pth)])
  (delete-file o-pth))

(define-runtime-path langs "langs")
(define (compile in-lang in-pth
                 out-lang out-pth)
  (define in (file->value in-pth))
  (define in-parse
    (dynamic-require (build-path langs (format "~a.rkt" in-lang))
                     'parse))
  (define in-parsed
    (in-parse in))
  (define to-out
    (dynamic-require (build-path langs (format "~a.rkt" in-lang))
                     (string->symbol (format "to-~a" out-lang))))
  (define out-form
    (to-out in-parsed))
  (define out-write
    (dynamic-require (build-path langs (format "~a.rkt" out-lang))
                     'write))
  (with-output-to-file
      out-pth #:exists 'replace
      (lambda ()
        (out-write out-form))))
