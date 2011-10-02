#lang racket/base
(require racket/contract
         racket/match)

(provide
 (contract-out
  [eax register?]
  [asm? (-> any/c boolean?)]
  [asm-begin (->* () () #:rest (listof asm?)
                  asm?)]
  [mov (-> register? constant?
           asm?)]
  [write (-> asm? void)]))

;; Registers
(struct register (name) #:prefab)
(define eax (register 'eax))

(define register->string
  (match-lambda
   [(register name)
    (symbol->string name)]))

;; Constants
(define constant?
  byte?)

(define (constant->string c)
  (number->string c))

;; Assembly
(struct asm () #:prefab)
(struct block asm (l) #:prefab)
(struct mov asm (dest src) #:prefab)

(define (asm-begin . l)
  (block l))

(define write-one
  (match-lambda
   [(block l)
    (for-each write-one l)]
   [(mov dest src)
    (printf "mov ~a, ~a\n"
            (register->string dest)
            (constant->string src))]))

(define (write a)
  (printf "section .text\n")
  (printf "global start\n")
  (printf "start:\n")
  (write-one a)
  (printf "push eax\n")
  (printf "mov eax, 0x1\n")
  (printf "sub esp, 4\n")
  (printf "int 0x80\n"))
