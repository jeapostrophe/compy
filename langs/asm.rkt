#lang racket/base
(require racket/contract
         racket/match)

(provide
 (contract-out
  [eax register?]
  [ebx register?]
  [asm? (-> any/c boolean?)]
  [seqn (->* () () #:rest (listof asm?)
                  asm?)]
  [push (-> register?
           asm?)]
  [pop (-> register?
           asm?)]
  [mov (-> register? (or/c constant? register?)
           asm?)]
  [add (-> register? register?
           asm?)]
  [sub (-> register? register?
           asm?)]
  [write (-> asm? void)]))

;; Registers
(struct register (name) #:prefab)
(define eax (register 'eax))
(define ebx (register 'ebx))

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
(struct push asm (src) #:prefab)
(struct pop asm (dest) #:prefab)
(struct add asm (dest src) #:prefab)
(struct sub asm (dest src) #:prefab)

(define arg->string
  (match-lambda
   [(? register? r)
    (register->string r)]
   [c
    (constant->string c)]))

(define (seqn . l)
  (block l))

(define write-one
  (match-lambda
   [(block l)
    (for-each write-one l)]
   [(push src)
    (printf "push ~a\n"
            (register->string src))]
   [(pop dest)
    (printf "pop ~a\n"
            (register->string dest))]
   [(add dest src)
    (printf "add ~a, ~a\n"
            (register->string dest)
            (register->string src))]
   [(sub dest src)
    (printf "sub ~a, ~a\n"
            (register->string dest)
            (register->string src))]
   [(mov dest src)
    (printf "mov ~a, ~a\n"
            (register->string dest)
            (arg->string src))]))

(define (write a)
  (printf "section .text\n")
  (printf "global start\n")
  (printf "start:\n")
  (write-one a)
  (printf "push eax\n")
  (printf "mov eax, 0x1\n")
  (printf "sub esp, 4\n")
  (printf "int 0x80\n"))
