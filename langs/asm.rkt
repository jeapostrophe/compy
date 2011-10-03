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
  [add binop/c]
  [sub binop/c]
  [rename _and and binop/c]
  [rename _or or binop/c]
  [rename _xor xor binop/c]
  [inc unaop/c]
  [dec unaop/c]
  [rename _not not unaop/c]
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
(struct inc asm (reg) #:prefab)
(struct dec asm (reg) #:prefab)
(struct _not asm (reg) #:prefab)
(struct add asm (dest src) #:prefab)
(struct sub asm (dest src) #:prefab)
(struct _and asm (dest src) #:prefab)
(struct _or asm (dest src) #:prefab)
(struct _xor asm (dest src) #:prefab)

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
   [(inc dest)
    (printf "inc ~a\n"
            (register->string dest))]
   [(dec dest)
    (printf "dec ~a\n"
            (register->string dest))]
   [(_not dest)
    (printf "not ~a\n"
            (register->string dest))]
   [(_and dest src)
    (printf "and ~a, ~a\n"
            (register->string dest)
            (register->string src))]
   [(_or dest src)
    (printf "or ~a, ~a\n"
            (register->string dest)
            (register->string src))]
   [(_xor dest src)
    (printf "xor ~a, ~a\n"
            (register->string dest)
            (register->string src))]
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

;; Contracts
(define binop/c
  (-> register? register?
      asm?))
(define unaop/c
  (-> register?
      asm?))
