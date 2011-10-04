#lang racket/base
(require racket/contract
         racket/match)

(provide
 (contract-out
  [al register?]
  [eax register?]
  [ebx register?]
  [ecx register?]
  [edx register?]
  [asm? (-> any/c boolean?)]
  [seqn (->* () () #:rest (listof asm?)
                  asm?)]
  [push (-> register?
           asm?)]
  [pop (-> register?
           asm?)]
  [mov (-> register? (or/c constant? register?)
           asm?)]
  [cltd (-> asm?)]
  [cmp binop/c]
  [xchg binop/c]
  [add binop/c]
  [sub binop/c]
  [rename _and and binop/c]
  [rename _or or binop/c]
  [rename _xor xor binop/c]
  [sete unaop/c]
  [setle unaop/c]
  [setge unaop/c]
  [setl unaop/c]
  [setg unaop/c]
  [imul unaop/c]
  [idiv unaop/c]
  [inc unaop/c]
  [dec unaop/c]
  [rename _not not unaop/c]
  [write (-> asm? void)]))

;; Registers
(struct register (name) #:prefab)
(define al (register 'al))
(define eax (register 'eax))
(define ebx (register 'ebx))
(define ecx (register 'ecx))
(define edx (register 'edx))

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
(struct idiv asm (reg) #:prefab)
(struct imul asm (reg) #:prefab)
(struct sete asm (reg) #:prefab)
(struct setle asm (reg) #:prefab)
(struct setge asm (reg) #:prefab)
(struct setl asm (reg) #:prefab)
(struct setg asm (reg) #:prefab)
(struct inc asm (reg) #:prefab)
(struct dec asm (reg) #:prefab)
(struct _not asm (reg) #:prefab)
(struct xchg asm (dest src) #:prefab)
(struct cmp asm (dest src) #:prefab)
(struct add asm (dest src) #:prefab)
(struct sub asm (dest src) #:prefab)
(struct _and asm (dest src) #:prefab)
(struct _or asm (dest src) #:prefab)
(struct _xor asm (dest src) #:prefab)
(struct cltd asm () #:prefab)

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
   [(cltd)
    (printf "cltd\n")]
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
   [(sete dest)
    (printf "sete ~a\n"
            (register->string dest))]
   [(setle dest)
    (printf "setle ~a\n"
            (register->string dest))]
   [(setge dest)
    (printf "setge ~a\n"
            (register->string dest))]
   [(setl dest)
    (printf "setl ~a\n"
            (register->string dest))]
   [(setg dest)
    (printf "setg ~a\n"
            (register->string dest))]
   [(imul dest)
    (printf "imul ~a\n"
            (register->string dest))]
   [(idiv dest)
    (printf "idiv ~a\n"
            (register->string dest))]
   [(_not dest)
    (printf "not ~a\n"
            (register->string dest))]
   [(_and dest src)
    (printf "and ~a, ~a\n"
            (register->string dest)
            (register->string src))]
   [(cmp dest src)
    (printf "cmp ~a, ~a\n"
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
   [(xchg dest src)
    (printf "xchg ~a, ~a\n"
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
