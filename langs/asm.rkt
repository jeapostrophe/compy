#lang racket/base
(require racket/contract
         racket/match)

;; XXX Make this code cleaner

(provide
 (contract-out
  [comment (-> string? asm?)]
  [make-label (->* () (symbol?) label?)]
  [label-mark (-> label? asm?)]
  [al register?]
  [eax register?]
  [ebx register?]
  [ecx register?]
  [edx register?]
  [esi register?]
  [edi register?]
  [ebp register?]
  [esp register?]
  [esp+ (-> number? register?)]
  [asm? (-> any/c boolean?)]
  [seqn (->* () () #:rest (listof asm?)
                  asm?)]
  [jmp (-> label? asm?)]
  [jne (-> label? asm?)]
  [push (-> register?
           asm?)]
  [pop (-> register?
           asm?)]
  [mov (-> register? (or/c constant? register?)
           asm?)]
  [cltd (-> asm?)]
  [cmp (-> register? (or/c constant? register?)
           asm?)]
  [xchg binop/c]
  [add (-> register? (or/c constant? register?)
           asm?)]
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

;; Label
(struct label (name) #:prefab)

(define (label->string l)
  (symbol->string (label-name l)))

(define (sterilize s)
  (regexp-replace* #rx"[^a-zA-Z]" (symbol->string s) "_"))

(define (make-label [id 'opt])
  (label (gensym (sterilize id))))

;; Registers
(struct register () #:prefab)

(struct named-register register (name) #:prefab)
(define al (named-register 'al))
(define eax (named-register 'eax))
(define ebx (named-register 'ebx))
(define ecx (named-register 'ecx))
(define edx (named-register 'edx))
(define esi (named-register 'esi))
(define edi (named-register 'edi))
(define esp (named-register 'esp))
(define ebp (named-register 'ebp))

(struct esp+ register (offset) #:prefab)

(define register->string
  (match-lambda
   [(esp+ offset)
    (format "[esp+~a]" offset)]
   [(named-register name)
    (symbol->string name)]))

;; Constants
(define constant?
  byte?)

(define (constant->string c)
  (number->string c))

;; Assembly
(struct asm () #:prefab)
(struct comment asm (s) #:prefab)
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
(struct jmp asm (l) #:prefab)
(struct jne asm (l) #:prefab)
(struct label-mark asm (l) #:prefab)

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
   [(comment s)
    (printf "\t;; ~a\n" s)]
   [(cltd)
    (printf "\tcltd\n")]
   [(label-mark l)
    (printf "~a:\n"
            (label->string l))]
   [(jmp l)
    (printf "\tjmp ~a\n"
            (label->string l))]
   [(jne l)
    (printf "\tjne ~a\n"
            (label->string l))]
   [(push src)
    (printf "\tpush ~a\n"
            (register->string src))]
   [(pop dest)
    (printf "\tpop ~a\n"
            (register->string dest))]
   [(inc dest)
    (printf "\tinc ~a\n"
            (register->string dest))]
   [(dec dest)
    (printf "\tdec ~a\n"
            (register->string dest))]
   [(sete dest)
    (printf "\tsete ~a\n"
            (register->string dest))]
   [(setle dest)
    (printf "\tsetle ~a\n"
            (register->string dest))]
   [(setge dest)
    (printf "\tsetge ~a\n"
            (register->string dest))]
   [(setl dest)
    (printf "\tsetl ~a\n"
            (register->string dest))]
   [(setg dest)
    (printf "\tsetg ~a\n"
            (register->string dest))]
   [(imul dest)
    (printf "\timul ~a\n"
            (register->string dest))]
   [(idiv dest)
    (printf "\tidiv ~a\n"
            (register->string dest))]
   [(_not dest)
    (printf "\tnot ~a\n"
            (register->string dest))]
   [(_and dest src)
    (printf "\tand ~a, ~a\n"
            (register->string dest)
            (register->string src))]
   [(cmp dest src)
    (printf "\tcmp ~a, ~a\n"
            (register->string dest)
            (arg->string src))]
   [(_or dest src)
    (printf "\tor ~a, ~a\n"
            (register->string dest)
            (register->string src))]
   [(_xor dest src)
    (printf "\txor ~a, ~a\n"
            (register->string dest)
            (register->string src))]
   [(xchg dest src)
    (printf "\txchg ~a, ~a\n"
            (register->string dest)
            (register->string src))]
   [(add dest src)
    (printf "\tadd ~a, ~a\n"
            (register->string dest)
            (arg->string src))]
   [(sub dest src)
    (printf "\tsub ~a, ~a\n"
            (register->string dest)
            (register->string src))]
   [(mov dest src)
    (printf "\tmov ~a, ~a\n"
            (register->string dest)
            (arg->string src))]))

(define (write a)
  (printf "section .text\n")
  (printf "global start\n")
  (printf "start:\n")
  (write-one a)
  (printf "\tpush eax\n")
  (printf "\tmov eax, 0x1\n")
  (printf "\tsub esp, 4\n")
  (printf "\tint 0x80\n"))

;; Contracts
(define binop/c
  (-> register? register?
      asm?))
(define unaop/c
  (-> register?
      asm?))
