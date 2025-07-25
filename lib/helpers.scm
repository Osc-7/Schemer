;;; helpers.ss
;;; Copyright (c) 2000-2009 R. Kent Dybvig, Andy Keep, Oscar Waddell, Daniel P. Friedman
;;;
;;; In this file:
;;;
;;;   (define-who id defn ... expr) is a syntactic form that defines id
;;;   to be the value of (let () defn ... expr) and, within the let,
;;;   defines the variable who to be 'id.
;;;
;;;   (sra x n) shifts x right by n bits
;;;
;;;   align-shift is the amount by which an machine-word index must be
;;;   shifted to convert it to a byte index, e.g., 3 for 8-byte words
;;;
;;;   registers is a list of registers
;;;
;;;   (register? x) returns true iff x is a register, i.e., if x is
;;;   in the list of registers
;;;
;;;   parameter-registers is a list of the registers used for parameter
;;;   passing, in order
;;;
;;;   caller-saved-registers is a list of the caller-saved registers, a
;;;   subset (possibly improper) of the entire set of registers.
;;;
;;;   return-value-register is the register used for the return value
;;;
;;;   return-address-register is the register used for the return address
;;;
;;;   frame-pointer-register is the register used for the frame pointer
;;;
;;;   allocation-pointer-register is the register used for the allocation
;;;   pointer
;;;
;;;   rax, rcx, rdx, rbx, rbp, rsi, rdi, r8, r9, r10, r11, r12,
;;;   r13, r14, r15 are all defined and set to fixed non-zero values
;;;
;;;   (stack-size) returns the current stack size in 8-byte words
;;;   (stack-size n) sets the stack size to n
;;;
;;;   (heap-size) returns the current heap size in 8-byte words
;;;   (heap-size n) sets the heap size to n
;;;
;;;   (mref reg reg/offset) references the stack or heap memory at
;;;   effective address reg + reg/offset
;;;
;;;   (mset! reg reg/offset x) sets the stack or heap memory at
;;;   effective address reg + reg/offset to x
;;;
;;;   (reset-machine-state!) resets the registers and contents of the
;;;   stack and heap back to their original values.
;;;
;;;   (set? ls) returns true iff the list ls contains unique items
;;;
;;;   (set-cons x set) if x is not already in the set "set", then it
;;;   will be added and the new set returned, otherwise "set" is returned
;;;
;;;   (union set ...) returns a new set containing all the unique
;;;   items that appear in any of the set arguments.
;;;
;;;   (intersection set ...) returns a new set containing all the
;;;   unique items that appear in all of the set arguments.
;;;
;;;   (difference set1 set2) returns a new set containing all the
;;;   items from set1 that were not in set2
;;;
;;;   (int32? x) returns true iff x is a 32-bit exact integer
;;;
;;;   (int64? x) returns true iff x is a 64-bit exact integer
;;;
;;;   (uint6? x) returns true iff x is an unsigned 6-bit exact integer
;;;
;;;   (max-frame-var) returns the current max frame var set up by the system.
;;;   (max-frame-var n) sets the max frame var to n
;;;   
;;;   fv0, fv1, ..., fvN, where N is (max-frame-var), are all defined
;;;   to refer to the proper frame location within the stack, based
;;;   at fp, the frame-pointer register.  That is, a reference to fv3 is
;;;   a reference to the stack at location fp + 24.  The offset is 24
;;;   rather than 3 since each word is 8-bytes.
;;;
;;;   (frame-var? x) returns true iff x is a frame-var in the range
;;;
;;;   (frame-var->index fv) returns the frame variable's index, e.g.,
;;;   the call (frame-var->index 'fv3) returns 3.
;;;
;;;   (index->frame-var n) returns the frame variable corresponding to
;;;   the index n, e.g., the call (index->frame-var 3) returns fv3.
;;;
;;;   fv0, fv1, ..., fvN, where N is (max-frame-var)
;;;
;;;   (uvar? x) returns true iff x is a unique variable, i.e., one
;;;   whose name is of the form x.n
;;;
;;;   (label? x) returns true iff x is a label, i.e., one whose name
;;;   is of the form x$n
;;;
;;;   (extract-root name) extracts the root of name, i.e., the part
;;;   before the last dollar sign ($) or period (.).
;;;
;;;   (extract-suffix name) extracts the suffix of name, i.e., the part
;;;   after the last dollar sign ($) or period (.).
;;;
;;;   (unique-suffix) creates a new suffix for use in labels and later
;;;   variable names from an internal counter.  To avoid clashing with
;;;   testcase labels the counter starts at 1000
;;;
;;;   (unique-label sym) expects a Scheme symbol and returns a new
;;;   label with sym as the root and a new suffix number pulled from
;;;   unique-suffix.  (e.g. (unique-label 'join) => 'join$1004)
;;;
;;;   (make-disp-opnd reg offset) returns a disp-opnd record, used to
;;;   represent a displacement-mode operand
;;;
;;;   (disp-opnd? x) returns true iff x is a disp-opnd
;;;
;;;   (disp-opnd-reg disp-opnd) returns the disp-opnd's register
;;;
;;;   (disp-opnd-offset disp-opnd) returns the disp-opnd's offset
;;;
;;;   (make-index-opnd reg offset) returns an index-opnd record, used to
;;;   represent a index-mode operand
;;;
;;;   (index-opnd? x) returns true iff x is a index-opnd
;;;
;;;   (index-opnd-breg index-opnd) returns the index-opnd's base register
;;;
;;;   (index-opnd-ireg index-opnd) returns the index-opnd's index register
;;;
;;;   This file also sets up record writers for disp-opnd and index-opnd
;;;   records and redefine's match's equality check to handle disp-opnds
;;;   and index-opnds
;;;
;;;   (label->x86-64-label label) returns a string of the form Ln, where
;;;   n is label's suffix.  used by emit.
;;;
;;;   (rand->x86-64-arg operand) returns a string representing the
;;;   operand, with integers prefixed by $, registers prefixed by
;;;   %, labels converted to rip-relative operands, disp-opnds
;;;   converted to displacement-mode syntax, and index-opnds converted
;;;   to index-mode syntax.  used by emit.
;;;
;;;   (emit-label label) emits label on a line by itself, followed
;;;   by a colon.
;;;
;;;   (emit opcode opnd ...) emits the instruction described by
;;;   opcode and opnd ....  The operands are automatically formatted
;;;   with the help of rand->x86-64-arg.  Use for everything except
;;;   jump instructions.
;;;
;;;   (emit-jump opcode opnd) emits a jump instruction, using the
;;;   peculiar syntax required for jumps.
;;;
;;;   (emit-program code code ...) emits the boilerplate code with
;;;   the code generated by code code ... embedded within it.
;;;
;;;   (make-begin expr*) flattens begin expressions in expr* and
;;;   tacks the symbol begin on the front of the list, except if the
;;;   list has one element, in which case it returns the element.
;;;   expr* should be nonempty.  E.g., (make-begin '(e1)) => e1 and
;;;   (make-begin '(e1 (begin (begin e2 e3) e4) e5)) =>
;;;   (begin e1 e2 e3 e4 e5).
;;;
;;;  ; the remainder support the scheme-dependent portion of the compiler
;;;
;;;   fixnum-bits is the width of a fixnum
;;;
;;;   shift-fixnum is the amount by which a fixnum is shifted left
;;;
;;;   mask-fixnum, mask-pair, mask-vector, mask-procedure, and
;;;   mask-boolean are masks used to isolate type-tag bits
;;;
;;;   type-fixnum, type-pair, type-vector, type-procedure, and
;;;   type-boolean are type tags
;;;
;;;   size-pair is the size of a pair
;;;
;;;   disp-car, disp-cdr, disp-vector-length, disp-vector-data,
;;;   disp-procedure-code, and disp-procedure-data are displacements
;;;   from the true address of an object to the identified field
;;;   of the object
;;;
;;;   $false is the ptr representation of #f
;;;
;;;   $true is the ptr representation of #t
;;;
;;;   $nil is the ptr representation of ()
;;;
;;;   $void is the ptr representation of (void)
;;;
;;;   (fixnum-range n) returns #t iff n is within the fixnum range
;;;   based on fixnum-bits

(define-syntax define-who
  (lambda (x)
    (syntax-case x ()
      [(k name defn ... expr)
       (with-syntax ([who (datum->syntax-object #'k 'who)])
         #'(define name
             (let ([who 'name])
               defn ...
               expr)))])))

;;; language extensions

(define sra (lambda (x n) (ash x (- n))))

;;; machine state

(define align-shift 3) ; 8-byte words

(module (registers register? rax rcx rdx rbx rbp rsi rdi r8 r9 r10 r11 r12 
         r13 r14 r15 heap-size stack-size $check-heap-overflow mref mset!
	 reset-machine-state!  parameter-registers caller-saved-registers
	 frame-pointer-register return-value-register
	 return-address-register allocation-pointer-register)
  (define heap-offset (/ (+ (most-positive-fixnum) 1) 2))

  (define registers
    '(rax rcx rdx rbx rbp rsi rdi r8 r9 r10 r11 r12 r13 r14 r15))

  (define register?
    (lambda (x)
      (memq x registers)))

 ; calling conventions
  (define caller-saved-registers registers)
  (define parameter-registers '(r8 r9))
  (define frame-pointer-register 'rbp)
  (define return-value-register 'rax)
  (define return-address-register 'r15)
  (define allocation-pointer-register 'rdx)

  (define rax)
  (define rcx)
  (define rdx)
  (define rbx)
  (define rbp)
  (define rsi)
  (define rdi)
  (define r8)
  (define r9)
  (define r10)
  (define r11)
  (define r12)
  (define r13)
  (define r14)
  (define r15)

  (define the-heap '#())
  (define the-stack '#())

  (define-who heap-size
    (make-parameter 100000
      (lambda (n)
        (unless (and (fixnum? n) (fx>= n 0))
          (format-error who "invalid size ~s" n))
        (unless (fx= n (vector-length the-heap))
          (set! the-heap (make-vector n))))))

  (define-who stack-size
    (make-parameter 100000
      (lambda (n)
        (unless (and (fixnum? n) (fx>= n 0))
          (format-error who "invalid size ~s" n))
        (unless (fx= n (vector-length the-stack))
          (set! the-stack (make-vector n))))))

  (define $check-heap-overflow
    (lambda (ap)
      (when (> (fxsrl (- ap heap-offset) align-shift) (vector-length the-heap))
        (format-error 'alloc "heap overflow"))))

  (define-who mref
    (lambda (x y)
      (let ([i (fxsrl (fx+ x y) align-shift)])
        (cond
          [(fx< i 0)
           (format-error who "attempt to reference below stack at index ~s" i)]
          [(fx< i (vector-length the-stack))
           (vector-ref the-stack i)]
          [(fx< i (fx- (fxsrl heap-offset align-shift) 1000))
           (format-error who "attempt to reference above stack at index ~s" i)]
          [else
           (let ([i (fx- i (fxsrl heap-offset align-shift))])
             (cond
               [(fx< i 0)
                (format-error who "attempt to reference below heap at index ~s" i)]
               [(fx< i (vector-length the-heap))
                (vector-ref the-heap i)]
               [else (format-error who "attempt to reference above stack at index ~s" i)]))]))))

  (define-who mset!
    (lambda (x y z)
      (let ([i (fxsrl (fx+ x y) align-shift)])
        (cond
          [(fx< i 0)
           (format-error who "attempt to reference below stack at index ~s" i)]
          [(fx< i (vector-length the-stack))
           (vector-set! the-stack i z)]
          [(fx< i (fx- (fxsrl heap-offset align-shift) 1000))
           (format-error who "attempt to reference above stack at index ~s" i)]
          [else
           (let ([i (fx- i (fxsrl heap-offset align-shift))])
             (cond
               [(fx< i 0)
                (format-error who "attempt to reference below heap at index ~s" i)]
               [(fx< i (vector-length the-heap))
                (vector-set! the-heap i z)]
               [else (format-error who "attempt to reference above stack at index ~s" i)]))]))))

  (define-who reset-machine-state!
    (lambda ()
      (set! rax #x7c7c7c7c7c7c7c70)
      (set! rcx #x7c7c7c7c7c7c7c71)
      (set! rdx #x7c7c7c7c7c7c7c72)
      (set! rbx #x7c7c7c7c7c7c7c73)
      (set! rbp #x7c7c7c7c7c7c7c75)
      (set! rsi #x7c7c7c7c7c7c7c76)
      (set! rdi #x7c7c7c7c7c7c7c77)
      (set! r8  #x7c7c7c7c7c7c7c78)
      (set! r9  #x7c7c7c7c7c7c7c79)
      (set! r10 #x7c7c7c7c7c7c7c7a)
      (set! r11 #x7c7c7c7c7c7c7c7b)
      (set! r12 #x7c7c7c7c7c7c7c7c)
      (set! r13 #x7c7c7c7c7c7c7c7d)
      (set! r14 #x7c7c7c7c7c7c7c7e)
      (set! r15 #x7c7c7c7c7c7c7c7f)
      (case frame-pointer-register
        [(rax) (set! rax 0)]
        [(rcx) (set! rcx 0)]
        [(rdx) (set! rdx 0)]
        [(rbx) (set! rbx 0)]
        [(rbp) (set! rbp 0)]
        [(rsi) (set! rsi 0)]
        [(rdi) (set! rdi 0)]
        [(r8) (set! r8 0)]
        [(r9) (set! r9 0)]
        [(r10) (set! r10 0)]
        [(r11) (set! r11 0)]
        [(r12) (set! r12 0)]
        [(r13) (set! r13 0)]
        [(r14) (set! r14 0)]
        [(r15) (set! r15 0)]
        [else (format-error who
                "unrecognized frame-pointer-register ~s"
                frame-pointer-register)])
      (case allocation-pointer-register
        [(rax) (set! rax heap-offset)]
        [(rcx) (set! rcx heap-offset)]
        [(rdx) (set! rdx heap-offset)]
        [(rbx) (set! rbx heap-offset)]
        [(rbp) (set! rbp heap-offset)]
        [(rsi) (set! rsi heap-offset)]
        [(rdi) (set! rdi heap-offset)]
        [(r8) (set! r8 heap-offset)]
        [(r9) (set! r9 heap-offset)]
        [(r10) (set! r10 heap-offset)]
        [(r11) (set! r11 heap-offset)]
        [(r12) (set! r12 heap-offset)]
        [(r13) (set! r13 heap-offset)]
        [(r14) (set! r14 heap-offset)]
        [(r15) (set! r15 heap-offset)]
        [else (format-error who
                "unrecognized allocation-pointer-register ~s"
                allocation-pointer-register)])
      (vector-fill! the-stack #x3b3b3b3b3b3b3b3b)
      (vector-fill! the-heap #x5d5d5d5d5d5d5d5d)))

  (reset-machine-state!))

;;; set related procedures

(define set?
  (lambda (ls)
    (or (null? ls)
        (and (not (memq (car ls) (cdr ls)))
             (set? (cdr ls))))))

(define set-cons
  (lambda (x set)
    (cond
      [(null? set) (list x)]
      [(eq? x (car set)) set]
      [else (cons (car set) (set-cons x (cdr set)))])))

(define union
  (case-lambda
    [(set1 set2)
     (let loop ([set1 set1])
       (cond
         [(null? set1) set2]
         [(memq (car set1) set2) (loop (cdr set1))]
         [else (cons (car set1) (loop (cdr set1)))]))]
    [() '()]
    [(set1 . sets)
     (let loop ([set1 set1] [sets sets])
       (if (null? sets)
           set1
           (loop (union set1 (car sets)) (cdr sets))))]))

(define intersection
  (case-lambda
    [(set1 set2)
     (let loop ([set1 set1])
       (cond
         [(null? set1) '()]
         [(memq (car set1) set2) (cons (car set1) (loop (cdr set1)))]
         [else (loop (cdr set1))]))]
    [(set1 . sets)
     (let loop ([set1 set1] [sets sets])
       (if (null? sets)
           set1
           (loop (intersection set1 (car sets)) (cdr sets))))]))

(define difference
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((memq (car set1) set2) (difference (cdr set1) set2))
      (else (cons (car set1) (difference (cdr set1) set2))))))

;;; integers

(define int32?
  (lambda (x)
    (and (and (integer? x) (exact? x))
         (<= (- (expt 2 31)) x (- (expt 2 31) 1)))))

(define int64?
  (lambda (x)
    (and (and (integer? x) (exact? x))
         (<= (- (expt 2 63)) x (- (expt 2 63) 1)))))

(define uint6?
  (lambda (x)
    (and (and (integer? x) (exact? x)) (<= 0 x 63))))

;;; unique variables

(define uvar?
  (lambda (x)
    (and (symbol? x)
      (let* ([s (symbol->string x)] [n (string-length s)])
        (define (s0 i)
          (and (not (fx= i -1))
            (cond
              [(char<=? #\0 (string-ref s i) #\9) (s1 (fx- i 1))]
              [else #f])))
        (define (s1 i)
          (and (not (fx= i -1))
            (let ([c (string-ref s i)])
              (cond
                [(char<=? #\1 (string-ref s i) #\9) (s1 (fx- i 1))]
                [(char=? c #\.) #t]
                [(char=? c #\0) (s2 (fx- i 1))]
                [else #f]))))
        (define (s2 i)
          (and (not (fx= i -1))
            (let ([c (string-ref s i)])
              (cond
                [(char<=? #\1 (string-ref s i) #\9) (s1 (fx- i 1))]
                [(char=? c #\0) (s2 (fx- i 1))]
                [else #f]))))
        (s0 (fx- n 1))))))

;;; frame variables

(define $fp-offset 0)

(define max-frame-var
  (make-parameter 100
    (let ([next 0])
      (lambda (n)
        (unless (and (fixnum? n) (fx>= n 0))
          (format-error 'max-frame-var "invalid max ~s" n))
        (when (fx>= n next)
          (do ([i next (fx+ i 1)])
              ((fx>= i n))
            (let ([fvi (string->symbol (format "fv~s" i))])
              (eval `(define-syntax ,fvi
                       (make-variable-transformer
                         (lambda (x)
                           (syntax-case x ()
                             [var (identifier? #'var)
                               (with-syntax ([fp (datum->syntax #'var frame-pointer-register)])
                                 #'(mref (- fp $fp-offset) ,(fxsll i align-shift)))]
                             [(set! var val)
                              (and (eq? (syntax->datum #'set!) 'set!) (identifier? #'var))
                              (with-syntax ([fp (datum->syntax #'var frame-pointer-register)])
                                #'(mset! (- fp $fp-offset)
                                    ,(fxsll i align-shift)
                                    val))]
                             [(var x ...)
                              (with-syntax ([fp (datum->syntax #'var frame-pointer-register)])
                                #'((mref (- fp $fp-offset) ,(fxsll i align-shift)) x ...))])))))
              (putprop fvi 'frame-index i)))
          (set! next n))
        n))))

(define frame-var?
  (lambda (x)
    (and (symbol? x) (getprop x 'frame-index #f) #t)))

(define frame-var->index
  (lambda (fv)
    (getprop fv 'frame-index)))

(define index->frame-var 
  (lambda (n)
    (when (> n (max-frame-var)) (max-frame-var n))
    (string->symbol (string-append "fv" (number->string n)))))

;;; labels

(define label?
  (lambda (x)
    (and (symbol? x)
      (let* ([s (symbol->string x)] [n (string-length s)])
        (define (s0 i)
          (and (not (fx= i -1))
            (cond
              [(char<=? #\0 (string-ref s i) #\9) (s1 (fx- i 1))]
              [else #f])))
        (define (s1 i)
          (and (not (fx= i -1))
            (let ([c (string-ref s i)])
              (cond
                [(char<=? #\1 (string-ref s i) #\9) (s1 (fx- i 1))]
                [(char=? c #\$) #t]
                [(char=? c #\0) (s2 (fx- i 1))]
                [else #f]))))
        (define (s2 i)
          (and (not (fx= i -1))
            (let ([c (string-ref s i)])
              (cond
                [(char<=? #\1 (string-ref s i) #\9) (s1 (fx- i 1))]
                [(char=? c #\0) (s2 (fx- i 1))]
                [else #f]))))
        (s0 (fx- n 1))))))

(define extract-root
  (lambda (sym)
    (list->string
      (let ([chars (string->list (symbol->string sym))])
        (define (s0 ls)
          (cond
            [(null? ls) chars]
            [(char-numeric? (car ls)) (s1 (cdr ls))]
            [else chars]))
        (define (s1 ls)
          (cond
            [(null? ls) chars]
            [(char-numeric? (car ls)) (s1 (cdr ls))]
            [(memv (car ls) '(#\. #\$)) (reverse (cdr ls))]
            [else chars]))
        (s0 (reverse chars))))))

(define extract-suffix
  (lambda (sym)
    (let ([str (symbol->string sym)])
      (let ([n (string-length str)]
            [m (string-length (extract-root sym))])
        (and (not (= n m))
             (substring str (+ m 1) n))))))

;;; unique-name produces a unique name derived the input name by
;;; adding a unique suffix of the form .<digit>+.  creating a unique
;;; name from a unique name has the effect of replacing the old
;;; unique suffix with a new one.
;;;
;;; unique-label produces a unique label derived from the input name.
;;; if the input name has the form of a unique name, it replaces the
;;; suffix ".nnn" with the suffix "$nnn", e.g., f.3 => f$3.  If the
;;; input name has the form of a unique label, the same label is
;;; returned, i.e., unique-name is the identity function on unique
;;; labels.  If the input name is not in either form, a new suffix
;;; is generated, e.g., f => f$3.
;;;
;;; unique-name-count is a parameter.  When called without arguments,
;;; it returns the value of the internal counter used to produce
;;; unique suffixes by unique-name and unique-label.  When called with
;;; one argument n, it sets the counter to n.  n must be a nonnegative
;;; exact integer.
;;;
;;; extract-suffix returns the numeric portion of the unique suffix
;;; of a unique name or label.  It returns #f if passed something other
;;; than a unique name or label.
(module (unique-name unique-name-count extract-suffix unique-label)
  (define count 1000)
  (define unique-suffix
    (lambda ()
      (set! count (+ count 1))
      (number->string count)))
  (define unique-name-count
    (case-lambda
      [() count]
      [(x)
       (unless (and (integer? x) (exact? x) (>= x 0))
         (format-error 'unique-name-count "invalid count ~s" count))
       (set! count x)]))
  (define extract-root
    (lambda (sym)
      (list->string
        (let ([chars (string->list (symbol->string sym))])
          (define (s0 ls)
            (cond
              [(null? ls) chars]
              [(char-numeric? (car ls)) (s1 (cdr ls))]
              [else chars]))
          (define (s1 ls)
            (cond
              [(null? ls) chars]
              [(char-numeric? (car ls)) (s1 (cdr ls))]
              [(memv (car ls) '(#\. #\$)) (reverse (cdr ls))]
              [else chars]))
          (s0 (reverse chars))))))
  (define extract-suffix
    (lambda (sym)
      (let ([str (symbol->string sym)])
        (let ([n (string-length str)]
              [m (string-length (extract-root sym))])
          (and (not (= n m))
               (substring str (+ m 1) n))))))
  (define unique-name
    (lambda (sym)
      (string->symbol
        (string-append (extract-root sym) "." (unique-suffix)))))
  (define unique-label
    (lambda (sym)
      (string->symbol
        (string-append
          (extract-root sym)
          "$"
          (let ([suffix (or (extract-suffix sym) (unique-suffix))])
            (substring suffix 0 (string-length suffix))))))))

(define label->x86-64-label
  (lambda (lab)
    (format "L~a" (extract-suffix lab))))

;;; operands

(define-record disp-opnd (reg offset))
(define-record index-opnd (breg ireg))
(module ()
  (record-writer (type-descriptor disp-opnd)
    (lambda (x p wr)
      (display-string "#<disp " p)
      (wr (disp-opnd-reg x) p)
      (display-string " " p)
      (wr (disp-opnd-offset x) p)
      (display-string ">" p)))

  (record-writer (type-descriptor index-opnd)
    (lambda (x p wr)
      (display-string "#<index " p)
      (wr (index-opnd-breg x) p)
      (display-string " " p)
      (wr (index-opnd-ireg x) p)
      (display-string ">" p)))

  (match-equality-test
    (lambda (x y)
      (cond
        [(disp-opnd? x)
         (and (disp-opnd? y)
           (eq? (disp-opnd-reg x) (disp-opnd-reg y))
           (= (disp-opnd-offset x) (disp-opnd-offset y)))]
        [(index-opnd? x)
         (and (index-opnd? y)
           (eq? (index-opnd-breg x) (index-opnd-breg y))
           (eq? (index-opnd-ireg x) (index-opnd-ireg y)))]
        [else (equal? x y)]))))

(define-who rand->x86-64-arg
  (lambda (rand)
    (define (register? x)
      (memq x '(rax rcx rdx rbx rsp rbp rsi rdi r8 r9 r10 r11 r12 r13 r14 r15)))
    (cond
      [(string? rand) rand] ; precooked operand
      [(number? rand)  (format "$~s" rand)]
      [(register? rand)  (format "%~s" rand)]
      [(label? rand) (format "~a(%rip)" (label->x86-64-label rand))]
      [(disp-opnd? rand) (format "~s(%~s)" (disp-opnd-offset rand) (disp-opnd-reg rand))]
      [(index-opnd? rand) (format "(%~s, %~s)" (index-opnd-breg rand) (index-opnd-ireg rand))]
      [else (format-error who "invalid instruction argument ~s" rand)])))

;;; emit routines

(define emit-label
  (lambda (label)
    (if (string? label)
        (printf "~a:\n" label)
        (printf "~a:\n" (label->x86-64-label label)))))

(define-syntax emit
  (syntax-rules ()
    [(_ opcode) (printf "    ~a\n" opcode)]
    [(_ opcode rand)
     (printf "    ~a ~a\n" opcode (rand->x86-64-arg rand))]
    [(_ opcode rand1 rand2)
     (printf "    ~a ~a, ~a\n" opcode
       (rand->x86-64-arg rand1)
       (rand->x86-64-arg rand2))]))

(define-syntax emit-jump
  (syntax-rules ()
    [(_ opcode ?target)
     (let ([target ?target])
       (if (label? target)
           (emit opcode (label->x86-64-label target))
           (emit opcode (format "*~a" (rand->x86-64-arg target)))))]))

(define-syntax emit-program
  (syntax-rules ()
    [(_ code code* ...)
     (begin
       (emit '.globl "_scheme_entry")
       (emit-label "_scheme_entry")
       (emit 'pushq 'rbx)
       (emit 'pushq 'rbp)
       (emit 'pushq 'r12)
       (emit 'pushq 'r13)
       (emit 'pushq 'r14)
       (emit 'pushq 'r15)
       (emit 'movq 'rdi frame-pointer-register)
       (emit 'movq 'rsi allocation-pointer-register)
       (emit 'leaq "_scheme_exit(%rip)" return-address-register)
       code code* ...
       (emit-label "_scheme_exit")
       (unless (eq? return-value-register 'rax)
         (emit 'movq return-value-register 'rax))
       (emit 'popq 'r15)
       (emit 'popq 'r14)
       (emit 'popq 'r13)
       (emit 'popq 'r12)
       (emit 'popq 'rbp)
       (emit 'popq 'rbx)
       (emit 'ret))]))

;;; other helpers for coding the compiler passes

(define make-begin
  (lambda (expr*)
    (define strip-begin
      (lambda (expr*)
        (match `(begin ,@expr*)
          [(begin ,[expr*] ...) (apply append expr*)]
          [,expr (list expr)])))    
    (match (strip-begin expr*)
      [(,x) x]
      [(,x ,x* ...) `(begin ,x ,x* ...)])))


;;; helpers for the scheme-dependent portion of the compiler

(define fixnum-bits 61)
(define shift-fixnum 3)
(define mask-fixnum #b111)
(define tag-fixnum #b000)

(define mask-pair #b111)
(define tag-pair #b001)
(define size-pair 16)
(define disp-car 0)
(define disp-cdr 8)

(define mask-vector #b111)
(define tag-vector  #b011)
(define disp-vector-length 0)
(define disp-vector-data 8)

(define mask-procedure #b111)
(define tag-procedure  #b010)
(define disp-procedure-code 0)
(define disp-procedure-data 8)

(define mask-boolean   #b11110111)
(define tag-boolean    #b00000110)

(define $false         #b00000110)
(define $true          #b00001110)
(define $nil           #b00010110)
(define $void          #b00011110)

(define fixnum-range?
  (lambda (n)
    (<= (- (expt 2 (- fixnum-bits 1)))
        n
        (- (expt 2 (- fixnum-bits 1)) 1))))


;; helper functions that is implemented by myself

(define (unzip list-of-pairs)
  (let loop ((lop list-of-pairs) (firsts '()) (seconds '()))
    (if (null? lop)
        (values (reverse firsts) (reverse seconds))
        (loop (cdr lop)
              (cons (caar lop) firsts)
              (cons (cadar lop) seconds)))))
(define (remove-duplicates ls)
      (letrec ([loop (lambda (in out)
                       (if (null? in)
                           (reverse out)
                           (let ([item (car in)])
                             (if (memq item out)
                                 (loop (cdr in) out)
                                 (loop (cdr in) (cons item out))))))])
        (loop ls '())))              

(define value-primitives
  '(+ - * car cdr cons make-vector vector-length vector-ref void))

(define predicate-primitives
  '(<= < = >= > boolean? eq? fixnum? null? pair? vector? procedure?))

(define effect-primitives
  '(set-car! set-cdr! vector-set!))


(define (pred-prim? op)
  (memq op predicate-primitives))

(define (value-prim? op)
  (memq op value-primitives))

(define (effect-prim? op)
  (memq op effect-primitives))

(define (make-nopless-begin expr*)
  (let ([filtered-expr* (remove '(nop) expr*)])
        (if (null? filtered-expr*)
        '(nop)
        (make-begin filtered-expr*))))
(define prim? 
  (lambda (x)
    (or (pred-prim? x) (value-prim? x) (effect-prim? x))))
(define (index-of obj lst)
    (let loop ((lst lst) (index 0))
        (cond
            ((null? lst) #f) 
            ((eq? obj (car lst)) index) 
            (else (loop (cdr lst) (+ index 1))) 
        )
    )
)