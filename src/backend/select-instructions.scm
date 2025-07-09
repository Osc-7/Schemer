(define select-instructions
  (lambda (program)

    (define (is-operand-register? op) (or (register? op) (uvar? op)))
    (define (is-operand-memory? op) (or (frame-var? op) (disp-opnd? op)))
    (define (is-operand-immediate? op) (integer? op))
    (define (is-commutative? op) (memq op '(+ * logand logor)))

    (define (map-walk walk-fn expr-list)
      (if (null? expr-list)
          (values '() '())
          (let-values ([(new-hd u1) (walk-fn (car expr-list))])
            (let-values ([(new-tl u2) (map-walk walk-fn (cdr expr-list))])
              (values (cons new-hd new-tl) (union u1 u2))))))

    (define (walk-tail tail)
      (match tail
        [(if ,p ,t ,e)
         (let-values ([(new-p u1) (walk-pred p)])
           (cond
             [(eq? new-p #t) (walk-tail t)]
             [(eq? new-p #f) (walk-tail e)]
             [else
              (let-values ([(new-t u2) (walk-tail t)])
                (let-values ([(new-e u3) (walk-tail e)])
                  (values `(if ,new-p ,new-t ,new-e) (union u1 u2 u3))))]))]
        [(begin ,effects ... ,last)
         (let-values ([(new-effects u1) (map-walk walk-effect effects)])
           (let-values ([(new-last u2) (walk-tail last)])
             (values (make-begin (append new-effects (list new-last))) (union u1 u2))))]
        [,else (values else '())]))

    (define (walk-pred pred)
      (match pred
        [#t (values #t '())]
        [#f (values #f '())]
        [(if ,p1 ,p2 ,p3)
         (let-values ([(new-p1 u1) (walk-pred p1)])
           (let-values ([(new-p2 u2) (walk-pred p2)])
             (let-values ([(new-p3 u3) (walk-pred p3)])
               (values `(if ,new-p1 ,new-p2 ,new-p3) (union u1 u2 u3)))))]
        [(begin ,effects ... ,last)
         (let-values ([(new-effects u1) (map-walk walk-effect effects)])
           (let-values ([(new-last u2) (walk-pred last)])
             (values (make-begin (append new-effects (list new-last))) (union u1 u2))))]
        
        [(,relop ,triv1 ,triv2)
        ;  (display `(walk-pred ,relop ,triv1 ,triv2) (newline))
         (cond
          ;; Case 5: Both operands are immediates — evaluate at compile time
          [(and (is-operand-immediate? triv1) (is-operand-immediate? triv2))
           (let ([result (case relop
                           [(=) (if (= triv1 triv2) #t #f)]
                           [(<) (if (< triv1 triv2) #t #f)]
                           [(>) (if (> triv1 triv2) #t #f)]
                           [(<=) (if (<= triv1 triv2) #t #f)]
                           [(>=) (if (>= triv1 triv2) #t #f)]
                           [else (error "Unknown relop" relop)])])
             (values result '()))]
           ;; Case 1: triv2 is a large immediate. Return a (begin ...) block as the new Pred.
           [(and (is-operand-immediate? triv2) (not (int32? triv2)))
            (let ([tmp (unique-name 't)])
              (let-values ([(new-set u1) (walk-effect `(set! ,tmp ,triv2))])
                (values `(begin ,new-set (,relop ,triv1 ,tmp)) (union (list tmp) u1))))]

           ;; Case 2: triv1 is a large immediate.
           [(and (is-operand-immediate? triv1) (not (int32? triv1)))
            (let ([tmp (unique-name 't)])
              (let-values ([(new-set u1) (walk-effect `(set! ,tmp ,triv1))])
                (values `(begin ,new-set (,relop ,tmp ,triv2)) (union (list tmp) u1))))]

           ;; Case 3: Both operands are memory.
           [(and (is-operand-memory? triv1) (is-operand-memory? triv2))
            (let ([tmp (unique-name 't)])
              (let-values ([(new-set u1) (walk-effect `(set! ,tmp ,triv1))])
                (values `(begin ,new-set (,relop ,tmp ,triv2)) (union (list tmp) u1))))]

          ;; Case 4: Optimization to swap small immediate and non-immediate.
          [(and (is-operand-immediate? triv1)
                (int32? triv1)
                (not (is-operand-immediate? triv2)))  ; allow labels, registers, memory
           (let ([new-relop (case relop ((<) '>) ((<=) '>=) ((>) '<) ((>=) '<=) ((=) '=))])
             (values `(,new-relop ,triv2 ,triv1) '()))]
            
           ;; Default case: Operands are valid.
           [else (values pred '())])]
        [,else (values pred '())]))
    
    (define (walk-effect effect)
      (match effect
        [(nop) (values effect '())]
        
        [(return-point ,label ,tail)
          (let-values ([(new-tail u) (walk-tail tail)])
            (values `(return-point ,label ,new-tail) u))] 

        [(if ,p ,t ,e)
         (let-values ([(new-p u1) (walk-pred p)])
           (cond
             [(eq? new-p #t) (walk-effect t)]
             [(eq? new-p #f) (walk-effect e)]
             [else
              (let-values ([(new-t u2) (walk-effect t)])
                (let-values ([(new-e u3) (walk-effect e)])
                  (values `(if ,new-p ,new-t ,new-e) (union u1 u2 u3))))]))]
        [(begin ,effects ...)
         (let-values ([(new-effects u) (map-walk walk-effect effects)])
           (values (make-begin new-effects) u))]
        
        [(set! ,var (,binop ,triv1 ,triv2))
         (cond
           [(and (eq? binop '*) (is-operand-memory? var))
            (let ([tmp (unique-name 't)])
              (let ([instr1 `(set! ,tmp ,triv1)]
                    [instr2 `(set! ,tmp (* ,tmp ,triv2))]
                    [instr3 `(set! ,var ,tmp)])
                (let-values ([(n-i1 u1) (walk-effect instr1)])
                  (let-values ([(n-i2 u2) (walk-effect instr2)])
                    (let-values ([(n-i3 u3) (walk-effect instr3)])
                      (values (make-begin (list n-i1 n-i2 n-i3))
                              (union (list tmp) u1 u2 u3)))))))]
           [(is-operand-register? var)
             (cond
              [(equal? var triv1)
               (cond
                 [(and (is-operand-immediate? triv2) (not (int32? triv2)))
                  (let ([tmp (unique-name 't)])
                    (let-values ([(n-i1 u1) (walk-effect `(set! ,tmp ,triv2))])
                      (let-values ([(n-i2 u2) (walk-effect `(set! ,var (,binop ,var ,tmp)))])
                        (values (make-begin (list n-i1 n-i2))
                                (union (list tmp) u1 u2)))))]
                 [(or (is-operand-register? triv2) (is-operand-memory? triv2) (int32? triv2)) (values effect '())]
                 [else (let ([tmp (unique-name 't)]) (let-values ([(n-i1 u1) (walk-effect `(set! ,tmp ,triv2))]) (let-values ([(n-i2 u2) (walk-effect `(set! ,var (,binop ,var ,tmp)))]) (values (make-begin (list n-i1 n-i2)) (union (list tmp) u1 u2)))))])]
              [(and (is-commutative? binop) (equal? var triv2))
               (if (or (is-operand-register? triv1) (is-operand-memory? triv1) (int32? triv1))
                   (walk-effect `(set! ,var (,binop ,triv2 ,triv1)))
                   (let ([tmp (unique-name 't)]) (let-values ([(n-i1 u1) (walk-effect `(set! ,tmp ,triv1))]) (let-values ([(n-i2 u2) (walk-effect `(set! ,var (,binop ,var ,tmp)))]) (values (make-begin (list n-i1 n-i2)) (union (list tmp) u1 u2))))))]
              [(and (not (is-commutative? binop)) (equal? var triv2))
               (let ([tmp (unique-name 't)]) (let-values ([(n-i1 u1) (walk-effect `(set! ,tmp ,var))]) (let-values ([(n-i2 u2) (walk-effect `(set! ,var ,triv1))]) (let-values ([(n-i3 u3) (walk-effect `(set! ,var (,binop ,var ,tmp)))]) (values (make-begin (list n-i1 n-i2 n-i3)) (union (list tmp) u1 u2 u3))))))]
              [else (cond
                 [(and (is-operand-immediate? triv2) (not (int32? triv2)))
                  (let ([tmp (unique-name 't)]) (let-values ([(n-i1 u1) (walk-effect `(set! ,tmp ,triv2))]) (let-values ([(n-i2 u2) (walk-effect `(set! ,var ,triv1))]) (let-values ([(n-i3 u3) (walk-effect `(set! ,var (,binop ,var ,tmp)))]) (values (make-begin (list n-i1 n-i2 n-i3)) (union (list tmp) u1 u2 u3))))))]
                 [(and (is-operand-immediate? triv1) (not (int32? triv1)) (is-commutative? binop))
                  (walk-effect `(set! ,var (,binop ,triv2 ,triv1)))]
                 [else (let-values ([(n-i1 u1) (walk-effect `(set! ,var ,triv1))]) (let-values ([(n-i2 u2) (walk-effect `(set! ,var (,binop ,var ,triv2)))]) (values (make-begin (list n-i1 n-i2)) (union u1 u2))))])]
             )]
           [(is-operand-memory? var)
             (cond
              [(and (equal? var triv1) (or (is-operand-register? triv2) (int32? triv2))) (values effect '())]
              [(and (equal? var triv1) (is-operand-immediate? triv2) (not (int32? triv2)))
               (let ([tmp (unique-name 't)]) (let-values ([(n-i1 u1) (walk-effect `(set! ,tmp ,triv2))]) (let-values ([(n-i2 u2) (walk-effect `(set! ,var (,binop ,var ,tmp)))]) (values (make-begin (list n-i1 n-i2)) (union (list tmp) u1 u2)))))]
              [else (let ([tmp1 (unique-name 't)])
                 (cond
                   [(and (is-operand-immediate? triv2) (not (int32? triv2)))
                    (let ([tmp2 (unique-name 't)]) (let-values ([(n-i1 u1) (walk-effect `(set! ,tmp1 ,triv1))]) (let-values ([(n-i2 u2) (walk-effect `(set! ,tmp2 ,triv2))]) (let-values ([(n-i3 u3) (walk-effect `(set! ,tmp1 (,binop ,tmp1 ,tmp2)))]) (let-values ([(n-i4 u4) (walk-effect `(set! ,var ,tmp1))]) (values (make-begin (list n-i1 n-i2 n-i3 n-i4)) (union (list tmp1 tmp2) u1 u2 u3 u4)))))))]
                   [(and (is-operand-immediate? triv1) (not (int32? triv1)) (is-commutative? binop))
                    (walk-effect `(set! ,var (,binop ,triv2 ,triv1)))]
                   [else (let ([tmp1 (unique-name 't)]) (let-values ([(n-i1 u1) (walk-effect `(set! ,tmp1 ,triv1))]) (let-values ([(n-i2 u2) (walk-effect `(set! ,tmp1 (,binop ,tmp1 ,triv2)))]) (let-values ([(n-i3 u3) (walk-effect `(set! ,var ,tmp1))]) (values (make-begin (list n-i1 n-i2 n-i3)) (union (list tmp1) u1 u2 u3))))))]
                 ))]
             )]
           [else (values effect '())]
           )]
        [(set! ,var ,triv)
         (cond
           [(and (is-operand-memory? var) (is-operand-memory? triv))
            (let ([tmp (unique-name 't)]) (let-values ([(n-i1 u1) (walk-effect `(set! ,tmp ,triv))]) (let-values ([(n-i2 u2) (walk-effect `(set! ,var ,tmp))]) (values (make-begin (list n-i1 n-i2)) (union (list tmp) u1 u2)))))]
           [(and (is-operand-memory? var) (label? triv))
            (let ([tmp (unique-name 't)]) (let-values ([(n-i1 u1) (walk-effect `(set! ,tmp ,triv))]) (let-values ([(n-i2 u2) (walk-effect `(set! ,var ,tmp))]) (values (make-begin (list n-i1 n-i2)) (union (list tmp) u1 u2)))))]
           [(and (not (is-operand-register? var)) (is-operand-immediate? triv) (not (int32? triv)))
            (let ([tmp (unique-name 't)]) (let-values ([(n-i1 u1) (walk-effect `(set! ,tmp ,triv))]) (let-values ([(n-i2 u2) (walk-effect `(set! ,var ,tmp))]) (values (make-begin (list n-i1 n-i2)) (union (list tmp) u1 u2)))))]
           [else (values effect '())])]
        [,else (values else '())]))

    (define (process-body body)
      (match body
        [(locate ,bindings ,tail)
         body]
        [(locals ,l-vars (ulocals ,u-vars (locate ,loc-binds (frame-conflict ,graph ,tail))))
         (let-values ([(new-tail new-uvars) (walk-tail tail)])
           `(locals ,l-vars (ulocals ,(union u-vars new-uvars) (locate ,loc-binds (frame-conflict ,graph ,new-tail)))))]
        [,else (error 'select-instructions "Invalid body structure" body)]))

    (match program
      [(letrec ,bindings ,main-body)
       (let ([new-main-body (process-body main-body)]
             [new-bindings (map (lambda (binding)
                                  (match binding
                                    [(,label (lambda () ,body))
                                     `(,label (lambda () ,(process-body body)))]))
                                bindings)])
         `(letrec ,new-bindings ,new-main-body))]
      [,else program])))