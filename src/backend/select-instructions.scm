(define select-instructions
  (lambda (program)


    ;; A helper to check if an operand is in a register or is a uvar
    ;; that is expected to be allocated to a register.
    (define (is-operand-register? op)
      (or (register? op) (uvar? op)))

    ;; A helper to check if an operand is in memory.
    (define (is-operand-memory? op)
      (or (frame-var? op) (disp-opnd? op)))

    (define (is-operand-immediate? op)
      (integer? op))
      
    ;; A helper to process a list of expressions (like effects) with a 
    ;; function that returns multiple values. This replaces `(unzip (map ...))`.
    (define (map-walk walk-fn expr-list)
      (if (null? expr-list)
          (values '() '())
          (let-values ([(new-hd hd-uvars) (walk-fn (car expr-list))])
            (let-values ([(new-tl tl-uvars) (map-walk walk-fn (cdr expr-list))])
              (values (cons new-hd new-tl) (union hd-uvars tl-uvars))))))


    (define (walk-tail tail)
      (match tail
        [(if ,pred ,then-t ,else-t)
         (let-values ([(new-pred pred-uvars) (walk-pred pred)])
           (let-values ([(new-then then-uvars) (walk-tail then-t)])
             (let-values ([(new-else else-uvars) (walk-tail else-t)])
               (values `(if ,new-pred ,new-then ,new-else)
                       (union pred-uvars then-uvars else-uvars)))))]

        [(begin ,effects ... ,last-tail)
         (let-values ([(new-effects effects-uvars) (map-walk walk-effect effects)])
           (let-values ([(new-tail tail-uvars) (walk-tail last-tail)])
             (values (make-begin (append new-effects (list new-tail)))
                     (union effects-uvars tail-uvars))))]
        
        [,else (values else '())]))

    (define (walk-pred pred)
      (match pred
        [(if ,p1 ,p2 ,p3)
         (let-values ([(new-p1 u1) (walk-pred p1)])
           (let-values ([(new-p2 u2) (walk-pred p2)])
             (let-values ([(new-p3 u3) (walk-pred p3)])
               (values `(if ,new-p1 ,new-p2 ,new-p3) (union u1 u2 u3)))))]
        
        [(begin ,effects ... ,last-pred)
         (let-values ([(new-effects effects-uvars) (map-walk walk-effect effects)])
           (let-values ([(new-pred pred-uvars) (walk-pred last-pred)])
             (values `(begin ,@new-effects ,new-pred)
                     (union effects-uvars pred-uvars))))]

        [(,relop ,triv1 ,triv2)
         (cond
           [(and (is-operand-memory? triv1) (is-operand-memory? triv2))
            (let ([tmp (unique-name 't)])
              (values `(begin (set! ,tmp ,triv1) (,relop ,tmp ,triv2))
                      (list tmp)))]
           [(and (is-operand-immediate? triv1) (not (is-operand-immediate? triv2)))
            (let ([new-relop (case relop [(<) '>] [(<=) '>=] [(>) '<] [(>=) '<=] [(=) '=])])
              (if (eq? new-relop '=)
                  (values pred '())
                  (values `(,new-relop ,triv2 ,triv1) '())))]
           [else (values pred '())])]

        [,else (values pred '())]))

    (define (walk-effect effect)
      (match effect
        [(nop) (values effect '())]

        [(if ,pred ,then-e ,else-e)
         (let-values ([(new-pred u1) (walk-pred pred)])
           (let-values ([(new-then u2) (walk-effect then-e)])
             (let-values ([(new-else u3) (walk-effect else-e)])
               (values `(if ,new-pred ,new-then ,new-else) (union u1 u2 u3)))))]

        [(begin ,effects ...)
         (let-values ([(new-effects uvars) (map-walk walk-effect effects)])
           (values (make-begin new-effects) uvars))]

        [(set! ,var ,triv)
         (cond
           [(and (is-operand-memory? var) (is-operand-memory? triv))
            (let ([tmp (unique-name 't)])
              (values `(begin (set! ,tmp ,triv) (set! ,var ,tmp)) (list tmp)))]
           [(and (is-operand-memory? var) (label? triv))
            (let ([tmp (unique-name 't)])
              (values `(begin (set! ,tmp ,triv) (set! ,var ,tmp)) (list tmp)))]
           [(and (not (is-operand-register? var)) (integer? triv) (not (int32? triv)))
            (let ([tmp (unique-name 't)])
              (values `(begin (set! ,tmp ,triv) (set! ,var ,tmp)) (list tmp)))]
           [else (values effect '())])]

        [(set! ,var (,binop ,triv1 ,triv2))
         (cond
           [(eq? binop 'sra)
            (if (is-operand-immediate? triv2)
                (if (is-operand-memory? var)
                    (let ([tmp (unique-name 't)])
                      (values `(begin (set! ,tmp ,triv1) (set! ,tmp (sra ,tmp ,triv2)) (set! ,var ,tmp))
                              (list tmp)))
                    (values effect '()))
                (let ([tmp (unique-name 't)])
                  (values `(begin (set! ,tmp ,triv2) (set! ,var (sra ,triv1 ,tmp)))
                          (list tmp))))]
           [(and (eq? binop '*) (not (is-operand-register? var)))
            (let ([tmp (unique-name 't)])
              (values `(begin (set! ,tmp ,triv1) (set! ,tmp (* ,tmp ,triv2)) (set! ,var ,tmp))
                      (list tmp)))]
           [(and (memq binop '(+ * logand logor)) (is-operand-memory? triv1) (is-operand-register? triv2))
            (walk-effect `(set! ,var (,binop ,triv2 ,triv1)))]
           [(and (is-operand-memory? triv1) (is-operand-memory? triv2))
            (let ([tmp (unique-name 't)])
              (values `(begin (set! ,tmp ,triv1) (set! ,tmp (,binop ,tmp ,triv2)) (set! ,var ,tmp))
                      (list tmp)))]
           [(and (is-operand-memory? var) (not (equal? var triv1)))
             (let ([tmp (unique-name 't)])
               (values `(begin (set! ,tmp ,triv1) (set! ,tmp (,binop ,tmp ,triv2)) (set! ,var ,tmp))
                       (list tmp)))]
           [(and (not (is-operand-register? triv1)) (not (is-operand-register? triv2)))
            (let ([tmp (unique-name 't)])
              (values `(begin (set! ,tmp ,triv2) (set! ,var (,binop ,triv1 ,tmp)))
                      (list tmp)))]
           [else (values effect '())])]
           
        [,else (values else '())]))

    (define (process-body body)
      (match body
        [(locate . _) body] 
        [(locals ,locals-vars
           (ulocals ,ulocals-vars
             (locate ,locate-bindings
               (frame-conflict ,graph ,tail))))
         (let-values ([(new-tail new-ulocals) (walk-tail tail)])
            `(locals ,locals-vars
               (ulocals ,(union ulocals-vars new-ulocals)
                 (locate ,locate-bindings
                   (frame-conflict ,graph ,new-tail)))))]
        [,else body]))

    (define (process-binding binding)
      (match binding
        [(,label (lambda () ,body))
         `(,label (lambda () ,(process-body body)))]))
        
    (match program
      [(letrec ,bindings ,main-body)
       (let ([new-main-body (process-body main-body)]
             [new-bindings (map process-binding bindings)])
         `(letrec ,new-bindings ,new-main-body))]
      [,else program])))