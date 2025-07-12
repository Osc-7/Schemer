(define discard-call-live
  (lambda (program)

    (define (is-binop? x) (memq x '(+ - * sra logand logor)))
    (define (is-relop? x) (memq x '(< <= = >= >)))

    ;; This is the pattern that matches a procedure call but NOT a primitive.
    (define (is-a-call? rator rands)
      (and  (symbol? rator)
            (not (null? rands))
            (not (is-binop? rator))
            (not (is-relop? rator))
            (not (memq rator '(alloc mref mset!)))))

    (define (walk-tail tail)
      (match tail
        [(if ,p ,c ,a) `(if ,(walk-pred p) ,(walk-tail c) ,(walk-tail a))]
        [(begin ,e ... ,t) `(begin ,@(map walk-effect e) ,(walk-tail t))]
        [(,rator . ,rands) (guard (is-a-call? rator rands)) `(,rator)]
        [,else else]))

    (define (walk-effect effect)
      (match effect
        [(return-point ,l ,t) `(return-point ,l ,(walk-tail t))]
        [(if ,p ,c ,a) `(if ,(walk-pred p) ,(walk-effect c) ,(walk-effect a))]
        [(begin ,e ...) `(begin ,@(map walk-effect e))]
        [(set! ,var ,val) `(set! ,var ,(walk-value val))]
        [(,rator . ,rands) (guard (is-a-call? rator rands)) `(,rator)]
        [,else else]))

    (define (walk-pred pred)
      (match pred
        [(if ,p ,c ,a) `(if ,(walk-pred p) ,(walk-pred c) ,(walk-pred a))]
        [(begin ,e ... ,p) `(begin ,@(map walk-effect e) ,(walk-pred p))]
        [,else pred]))

    (define (walk-value val)
      (match val
        [(,rator . ,rands) (guard (is-a-call? rator rands)) `(,rator)]
        [,else val]))

    (define (process-body body)
      (match body
        ;; The input structure is the output of the iterate block
        [(locals ,l (ulocals ,u (locate ,loc (frame-conflict ,fc (register-conflict ,rc ,tail)))))
         `(locals ,l (ulocals ,u (locate ,loc (frame-conflict ,fc (register-conflict ,rc ,(walk-tail tail))))))]
        ;; Fallback for simpler structures during testing
        [(locate ,bindings ,tail)
         `(locate ,bindings ,(walk-tail tail))]
        [,else else]))

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