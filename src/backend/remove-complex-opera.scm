(define remove-complex-opera*
  (lambda (program)
    (define (Triv? x) (or (symbol? x) (integer? x)))
    (define (is-binop? x) (memq x '(+ - * sra logand logor)))
    (define (is-relop? x) (memq x '(< <= = >= >)))
    (define (is-memop? x) (memq x '(alloc mref mset!)))
    (define (is-a-call? x) (and (symbol? x) (not (is-memop? x)) (not (is-binop? x)) (not (is-relop? x))))
    (define (value->triv v)
      (let-values ([(new-v v-vars) (walk-value v)])
        (if (Triv? new-v)
            (values '() new-v v-vars)
            (let ([tmp (unique-name 'tmp)])
              (values `((set! ,tmp ,new-v))
                      tmp
                      (append v-vars (list tmp)))))))

    (define (values->trivs vs)
      (if (null? vs)
          (values '() '() '())
          (let-values ([(b1 t1 v1) (value->triv (car vs))])
            (let-values ([(b2 t2 v2) (values->trivs (cdr vs))])
              (values (append b1 b2) (cons t1 t2) (append v1 v2))))))
              
    (define (map-values-and-append f ls)
      (if (null? ls)
          (values '() '())
          (let-values ([(h-item h-vars) (f (car ls))])
            (let-values ([(t-items t-vars) (map-values-and-append f (cdr ls))])
              (values (cons h-item t-items)
                      (append h-vars t-vars))))))

    (define (walk-tail tail)
      (match tail
        [(if ,p ,c ,a)
         (let-values ([(new-p p-vars) (walk-pred p)])
           (let-values ([(new-c c-vars) (walk-tail c)])
             (let-values ([(new-a a-vars) (walk-tail a)])
               (values `(if ,new-p ,new-c ,new-a)
                       (append p-vars c-vars a-vars)))))]

        [(begin ,effects ... ,last-tail)
         (let-values ([(new-effects effects-vars) (map-values-and-append walk-effect effects)])
           (let-values ([(new-tail tail-vars) (walk-tail last-tail)])
             (values (make-begin (append new-effects (list new-tail)))
                     (append effects-vars tail-vars))))]

        [(,op ,v1 ,v2) (guard (is-binop? op))
         (let-values ([(binds triv new-vars) (value->triv tail)])
           (values (make-begin (append binds (list triv)))
                   new-vars))]

        [(,rator ,rands ...)
         (let-values ([(rator-binds new-rator rator-vars) (value->triv rator)])
           (let-values ([(rands-binds new-rands rands-vars) (values->trivs rands)])
             (values (make-begin (append rator-binds rands-binds (list `(,new-rator ,@new-rands))))
                     (append rator-vars rands-vars))))]
        [,triv (guard (Triv? triv))
               (values triv '())]
        [,else-tail (error 'walk-tail "Invalid Tail expression" else-tail)]))

    (define (walk-pred pred)
      (match pred
        [(true) (values '(true) '())]
        [(false) (values '(false) '())]
        [(if ,p1 ,p2 ,p3)
         (let-values ([(new-p1 p1-vars) (walk-pred p1)])
           (let-values ([(new-p2 p2-vars) (walk-pred p2)])
             (let-values ([(new-p3 p3-vars) (walk-pred p3)])
               (values `(if ,new-p1 ,new-p2 ,new-p3)
                       (append p1-vars p2-vars p3-vars)))))]
        [(begin ,effects ... ,last-pred)
         (let-values ([(new-effects effects-vars) (map-values-and-append walk-effect effects)])
           (let-values ([(new-pred pred-vars) (walk-pred last-pred)])
             (values (make-begin (append new-effects (list new-pred)))
                     (append effects-vars pred-vars))))]
        [(,op ,v1 ,v2) (guard (is-relop? op))
         (let-values ([(b1 t1 v1-vars) (value->triv v1)])
           (let-values ([(b2 t2 v2-vars) (value->triv v2)])
             (values (make-begin (append b1 b2 (list `(,op ,t1 ,t2))))
                     (append v1-vars v2-vars))))]
        [,else-pred (error 'walk-pred "Invalid Predicate expression" else-pred)]))

    (define (walk-effect effect)
      (match effect
        [(nop) (values '(nop) '())]
        [(set! ,x ,v)
         (guard (symbol? x))
         (let-values ([(new-v v-vars) (walk-value v)])
           (values `(set! ,x ,new-v) v-vars))]

        [(mset! ,base ,offset ,val)
         (let-values ([(binds trivs vars) (values->trivs (list base offset val))])
           (values (make-begin (append binds (list `(mset! ,@trivs))))
                   vars))]

        [(if ,p ,c ,a)
         (let-values ([(new-p p-vars) (walk-pred p)])
           (let-values ([(new-c c-vars) (walk-effect c)])
             (let-values ([(new-a a-vars) (walk-effect a)])
               (values `(if ,new-p ,new-c ,new-a)
                       (append p-vars c-vars a-vars)))))]
        [(begin ,effects ... ,last-effect)
         (let-values ([(new-effects effects-vars) (map-values-and-append walk-effect effects)])
           (let-values ([(new-last-effect last-effect-vars) (walk-effect last-effect)])
             (values (make-begin (append new-effects (list new-last-effect)))
                     (append effects-vars last-effect-vars))))]
        ;; ++ A7: Handle non-tail calls in Effect context.
        [(,rator ,rands ...) 
         (let-values ([(rator-binds new-rator rator-vars) (value->triv rator)])
           (let-values ([(rands-binds new-rands rands-vars) (values->trivs rands)])
             (values (make-begin (append rator-binds rands-binds (list `(,new-rator ,@new-rands))))
                     (append rator-vars rands-vars))))]

        [,else-effect (error 'walk-effect "Invalid Effect expression" else-effect)]))

    (define (walk-value value)
      (match value
        [,triv (guard (Triv? triv))
               (values triv '())]

        [(,op ,v1 ,v2) (guard (is-binop? op))
        ;; 将两个操作数打包成列表，统一交给 values->trivs 处理
        (let-values ([(binds trivs vars) (values->trivs (list v1 v2))])
          ;; trivs 是一个列表，如 '(triv1 triv2)，我们需要把它解开
          (values (make-begin (append binds (list `(,op ,(car trivs) ,(cadr trivs)))))
                  vars))]


        [(alloc ,val)
         (let-values ([(binds triv vars) (value->triv val)])
           (values (make-begin (append binds (list `(alloc ,triv))))
                   vars))]
        [(mref ,base ,offset)
         (let-values ([(binds trivs vars) (values->trivs (list base offset))])
           (values (make-begin (append binds (list `(mref ,@trivs))))
                   vars))]
        
        [(if ,p ,c ,a)
         (let-values ([(new-p p-vars) (walk-pred p)])
           (let-values ([(new-c c-vars) (walk-value c)])
             (let-values ([(new-a a-vars) (walk-value a)])
               (values `(if ,new-p ,new-c ,new-a)
                       (append p-vars c-vars a-vars)))))]

        [(begin ,effects ... ,last-val)
         (let-values ([(new-effects effects-vars) (map-values-and-append walk-effect effects)])
           (let-values ([(new-last-val val-vars) (walk-value last-val)])
             (values (make-begin (append new-effects (list new-last-val)))
                     (append effects-vars val-vars))))]
        ;; ++ A7: Handle non-tail calls in Value context.
        [(,rator ,rands ...)
        (let-values ([(rator-binds new-rator rator-vars) (value->triv rator)])
          (let-values ([(rands-binds new-rands rands-vars) (values->trivs rands)])
            (values (make-begin (append rator-binds rands-binds (list `(,new-rator ,@new-rands))))
                    (append rator-vars rands-vars))))]

        ; [(,rator ,rands ...) (guard (is-a-call? rator))
        ;  (let ([tmp (unique-name 'tmp)])
        ;    (let-values ([(rator-binds new-rator rator-vars) (value->triv rator)])
        ;      (let-values ([(rands-binds new-rands rands-vars) (values->trivs rands)])
        ;        (values (make-begin (append rator-binds rands-binds (list `(set! ,tmp (,new-rator ,@new-rands)))))
        ;                (append rator-vars rands-vars (list tmp))))))]

        [,else-value (error 'walk-value "Invalid Value expression" else-value)]))

    (define (process-body body)
      (match body
        [(locals ,vars ,tail)
         (guard (list? vars))
         (let-values ([(new-tail new-vars) (walk-tail tail)])
           `(locals (,@vars ,@(remove-duplicates new-vars)) ,new-tail))]
        [,tail
         (let-values ([(new-tail new-vars) (walk-tail tail)])
           `(locals (,@(remove-duplicates new-vars)) ,new-tail))]
        [,else (error 'process-body "invalid body structure" else)]))

    (match program
      [(letrec ,bindings ,main-body)
       (let ([new-main-body (process-body main-body)]
             [new-bindings (map (lambda (binding)
                                  (match binding
                                    [(,label (lambda ,formals ,body))
                                     `(,label (lambda ,formals ,(process-body body)))]
                                    [,else (error 'program "invalid binding" else)]))
                                bindings)])
         `(letrec ,new-bindings ,new-main-body))]
      [,body (process-body body)])))