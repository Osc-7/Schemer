(define finalize-frame-locations
  (lambda (program)

    (define (process-body body)
      (match body

        [(locals ,locals-vars
           (ulocals ,ulocals-vars
             (locate ,locate-bindings
               (frame-conflict ,frame-graph ,tail))))
         
         (let ([new-tail (substitute tail locate-bindings)])
           ;; 重建 body，只替换 tail，其他结构保持不变
           `(locals ,locals-vars
              (ulocals ,ulocals-vars
                (locate ,locate-bindings
                  (frame-conflict ,frame-graph ,new-tail)))))]

        [(locals ,_ (ulocals ,_ (locate . _))) body]
        [(locate ,a ,b) body]

        [,else (error 'finalize-frame-locations "Invalid body structure" body)]))

(define (substitute expr env)
  (define (lookup var)
    (let ([binding (assq var env)])
      (if binding (cadr binding) var)))

  (match expr
    ;; 基本情况: 原子表达式
    [,x (guard (symbol? x)) (lookup x)]
    [,x (guard (not (pair? x))) x]

    [(if ,pred ,consequent ,alternative)
     `(if ,(substitute pred env)
          ,(substitute consequent env)
          ,(substitute alternative env))]

    [(begin . ,body)
     `(begin ,@(map (lambda (e) (substitute e env)) body))]

    [(set! ,var ,val)
     (let ([new-var (substitute var env)]
           [new-val (substitute val env)])
       (if (equal? new-var new-val)
           '(nop)
           `(set! ,new-var ,new-val)))]

    [(alloc ,size)
     `(alloc ,(substitute size env))]

    [(mref ,base ,offset)
     `(mref ,(substitute base env) ,(substitute offset env))]

    [(mset! ,base ,offset ,val)
     `(mset! ,(substitute base env) ,(substitute offset env) ,(substitute val env))]

    [(return-point ,label ,tail)
      `(return-point ,label ,(substitute tail env))]
      
    ;; 通用的函数/操作符调用规则
    ;; 这应该放在所有特定结构规则之后
    [(,rator . ,rands)
     (cons (substitute rator env)
           (map (lambda (rand) (substitute rand env)) rands))]

    [,else (error 'substitute "Unhandled expression form" expr)]))

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