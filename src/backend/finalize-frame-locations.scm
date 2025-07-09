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
        ;; Base Case: 如果是符号, 查找并替换
        [,x (guard (symbol? x)) (lookup x)]

        ;; Base Case: 非符号、非列表，直接返回
        [,x (guard (not (pair? x))) x]

        [(return-point ,label ,tail)
          `(return-point ,label ,(substitute tail env))]
          
        [(set! ,var ,val)
         (let ([new-var (substitute var env)]
               [new-val (substitute val env)])
           (if (equal? new-var new-val)
               '(nop)
               `(set! ,new-var ,new-val)))]

        [(,rator . ,rands) (guard (symbol? rator))
        (cons rator (substitute rands env))]
        
        ;; 递归处理列表
        [(,car-expr . ,cdr-expr)
         (cons (substitute car-expr env)
               (substitute cdr-expr env))]
          

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