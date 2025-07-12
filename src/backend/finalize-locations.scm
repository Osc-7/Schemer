(define finalize-locations
  (lambda (program)
    (define substitute
      (lambda (expr env)
        (match expr
          [,x (guard (symbol? x))
              (let ([binding (assq x env)])
                (if binding (cdr binding) x))]

          [,x (guard (not (pair? x))) x]
          
          [(alloc ,size)
           `(alloc ,(substitute size env))]
          [(mref ,base ,offset)
           `(mref ,(substitute base env) ,(substitute offset env))]
          [(mset! ,base ,offset ,val)
           `(mset! ,(substitute base env) ,(substitute offset env) ,(substitute val env))]

          [(return-point ,label ,tail)
          `(return-point ,label ,(substitute tail env))]
          [(set! ,var ,val)
          (let ([new-var (substitute var env)]
                [new-val (substitute val env)])
            (if (equal? new-var new-val)
                '(nop)
                `(set! ,new-var ,new-val)))]

          
          [(,car-expr . ,cdr-expr)
           (cons (substitute car-expr env)
                 (substitute cdr-expr env))])))

    (define process-body
      (lambda (body)
        (match body
          ;; 匹配 (locate bindings tail)
          [(locate ,bindings ,tail)
            (let ([env (map (lambda (binding)
                        (cons (car binding) (cadr binding)))
                      bindings)])
            (substitute tail env))]
          
          [,else else])))

    (match program
      [(letrec ,lambda-bindings ,main-body)
       (let ([new-main-body (process-body main-body)]
             [new-lambda-bindings
              (map
               (lambda (binding)
                 (match binding
                   ;; 对 letrec 中的每一个 lambda 绑定进行处理
                   [(,label (lambda () ,body))
                    `(,label (lambda () ,(process-body body)))]))
               lambda-bindings)])
         `(letrec ,new-lambda-bindings ,new-main-body))]

      [,else
       (error 'finalize-locations "Invalid program structure" else)])))