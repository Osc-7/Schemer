(define finalize-locations
  (lambda (program)
    (define substitute
      (lambda (expr env)
        (match expr
          ;; 基本情况 1: 如果是符号, 检查是否需要替换
          [,x (guard (symbol? x))
              (let ([binding (assq x env)])
                (if binding (cdr binding) x))]

          [,x (guard (not (pair? x))) x]
          
          [(,car-expr . ,cdr-expr)
           (cons (substitute car-expr env)
                 (substitute cdr-expr env))])))

    (define process-body
      (lambda (body)
        (match body
          ;; 匹配 (locate bindings tail)
          [(locate ,bindings ,tail)
           ;; 使用从 bindings 创建的环境来替换 tail 中的变量
           (substitute tail bindings)]
          
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