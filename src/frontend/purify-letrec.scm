(define (purify-letrec program)
  ;; 辅助函数：将 letrec 的绑定划分为 simple, lambda, complex 三类
  (define (partition-bindings letrec-vars bindings assigned-vars)
    (letrec
        ([
          is-lambda?
          (lambda (var bind-expr)
            (match bind-expr
              [(lambda ,formals ,body) (not (memq var assigned-vars))]
              [,_ #f]))]
         [
          is-simple?
          (lambda (var bind-expr)
            (if (memq var assigned-vars)
                #f
                (letrec ([check (lambda (e)
                                  (match e
                                    [`(,prim ,operands ...) (guard (prim? prim))
                                     (andmap check operands)]
                                    [`(quote ,c) (guard (immediate? c)) #t]
                                    [,v (guard (symbol? v)) (not (memq v letrec-vars))]
                                    [,atom #t]))])
                  (check bind-expr))))])

      (let loop ([bds bindings] [simple '()] [lambda '()] [complex '()])
        (if (null? bds)
            (values (reverse simple) (reverse lambda) (reverse complex))
            (let ([current-binding (car bds)])
              (cond
                [(is-lambda? (car current-binding) (cadr current-binding))
                 (loop (cdr bds) simple (cons current-binding lambda) complex)]
                [(is-simple? (car current-binding) (cadr current-binding))
                 (loop (cdr bds) (cons current-binding simple) lambda complex)]
                [else
                 (loop (cdr bds) simple lambda (cons current-binding complex))]))))))

  ;; 主转换函数
  (define (transform e)
    (match e
      ;; 关键情况：处理 letrec
      [(letrec ,bindings (assigned ,assigned-vars ,body))
       (let-values ([(simple-binds lambda-binds complex-binds)
                     (partition-bindings (map car bindings) 
                                           (map (lambda (b) `[,(car b) ,(transform (cadr b))]) bindings)
                                           assigned-vars)])
         (let* ([new-body (transform body)]
                [simple-vars (map car simple-binds)]
                [simple-inits (map cadr simple-binds)]
                [lambda-vars (map car lambda-binds)]
                [lambda-inits (map cadr lambda-binds)]
                [complex-vars (map car complex-binds)]
                [complex-inits (map cadr complex-binds)]
                [temp-vars (map (lambda (_) (unique-name 'tmp)) complex-vars)])
           
           ;; 如果没有 simple 和 complex 绑定，说明是纯 letrec，直接返回
           (if (and (null? simple-binds) (null? complex-binds))
               `(letrec ,(map list lambda-vars lambda-inits) ,new-body)
               
               ;; 否则，构建复杂的嵌套 let/letrec 结构
               `(let ,(map list simple-vars simple-inits)
                  (assigned ()
                    (let ,(map (lambda (v) `[,v (void)]) complex-vars)
                      (assigned ,complex-vars
                        (letrec ,(map list lambda-vars lambda-inits)
                          (let ,(map list temp-vars complex-inits)
                            (assigned ()
                              ,(make-begin
                               (append (map (lambda (c-var t-var) `(set! ,c-var ,t-var))
                                            complex-vars temp-vars)
                                       (list new-body)))))))))))))]
      
      ;; 递归处理其他复合表达式
      [(if ,t ,c ,a) `(if ,(transform t) ,(transform c) ,(transform a))]
      [(begin ,exprs ... ,last) `(begin ,@(map transform exprs) ,(transform last))]
      [(set! ,v ,val) `(set! ,v ,(transform val))]
      [(lambda ,formals ,body) `(lambda ,formals ,(transform body))]
      [(let ,bindings ,body)
       `(let ,(map (lambda (b) `[,(car b) ,(transform (cadr b))]) bindings)
          ,(transform body))]
      [(,rator ,rands ...)
       `(,(transform rator) ,@(map transform rands))]
      
      ;; 基本情况：原子表达式保持不变
      [,atom atom]))

  (transform program))