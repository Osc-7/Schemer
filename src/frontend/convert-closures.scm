(define (convert-closures p)

  ;; 辅助函数，用于判断一个符号是否为原始操作符
  (define (primitive? s)
    (and (symbol? s)
         (or (memq s value-primitives)
             (memq s predicate-primitives)
             (memq s effect-primitives))))

  (define (walk expr cp-in-scope)
    (match expr
      ;; 原子表达式保持不变
      [,x (guard (or (uvar? x) (number? x) (boolean? x))) x]
      [,s (guard (symbol? s)) s]
      [(quote ,d) expr]

      ;; letrec: 这是最核心的转换部分
      [(letrec ,bindings ,main-body)
       (let* (
              [closure-infos
               (map (lambda (b)
                      (match b
                        [(,func-name (lambda ,params (free ,free-vars ,body)))
                         (let ([label (unique-label func-name)]
                               [cp (unique-name 'cp)])
                           (list func-name label cp params free-vars body))]))
                    bindings)]
              [new-bindings
               (map (lambda (info)
                      (let ([label      (list-ref info 1)]
                            [cp         (list-ref info 2)]
                            [params     (list-ref info 3)]
                            [free-vars  (list-ref info 4)]
                            [lambda-body (list-ref info 5)])
                        `(,label (lambda (,cp ,@params)
                                   ,(walk `(free ,free-vars ,lambda-body) cp)))))
                    closure-infos)]
              [closures-bindings
               (map (lambda (info)
                      (let ([func-name  (list-ref info 0)]
                            [label      (list-ref info 1)]
                            [free-vars  (list-ref info 4)])
                        `(,func-name ,label ,@free-vars)))
                    closure-infos)])
         `(letrec ,new-bindings
            (closures ,closures-bindings
                      ,(walk main-body #f))))]

      ;; free 表达式的转换
      [(free (,fvs ...) ,body)
       (if cp-in-scope
           `(bind-free (,cp-in-scope ,@fvs) ,(walk body cp-in-scope))
           (error 'convert-closures "free form found outside a lambda" expr))]

      [(if ,c ,t ,f)
       `(if ,(walk c cp-in-scope) ,(walk t cp-in-scope) ,(walk f cp-in-scope))]
      
      [(begin ,e1 ,e* ...)
       `(begin ,@(map (lambda (e) (walk e cp-in-scope)) (cons e1 e*)))]
      
      [(let ([,vars ,vals] ...) ,body)
       (let ([new-vals (map (lambda (v) (walk v cp-in-scope)) vals)])
         `(let ,(map list vars new-vals)
            ,(walk body cp-in-scope)))]
      
      ;; 函数调用转换
      [(,rator ,rands ...)
       (let ([new-rator (walk rator cp-in-scope)]
             [new-rands (map (lambda (r) (walk r cp-in-scope)) rands)])
         (if (primitive? rator)
             ;; 如果是原始操作符，则保持不变
             `(,new-rator ,@new-rands)
             ;; 否则，认为是函数调用，需要处理闭包
             (let ((rator-var (unique-name 'rator)))
               `(let ([,rator-var ,new-rator])
                  ;; 调用时，将自身作为第一个参数传递
                  (,rator-var ,rator-var ,@new-rands)))))]

      [,else (error 'convert-closures "unsupported expression" expr)]))

  ;; 初始调用
  (walk p #f))