(define (remove-anonymous-lambda p)
  (define (walk expr)
    (match expr
      [,x (guard (or (symbol? x) (immediate? x)(uvar? x) (boolean? x))) x]
      [(quote ,d) expr]

     ;; 核心转换：处理匿名 lambda (当它出现在非绑定值位置时)
      [(lambda ,formals ,body)
       (let ([anon-name (unique-name 'anon)])
         `(letrec ([,anon-name (lambda ,formals ,(walk body))])
            ,anon-name))]

      ;; If 表达式：递归处理
      [(if ,c ,t ,f) `(if ,(walk c) ,(walk t) ,(walk f))]

      ;; Begin 表达式：递归处理
      [(begin ,e1 ,e* ...) `(begin ,@(map walk (cons e1 e*)))]

      [(let ([,vars ,vals] ...) ,body)
       (let ([new-vals (map (lambda (val)
                              (match val
                                ;; 如果绑定值是 lambda，只 walk 其 body
                                [(lambda ,formals ,lam-body)
                                 `(lambda ,formals ,(walk lam-body))]
                                ;; 如果是其他值，正常 walk
                                [,else (walk val)]))
                            vals)])
         `(let ,(map list vars new-vals) ,(walk body)))]

      [(letrec ([,vars ,vals] ...) ,body)
       ;; letrec 的绑定值按语法必须是 lambda，所以我们只 walk 它们的 body
       (let ([new-vals (map (lambda (lam)
                              (match lam
                                [(lambda ,formals ,lam-body)
                                 `(lambda ,formals ,(walk lam-body))]
                                ;; 按语法，这里不应有其他情况，但为健壮性加上
                                [,else (error 'remove-anonymous-lambda
                                             "letrec 中发现非 lambda 绑定" lam)]))
                            vals)])
         `(letrec ,(map list vars new-vals) ,(walk body)))]
      
      ;; 通用函数调用
      [(,rator ,rands ...) `(,(walk rator) ,@(map walk rands))]

      ;; 错误处理
      [,else (error 'remove-anonymous-lambda "不支持的表达式形式" expr)]))

  (walk p))