(define (sanitize-binding-forms p)

  ;; 辅助函数，判断一个表达式是否为 lambda
  (define (lambda? exp)
    (and (list? exp) (eq? (car exp) 'lambda)))

  ;; 内部递归函数，用于遍历表达式树
  (define (walk expr)
    (match expr
      ;; 原子表达式和 quote 保持不变
      [,x (guard (or (symbol? x) (immediate? x)(uvar? x))) x]
      [(quote ,d) expr]

      ;; If 表达式：递归处理所有子表达式
      [(if ,c ,t ,f)
       `(if ,(walk c) ,(walk t) ,(walk f))]

      ;; Letrec 表达式：递归处理绑定和主体，并检查是否为空
      [(letrec ,bindings ,body)
       (let ([new-bindings (map (lambda (b) `(,(car b) ,(walk (cadr b)))) bindings)]
             [new-body (walk body)])
         ;; 如果绑定为空，则移除 letrec
         (if (null? new-bindings)
             new-body
             `(letrec ,new-bindings ,new-body)))]

      ;; Begin 表达式：递归处理其所有子表达式
      [(begin ,e1 ,e* ...)
       `(begin ,@(map walk (cons e1 e*)))]

      ;处理 Let 表达式
      [(let ,bindings ,body)
       (let* (
              ;; 首先，对所有绑定值和主体进行递归处理
              [walked-bindings (map (lambda (b) `(,(car b) ,(walk (cadr b)))) bindings)]
              [new-body (walk body)]
              
              ;; 将绑定分为 lambda 绑定和其他绑定
              [lambda-bindings (filter (lambda (b) (lambda? (cadr b))) walked-bindings)]
              [other-bindings (filter (lambda (b) (not (lambda? (cadr b)))) walked-bindings)])
         
         (cond
           ;; 情况 1: 没有 lambda 绑定，则只保留 let
           [(null? lambda-bindings)
            (if (null? other-bindings)
                new-body ; 如果其他绑定也为空，则直接移除 let
                `(let ,other-bindings ,new-body))]

           ;; 情况 2: 只有 lambda 绑定，则整个表达式变为 letrec
           [(null? other-bindings)
            `(letrec ,lambda-bindings ,new-body)]
            
           ;; 情况 3: 两类绑定都存在，创建 letrec 包裹 let 的结构
           [else
            `(letrec ,lambda-bindings
               (let ,other-bindings
                 ,new-body))]))]
      ;; 函数调用：递归处理操作符和所有操作数
      [(,rator ,rands ...)
       `(,(walk rator) ,@(map walk rands))]

      ;; 错误处理
      [,else (error 'sanitize-binding-forms "不支持的表达式形式" expr)]))
      
  (walk p))