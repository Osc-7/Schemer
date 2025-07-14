(define (uncover-free p)

  ;; 辅助函数：在一个表达式中收集所有自由变量。
  ;; - expr: 要分析的表达式
  ;; - bound: 在当前作用域中已绑定的变量列表
  (define (collect-free expr bound)
    (match expr
      ;; 原子情况: 如果一个 uvar 不在 bound 集合里, 它就是自由的
      [,x (guard (uvar? x))
       (if (memq x bound) '() (list x))]
      [,a (guard (not (list? a))) '()]
      [(quote ,_) '()]

      ;; 复合表达式
      [(if ,c ,t ,f)
       (union (collect-free c bound) (collect-free t bound) (collect-free f bound))]

      [(begin ,e1 ,e* ...)
       (apply union (map (lambda (x) (collect-free x bound)) (cons e1 e*)))]
      
      ;; 绑定表达式: 将新绑定的变量加入 bound 集合, 然后递归
      [(let ([,vars ,vals] ...) ,body)
       (let ([fv-in-vals (apply union (map (lambda (v) (collect-free v bound)) vals))]
             [fv-in-body (collect-free body (union vars bound))])
         (union fv-in-vals fv-in-body))]

      [(letrec ,bindings ,body)
       (let* ([func-names (map car bindings)]
              ;; 把 letrec 定义的函数名加入 bound 集合
              [new-bound (union func-names bound)])
         (let ([fv-in-lambdas
                (apply union
                       (map (lambda (b)
                              (match b
                                ;; 对每个 lambda, 把它的参数也加入 bound 集合
                                [(,_ (lambda ,formals ,lambda-body))
                                 (collect-free lambda-body (union formals new-bound))]))
                            bindings))]
               [fv-in-body (collect-free body new-bound)])
           (union fv-in-lambdas fv-in-body)))]

      [(lambda ,formals ,body)
       ;; lambda 的参数是绑定的
       (collect-free body (union formals bound))]

      ;; 函数调用
      [(,rator ,rands ...)
       (apply union (map (lambda (x) (collect-free x bound)) (cons rator rands)))]
      
      [,else '()]))

  ;; 主转换函数：递归遍历 AST，并在 lambda 处插入 (free ...)
  (define (transform expr bound-vars)
    (match expr
      ;; 原子表达式直接返回
      [,a (guard (not (list? a))) a]
      [(quote ,d) expr]

      ;; 复合表达式
      [(if ,c ,t ,f)
       `(if ,(transform c bound-vars) ,(transform t bound-vars) ,(transform f bound-vars))]

      [(begin ,e1 ,e* ...)
       `(begin ,@(map (lambda (e) (transform e bound-vars)) (cons e1 e*)))]

      [(let ([,vars ,vals] ...) ,body)
       (let ([new-vals (map (lambda (v) (transform v bound-vars)) vals)])
         `(let ,(map list vars new-vals)
            ,(transform body (union vars bound-vars))))]
      
      ;; letrec: 核心逻辑, 为每个 lambda 计算自由变量并插入 free
      [(letrec ,bindings ,body)
       (let* ([func-names (map car bindings)]
              [new-bound-for-body (union func-names bound-vars)])
         (let ([new-bindings
                (map
                  (lambda (b)
                    (match b
                      [(,f (lambda ,formals ,lambda-body))
                       (let* ([;; 对 collect-free, 绑定的变量只包含 lambda 参数和同级函数
                               lambda-local-bound formals]
                              [free-vars (remove-duplicates (collect-free lambda-body lambda-local-bound))])
                         ;; 对 transform, 需要传递所有已绑定的变量
                         `(,f (lambda ,formals
                                (free ,free-vars
                                      ,(transform lambda-body (union lambda-local-bound bound-vars))))))]
                      [,else (error 'uncover-free "unexpected binding form" b)]))
                  bindings)])
           `(letrec ,new-bindings ,(transform body new-bound-for-body))))]

      [(lambda ,formals ,body)
       (let* ([bound-for-body (union formals bound-vars)]
              [free-vars (remove-duplicates (collect-free body bound-for-body))])
         `(lambda ,formals
            (free ,free-vars
                  ,(transform body bound-for-body))))]
      
      [(,rator ,rands ...)
       `(,(transform rator bound-vars)
         ,@(map (lambda (r) (transform r bound-vars)) rands))]

      [,else (error 'uncover-free "unsupported expression" expr)]))

  ;; 初始调用
  (transform p '()))