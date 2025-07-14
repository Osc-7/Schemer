(define (introduce-procedure-primitives p)

  (define (primitive? s)
    (and (symbol? s)
         (or (memq s value-primitives)
             (memq s predicate-primitives)
             (memq s effect-primitives))))

  ;; walk - 递归遍历和转换 AST
  ;; cp: 当前作用域的闭包指针变量 (uvar or #f)
  ;; fv-map: 一个关联列表 '((var . index) ...)，用于查找自由变量的索引
  (define (walk expr cp fv-map)
    (match expr
      ;; 任务3: 替换自由变量的访问
      [() '()]

      [,x (guard (uvar? x))
       (let ([found (assoc x fv-map)])
         (if found
             `(procedure-ref ,cp ,(cdr found))
             x))]

      ;; 原子表达式保持不变
      [,a (guard (not (list? a))) a]
      [(quote ,d) expr]

      ;; 任务2: 处理 bind-free, 丢弃其自身并建立新的 fv-map
      [(bind-free (,cp-var ,fvs ...) ,body)
       (let ([new-fv-map
              (let loop ([vars fvs] [i 0])
                (if (null? vars)
                    '()
                    (cons (cons (car vars) i)
                          (loop (cdr vars) (+ i 1)))))])
         ;; 递归调用 walk 时, 使用从 bind-free 中获取的新 cp 和 fv-map
         (walk body cp-var new-fv-map))]

      ;; 任务1: 处理 closures, 转换为 let, make-procedure 和 procedure-set!
      [(closures ,bindings ,body)
       (if (null? bindings)
           (walk body cp fv-map)
           (let* ([let-bindings
                   (map (lambda (b)
                          (let ([f (car b)]
                                [l (cadr b)]
                                [fvs (cddr b)])
                            `(,f (make-procedure ,l ,(length fvs)))))
                        bindings)]
                  [set-exprs
                   (apply append
                          (map (lambda (b)
                                 (let ([f (car b)]
                                       [fvs (cddr b)])
                                   (let loop ([vars fvs] [i 0])
                                     (if (null? vars)
                                         '()
                                         (cons `(procedure-set! ,f ,i ,(car vars))
                                               (loop (cdr vars) (+ i 1)))))))
                               bindings))]
                  [walked-body (walk body cp fv-map)])
             `(let ,let-bindings
                ,(make-begin (append set-exprs (list walked-body))))))]

      ;; 其他表达式的递归转换, 需要传递 cp 和 fv-map
      [(if ,c ,t ,f)
       `(if ,(walk c cp fv-map) ,(walk t cp fv-map) ,(walk f cp fv-map))]

      [(begin ,e1 ,e* ...)
       `(begin ,@(map (lambda (e) (walk e cp fv-map)) (cons e1 e*)))]
      
      [(let ([,vars ,vals] ...) ,body)
       ;; let 绑定的变量会遮蔽同名的自由变量
       (let ([new-fv-map (filter (lambda (pair) (not (memq (car pair) vars))) fv-map)])
         `(let ,(map (lambda (v val) `(,v ,(walk val cp fv-map))) vars vals)
            ,(walk body cp new-fv-map)))]

      [(letrec ([,bindings] ...) ,body)
       ;; letrec 绑定的函数名不是自由变量, 但其内部和主体可能使用外部的自由变量
       `(letrec ,(map (lambda (b)
                        `(,(car b) ,(walk (cadr b) cp fv-map)))
                     bindings)
          ,(walk body cp fv-map))]
      
      [(lambda ,params ,body)
       ;; lambda 参数会遮蔽同名的自由变量
       (let ([new-fv-map (filter (lambda (pair) (not (memq (car pair) params))) fv-map)])
         `(lambda ,params ,(walk body cp new-fv-map)))]
      ;; 任务4: 转换过程调用
      [(,rator ,rands ...)
       (let ([walked-rator (walk rator cp fv-map)])
         (if (and (uvar? rator) (not (primitive? rator)))
             `((procedure-code ,walked-rator)
               ,@(map (lambda (r) (walk r cp fv-map)) rands))
             `(,walked-rator
               ,@(map (lambda (r) (walk r cp fv-map)) rands))))]
      [,else (error 'introduce-procedure-primitives "unsupported expression" expr)]))

  ;; 初始调用, 没有 cp 和 fv-map
  (walk p #f '()))