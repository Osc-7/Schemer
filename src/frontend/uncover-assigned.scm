(define (uncover-assigned p)
  (define (set-union s1 s2)
    (remove-duplicates (append s1 s2)))

  (define (set-intersection s1 s2)
    (remove-duplicates (filter (lambda (x) (memq x s2)) s1)))

  (define (set-subtract s1 s2)
    (filter (lambda (x) (not (memq x s2))) s1))

  (define (walk e bound-vars)
    (match e
      ;; 基本情况: 变量和立即数本身不产生赋值
      [(quote ,_) (list e '())]
      [,x (guard (or (symbol? x) (immediate? x) (uvar? x) (boolean? x))) (list x '())]
      
      ;; set! 表达式
      [(set! ,var ,val-expr)
       (let* ([walked-val (walk val-expr bound-vars)]
              [new-val (car walked-val)]
              [assigned-in-val (cadr walked-val)])
         ;; 整个 set! 表达式的赋值集合 = {被赋值的变量 var} U {val-expr 内部的赋值集合}
         (list `(set! ,var ,new-val)
               (set-union (list var) assigned-in-val)))]

      ;; 复合表达式: if, begin, aplication
      [(if ,tst ,con ,alt)
       (let* ([walked-tst (walk tst bound-vars)] [new-tst (car walked-tst)] [assigned-tst (cadr walked-tst)]
              [walked-con (walk con bound-vars)] [new-con (car walked-con)] [assigned-con (cadr walked-con)]
              [walked-alt (walk alt bound-vars)] [new-alt (car walked-alt)] [assigned-alt (cadr walked-alt)])
         (list `(if ,new-tst ,new-con ,new-alt)
               (set-union assigned-tst (set-union assigned-con assigned-alt))))]
      
      [(begin ,exprs ... ,last-expr)
       (let* ([walked-all (map (lambda (sub-e) (walk sub-e bound-vars)) (append exprs (list last-expr)))]
              [new-exprs (map car walked-all)])
         (list (make-begin new-exprs)
               (fold-left set-union '() (map cadr walked-all))))]
      
      ;; 绑定表达式: lambda, let, letrec
      [(lambda (,formals ...) ,body)
       (let* ([new-bound-vars (append formals bound-vars)] ; 扩展环境
              [walked-body (walk body new-bound-vars)]
              [new-body (car walked-body)]
              [assigned-in-body (cadr walked-body)])
         ;; 将 body 内的赋值变量分为两部分：
         (let ([locally-assigned (set-intersection assigned-in-body formals)] ; 1. 当前 lambda 绑定的
               [free-assigned (set-subtract assigned-in-body formals)])       ; 2. 自由的（来自外部作用域）
           ;; 构造新 lambda，并只向上返回自由赋值变量集合
           (list `(lambda (,@formals) (assigned ,locally-assigned ,new-body))
                 free-assigned)))]

      [(let ,bindings ,body)
       (let* ([vars (map car bindings)]
              [inits (map cadr bindings)]
              [walked-inits (map (lambda (i) (walk i bound-vars)) inits)]
              [new-inits (map car walked-inits)]
              [assigned-in-inits (fold-left set-union '() (map cadr walked-inits))]
              [new-bound-vars (append vars bound-vars)]
              [walked-body (walk body new-bound-vars)]
              [new-body (car walked-body)]
              [assigned-in-body (cadr walked-body)])
         (let ([locally-assigned (set-intersection assigned-in-body vars)]
               [free-assigned-from-body (set-subtract assigned-in-body vars)])
           (list `(let ,(map list vars new-inits) (assigned ,locally-assigned ,new-body))
                 (set-union assigned-in-inits free-assigned-from-body))))]
      
      [(letrec ,bindings ,body)
       (let* ([vars (map car bindings)]
              [inits (map cadr bindings)]
              [new-bound-vars (append vars bound-vars)]
              [walked-inits (map (lambda (i) (walk i new-bound-vars)) inits)]
              [new-inits (map car walked-inits)]
              [assigned-in-inits (fold-left set-union '() (map cadr walked-inits))]
              [walked-body (walk body new-bound-vars)]
              [new-body (car walked-body)]
              [assigned-in-body (cadr walked-body)])
         (let* ([all-assigned (set-union assigned-in-inits assigned-in-body)]
                [locally-assigned (set-intersection all-assigned vars)]
                [free-assigned (set-subtract all-assigned vars)])
           (list `(letrec ,(map list vars new-inits) (assigned ,locally-assigned ,new-body))
                 free-assigned)))]

      ;; 函数调用
      [(,rator ,rands ...)
       (let* ([walked-all (map (lambda (sub-e) (walk sub-e bound-vars)) (cons rator rands))]
              [new-exprs (map car walked-all)])
         (list new-exprs
               (fold-left set-union '() (map cadr walked-all))))]
      
      [,_ (error 'uncover-assigned "unrecognized expression form" e)]
      ))

  (car (walk p '())))