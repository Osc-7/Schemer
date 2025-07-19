(define (optimize-direct-call exp)
  (cond
    ;; 原子表达式：变量和立即数保持不变
    [(or (symbol? exp) (immediate? exp))
     exp]

    ;; 引用表达式：保持不变
    [(eq? (car exp) 'quote)
     exp]

    ;; Lambda 表达式：递归处理其 body
    [(eq? (car exp) 'lambda)
     (let ([formals (cadr exp)]
           [body (caddr exp)])
       `(lambda ,formals ,(optimize-direct-call body)))]

    ;; If 表达式：递归处理其所有子表达式
    [(eq? (car exp) 'if)
     (let ([test (cadr exp)]
           [conseq (caddr exp)]
           [alt (cadddr exp)])
       `(if ,(optimize-direct-call test)
            ,(optimize-direct-call conseq)
            ,(optimize-direct-call alt)))]
            
    ;; Let 表达式：递归处理其绑定和 body
    [(eq? (car exp) 'let)
     (let ([bindings (cadr exp)]
           [body (caddr exp)])
       `(let ,(map (lambda (b)
                     `(,(car b) ,(optimize-direct-call (cadr b))))
                   bindings)
          ,(optimize-direct-call body)))]

    ;; Letrec 表达式：递归处理其绑定和 body
    [(eq? (car exp) 'letrec)
     (let ([bindings (cadr exp)]
           [body (caddr exp)])
       `(letrec ,(map (lambda (b)
                        `(,(car b) ,(optimize-direct-call (cadr b))))
                      bindings)
           ,(optimize-direct-call body)))]

    ;; Begin 和 Prim 表达式：递归处理其所有子表达式
    [(or (eq? (car exp) 'begin) (eq? (car exp) 'prim))
     (cons (car exp) (map optimize-direct-call (cdr exp)))]

    ;; 核心逻辑：处理过程调用 (Application)
    [(list? exp)
     (let ([rator (car exp)]  ; 操作符 (operator)
           [rands (cdr exp)]) ; 操作数 (operands)
       
       ;; 检查操作符是否是一个 lambda 表达式
       (if (and (list? rator) (eq? (car rator) 'lambda))
           (let ([formals (cadr rator)] ; lambda 的形参
                 [body (caddr rator)])   ; lambda 的函数体
             
             (if (= (length formals) (length rands))
                 `(let ,(map (lambda (f a) `(,f ,(optimize-direct-call a)))
                              formals
                              rands)
                    ,(optimize-direct-call body))
                 
                 (cons (optimize-direct-call rator)
                       (map optimize-direct-call rands))))
           
           ;; 操作符不是 lambda，这是一个普通调用
           ;; 仅递归处理操作符和操作数
           (cons (optimize-direct-call rator)
                 (map optimize-direct-call rands))))]

    ;; 其他情况，原样返回
    [else exp]))

;; 辅助函数，用于判断是否为立即数
(define (immediate? x)
  (or (number? x) (boolean? x) (null? x)))