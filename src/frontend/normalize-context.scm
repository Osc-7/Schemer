(define (normalize-context p)
  (normalize-value p))

(define (normalize-value e)
  (match e
    ;; 原子表达式和 quote 在 Value 上下文中是合法的
    [,x (guard (not (pair? x))) x]
    [(quote ,_) e]

    ;; if 的 test 部分进入 pred 上下文，then/else 部分保持在 value 上下文
    [(if ,test ,then ,else)
     `(if ,(normalize-pred test)
          ,(normalize-value then)
          ,(normalize-value else))]
    
    ;; let 的 rhs 在 value 上下文中，body 也在 value 上下文中
    [(let ([,vars ,rhss] ...) ,body)
     `(let ,(map (lambda (v r) `[,v ,(normalize-value r)]) vars rhss)
        ,(normalize-value body))]

    ;; begin 的效果部分进入 effect 上下文，最后一个表达式在 value 上下文
    [(begin ,effects ... ,final)
     (make-begin
       (append (map normalize-effect effects)
               (list (normalize-value final))))]

    ;; 函数调用
    [(,rator ,rands ...)
     (cond
       [(pred-prim? rator)
        ;; pred-prim 在 value 上下文中，需要转换为 if
        `(if ,(normalize-pred e) '#t '#f)]

       [(effect-prim? rator)
        ;; effect-prim 在 value 上下文中，需要返回一个 void 值
        `(begin ,(normalize-effect e) (void))]

       [else ; 默认是 value-prim 或普通函数调用
        `(,(normalize-value rator)
          ,@(map normalize-value rands))])]
        
    [,else (error 'normalize-value "Unknown expression form --" e)]))

(define (normalize-pred e)
  (match e
    ;; if 的所有部分都在 pred 上下文中
    [(if ,test ,then ,else)
     `(if ,(normalize-pred test)
          ,(normalize-pred then)
          ,(normalize-pred else))]

    [(quote #f) '(false)]
    [(quote ,_) '(true)]

    [(let ([,vars ,rhss] ...) ,body)
     `(let ,(map (lambda (v r) `[,v ,(normalize-value r)]) vars rhss)
        ,(normalize-pred body))]

    ;; 其他所有表达式 (变量, value-prim, 函数调用等) 都需要转换为对 #f 的显式比较
    [,else
     (cond
       [(pred-prim? (if (pair? e) (car e) #f))
        ;; 如果已经是 pred-prim，只需 normalize 其参数
        `(,(car e) ,@(map normalize-value (cdr e)))]
       [else
        ;; 否则进行转换
        `(if (eq? ,(normalize-value e) '#f)
             (false)
             (true))])]))


(define (normalize-effect e)
  (match e
    ;; 原子表达式和 quote 在 effect 上下文中没有副作用，变为 nop
    [,x (guard (not (pair? x))) '(nop)]
    [(quote ,_) '(nop)]
    
    ;; if 的 test 是 pred, then/else 是 effect
    [(if ,test ,then ,else)
     `(if ,(normalize-pred test)
          ,(normalize-effect then)
          ,(normalize-effect else))]
    
    ;; begin 的所有部分都在 effect 上下文中
    [(begin ,exprs ...)
     (make-begin (map normalize-effect exprs))]
     
    [(let ([,vars ,rhss] ...) ,body)
     `(let ,(map (lambda (v r) `[,v ,(normalize-value r)]) vars rhss)
        ,(normalize-effect body))]

    [(,rator ,rands ...)
     (cond
       [(or (pred-prim? rator) (value-prim? rator))
        ;; 对于 value/pred prim, 只保留其参数的副作用
        (make-nopless-begin (map normalize-effect rands))]
       [else ; effect-prim 或普通函数调用，是合法的
        `(,(normalize-value rator)
          ,@(map normalize-value rands))])]
          
    [,else (error 'normalize-effect "Unknown expression form --" e)]))