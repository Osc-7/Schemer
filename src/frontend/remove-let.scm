(define (remove-let p)
        ;;; 处理 lambda 表达式
        (define (Lambda lam)
          (match lam
            [(lambda (,formals ...) ,body)
            `(lambda ,formals ,(Tail body))]))

        ;;; 处理 Tail 类型的表达式
        (define (Tail t)
          (match t
            [(locals (,vars ...) ,body)
            `(locals ,vars ,(Tail body))]
            [(let ([,uvars ,values] ...) ,body)
            (let* ((set-exprs (map (lambda (u v) `(set! ,u ,(Value v))) uvars values))
                    (new-body (Tail body)))
              (make-begin (append set-exprs (list new-body))))]
            [(if ,pred ,t1 ,t2)
            `(if ,(Pred pred) ,(Tail t1) ,(Tail t2))]
            [(begin ,effects ... ,tail)
            (make-begin (append (map Effect effects) (list (Tail tail))))]
            [(,op ,v1 ,v2) ; binop
            `(,op ,(Value v1) ,(Value v2))]
            [(alloc ,v)
            `(alloc ,(Value v))]
            [(,_ ,values ...) ; 函数调用
            (cons (car t) (map Value (cdr t)))]
            [,else t])) ; Trivials

        ;;; 处理 Pred 类型的表达式
        (define (Pred p)
          (match p
            [(let ([,uvars ,values] ...) ,body)
            (let* ((set-exprs (map (lambda (u v) `(set! ,u ,(Value v))) uvars values))
                    (new-body (Pred body)))
              (make-begin (append set-exprs (list new-body))))]
            [(if ,pred ,p1 ,p2)
            `(if ,(Pred pred) ,(Pred p1) ,(Pred p2))]
            [(begin ,effects ... ,pred)
            (make-begin (append (map Effect effects) (list (Pred pred))))]
            [(,op ,v1 ,v2) ; relop
            `(,op ,(Value v1) ,(Value v2))]
            [,else p])) ; true, false, Trivials

        ;;; 处理 Effect 类型的表达式
        (define (Effect e)
          (match e
            [(let ([,uvars ,values] ...) ,body)
            (let* ((set-exprs (map (lambda (u v) `(set! ,u ,(Value v))) uvars values))
                    (new-body (Effect body)))
              (make-begin (append set-exprs (list new-body))))]
            [(if ,pred ,e1 ,e2)
            `(if ,(Pred pred) ,(Effect e1) ,(Effect e2))]
            [(begin ,effects ... ,effect)
            (make-begin (append (map Effect effects) (list (Effect effect))))]
            [(mset! ,v1 ,v2 ,v3)
            `(mset! ,(Value v1) ,(Value v2) ,(Value v3))]
            [(,_ ,values ...) ; 函数调用
            (cons (car e) (map Value (cdr e)))]
            [,else e])) ; nop, Trivials

        ;;; 处理 Value 类型的表达式
        (define (Value v)
          (match v
            [(let ([,uvars ,values] ...) ,body)
            (let* ((set-exprs (map (lambda (u v) `(set! ,u ,(Value v))) uvars values))
                    (new-body (Value body)))
              (make-begin (append set-exprs (list new-body))))]
            [(if ,pred ,v1 ,v2)
            `(if ,(Pred pred) ,(Value v1) ,(Value v2))]
            [(begin ,effects ... ,value)
            (make-begin (append (map Effect effects) (list (Value value))))]
            [(,op ,v1 ,v2) ; binop
            `(,op ,(Value v1) ,(Value v2))]
            [(alloc ,v)
            `(alloc ,(Value v))]
            [(,_ ,values ...) ; 函数调用
            (cons (car v) (map Value (cdr v)))]
            [,else v])) ; Trivials

      (match p
    [(letrec ([,labels ,lambdas] ...) ,body)
     ;; 对 letrec 中的每个 lambda 应用转换
     `(letrec ,(map (lambda (l lam) `(,l ,(Lambda lam))) labels lambdas)
        ,(Tail body))]))