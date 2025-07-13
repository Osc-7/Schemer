(define (uncover-locals p)
      ;;; 处理 <Tail> 类型的表达式
      (define (Tail t)
        (match t
          [(let ([,uvars ,values] ...) ,body)
          (append uvars
                  (apply append (map Value values))
                  (Tail body))]
          [(if ,pred ,t1 ,t2) (append (Pred pred) (Tail t1) (Tail t2))]
          [(begin ,effects ... ,tail) (append (apply append (map Effect effects)) (Tail tail))]
          [(,binop ,v1 ,v2) (append (Value v1) (Value v2))]
          [(alloc ,v) (Value v)]
          [(,_ ,values ...) (apply append (map Value values))]
          [,else '()] ; 基本情况：不包含 let，返回空列表
          ))

      ;;; 处理 <Pred> 类型的表达式
      (define (Pred p)
        (match p
          [(let ([,uvars ,values] ...) ,body)
          (append uvars (apply append (map Value values)) (Pred body))]
          [(if ,pred ,p1 ,p2) (append (Pred pred) (Pred p1) (Pred p2))]
          [(begin ,effects ... ,pred) (append (apply append (map Effect effects)) (Pred pred))]
          [(,relop ,v1 ,v2) (append (Value v1) (Value v2))]
          [,else '()]
          ))

      ;;; 处理 <Effect> 类型的表达式
      (define (Effect e)
        (match e
          [(let ([,uvars ,values] ...) ,body)
          (append uvars (apply append (map Value values)) (Effect body))]
          [(if ,pred ,e1 ,e2) (append (Pred pred) (Effect e1) (Effect e2))]
          [(begin ,effects ... ,effect) (append (apply append (map Effect effects)) (Effect effect))]
          [(mset! ,v1 ,v2 ,v3) (append (Value v1) (Value v2) (Value v3))]
          [(,_ ,values ...) (apply append (map Value values))]
          [,else '()]
          ))

      ;;; 处理 <Value> 类型的表达式
      (define (Value v)
        (match v
          [(let ([,uvars ,values] ...) ,body)
          (append uvars (apply append (map Value values)) (Value body))]
          [(if ,pred ,v1 ,v2) (append (Pred pred) (Value v1) (Value v2))]
          [(begin ,effects ... ,value) (append (apply append (map Effect effects)) (Value value))]
          [(,binop ,v1 ,v2) (append (Value v1) (Value v2))]
          [(alloc ,v) (Value v)]
          [(,_ ,values ...) (apply append (map Value values))]
          [,else '()]
          ))
      (match p
    [(letrec ,bindings ,main-body)
     (let* ([main-body-vars (remove-duplicates (Tail main-body))]
            [new-main-body `(locals ,main-body-vars ,main-body)])
       
       (let ([new-bindings
              (map (lambda (b)
                     (let* ([label (car b)]
                            [lam (cadr b)]
                            [formals (cadr lam)]
                            [body (caddr lam)])
                       ;; 对每个 lambda 的 body 执行同样的操作
                       (let* ([body-vars (remove-duplicates (Tail body))]
                              [new-lambda-body `(locals ,body-vars ,body)])
                         ;; 重建绑定
                         `(,label (lambda ,formals ,new-lambda-body)))))
                   bindings)])
         
         `(letrec ,new-bindings ,new-main-body)))]
    
    [,else (error 'uncover-locals "must be a letrec" p)]))