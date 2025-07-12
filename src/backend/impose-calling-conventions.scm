(define triv?
      (lambda (triv)
        (cond
          [(int64? triv) #t]
          [(label? triv) #t]
          [(uvar? triv) #t]
          [(register? triv) #t]
          [(frame-var? triv) #t]
          [else #f])))

(define impose-calling-conventions
  (lambda (program)
    (define fp frame-pointer-register)
    (define ra return-address-register)
    (define rv return-value-register)
    (define ap allocation-pointer-register)

    ; 这个辅助函数负责将参数列表的前N个分配给寄存器。
    (define (assign_regs_to_params params)
      (let loop ([params params] [regs parameter-registers])
        (match params
          [() (values '() '() '())]
          [(,var . ,rest)
           (if (null? regs)
               (values params '() '())
               (let-values ([(rest_params used_regs set_to_regs) (loop rest (cdr regs))])
                 (values
                  rest_params
                  (cons (car regs) used_regs)
                  (cons `(set! ,(car regs) ,var) set_to_regs))))])))

    (define (Body_with_wrapper lam-exp)
      (define (Body uvars body)
        (define new_frame_lists '()) ; 使用局部可变状态来收集 new-frames

        ;; 形参处理：函数入口处，将传入的值赋给形参变量
    (define (handle_formal_params params rp)
        (make-begin 
            (cons `(set! ,rp ,ra)
                (let loop 
                    ([params params] 
                    [regs parameter-registers] 
                    [pos 0])
                    (match params
                        [() '()]
                        [(,uvar . ,rest)
                            (if (null? regs)
                                (cons `(set! ,uvar ,(index->frame-var pos))
                                    (loop rest regs (add1 pos)))
                                (cons `(set! ,uvar ,(car regs))
                                    (loop rest (cdr regs) pos)))])))))

        ;; 尾调用实参处理：将实参赋值给约定的寄存器和栈帧
    (define (handle_actual_params rp proc params)
        (let-values
            ([(rest_params used_regs set_of_regs) (assign_regs_to_params params)])
            (make-begin 
                (let loop
                    ([params rest_params]
                    [pos 0]
                    [assigned_fvs '()])
                    (match params
                        [() (append set_of_regs `((set! ,ra ,rp) (,proc ,ap ,fp ,ra ,@used_regs ,@assigned_fvs)))]
                        [(,var . ,rest)
                            (let* 
                                ([fv (index->frame-var pos)]
                                [statements (loop rest (add1 pos) (cons fv assigned_fvs))])
                                (cons `(set! ,fv ,var) statements))])))))

        ;; 非尾调用处理
        (define (nontail_call proc params)
          (let ([rp-label (unique-label 'rp-label)])
            `(return-point ,rp-label
               ,(make-begin
                 (let-values ([(rest_params used_regs set_of_regs) (assign_regs_to_params params)])
                   (let loop ([params rest_params] [assigned_nfvs '()])
                     (match params
                       [()
                        (set! new_frame_lists (cons (reverse assigned_nfvs) new_frame_lists))
                        `(,@set_of_regs
                          (set! ,ra ,rp-label)
                          (,proc ,ap ,fp ,ra ,@used_regs ,@(reverse assigned_nfvs)))]
                       [(,var . ,rest)
                        (let ([nfv (unique-name 'nfv)])
                          `((set! ,nfv ,var) ,@(loop rest (cons nfv assigned_nfvs))))])))))))

        ;; 递归处理 Tail 表达式
              (define (Tail rp tail)
                (match tail
                    [(begin ,[Stat -> effect*] ... ,sub_tail) 
                        (make-begin (append effect* (list (Tail rp sub_tail))))]
                    [(if ,[Stat -> pred] ,tail1 ,tail2)
                        `(if ,pred ,(Tail rp tail1) ,(Tail rp tail2))]
                    [(,triv ,triv* ...) (guard (triv? triv))
                        (handle_actual_params rp triv triv*)]
                    [,expr 
                        `(begin 
                            (set! ,rv ,expr) 
                            (,rp ,ap ,fp ,rv))]))

        ;; 递归处理 Pred, Effect, 和 Value 表达式
            (define (Stat statement)
                (match statement
                    [(begin ,[Stat -> statement*] ...) `(begin ,statement* ...)]
                    [(if ,[Stat -> statement*] ...) `(if ,statement* ...)]
                    [(,triv ,triv* ...) (guard (triv? triv))
                        (nontail_call triv triv*)]
                    [(set! ,uvar (,triv ,triv* ...)) (guard (triv? triv))
                        (make-begin 
                            `(,(Stat `(,triv ,triv* ...))
                                (set! ,uvar ,rv)))]
                    [,x x]))

        ;; Body 的主逻辑
            (match body
                [(locals ,body_uvar* ,tail)
                    (let ([rp (unique-name 'rp)])
                        `(locals (,@body_uvar* ,@(cons rp uvars) ,@(apply union new_frame_lists))
                            (new-frames ,new_frame_lists
                                ,(make-begin 
                                    `(,(handle_formal_params uvars rp)
                                        ,(Tail rp tail))))))]))

      (match lam-exp
        [(lambda ,uvars ,body) (Body uvars body)]))

    ;; `impose-calling-conventions` 的入口
    (match program
      [(letrec ,bindings ,main-body)
       (let ([new-bindings
              (map (lambda (b)

                     `(,(car b) (lambda () ,(Body_with_wrapper (cadr b)))))
                   bindings)]
             [new-main-body (Body_with_wrapper `(lambda () ,main-body))])
         `(letrec ,new-bindings ,new-main-body))]
      [,else (error 'impose-calling-conventions "program must be a letrec" else)])))
