(define impose-calling-conventions
  (lambda (program)
    (define (is-a-call? exp)
      (and (pair? exp)
          (symbol? (car exp))
          (not (memq (car exp) '(+ - * sra logand logor < <= = >= >)))))
    (define (transform-nontail-call rator rands rp)
      (let* ([rp-label (unique-label 'rp-label)]
            [param-regs parameter-registers]
            [num-reg-params (length param-regs)]
            [reg-args (take rands num-reg-params)]
            [stack-args (drop rands num-reg-params)]
            ;; 1. 为栈参数创建 nfv
            [nfvs (map (lambda (_) (unique-name 'nfv)) stack-args)]
            ;; 2. 创建赋值语句 (nfv 在前)
            [stack-assigns (map (lambda (nfv arg) `(set! ,nfv ,arg)) nfvs stack-args)]
            [reg-assigns (map (lambda (reg arg) `(set! ,reg ,arg)) (take param-regs (length reg-args)) reg-args)]
            [all-assigns (append stack-assigns reg-assigns)])
        ;; 3. 构建 return-point 表达式
        (values `(return-point ,rp-label
                  (begin
                    ,@all-assigns
                    (set! ,return-address-register ,rp-label)
                    ;; 注意：此处最后一个参数是 nfv 列表
                    (,rator ,frame-pointer-register ,return-address-register ,@(take param-regs (length reg-args)) ,@nfvs)))
                nfvs)))
    ;; 新增辅助函数
    (define (map-values-and-append f ls)
      (if (null? ls)
          (values '() '())
          (let-values ([(h-item h-frames) (f (car ls))])
            (let-values ([(t-items t-frames) (map-values-and-append f (cdr ls))])
              (values (cons h-item t-items)
                      (append h-frames t-frames))))))

    (define (take ls n) (if (or (zero? n) (null? ls)) '() (cons (car ls) (take (cdr ls) (- n 1)))))
    (define (drop ls n) (if (or (zero? n) (null? ls)) ls (drop (cdr ls) (- n 1))))
    (define (Triv? x) (or (symbol? x) (integer? x)))
    (define (is-binop? x) (memq x '(+ - * sra logand logor)))

    (define (process-lambda lam-exp)
      (match lam-exp
        [(lambda ,formals ,body)
        (let* ([rp (unique-name 'rp)]
                [param-regs parameter-registers]
                [num-reg-params (length param-regs)]
                [reg-params (take formals num-reg-params)]
                [stack-params (drop formals num-reg-params)]
                [rp-assign `(set! ,rp ,return-address-register)]
                [reg-assigns (map (lambda (p r) `(set! ,p ,r)) reg-params (take param-regs (length reg-params)))]
                [stack-assigns (let loop ([ps stack-params] [i 0])
                                  (if (null? ps) '()
                                      (cons `(set! ,(car ps) ,(index->frame-var i))
                                            (loop (cdr ps) (+ i 1)))))]
                [all-assigns (cons rp-assign (append reg-assigns stack-assigns))])
          ;; 调用新的 icc-body 处理器
          (let-values ([(new-body all-nfvs frames-list) (icc-body body rp)])
            (let ([final-body `(begin ,@all-assigns ,new-body)])
              `(lambda ()
                  (locals (,@formals ,rp ,@all-nfvs)
                          ,(if (null? frames-list)
                              final-body
                              `(new-frames ,frames-list ,final-body)))))))]
        [,else (error 'process-lambda "expression is not a lambda" else)]))

    (define (icc-pred pred rp)
      (match pred
        [(true) (values '(true) '())]
        [(false) (values '(false) '())]
        [(if ,p ,c ,a)
        (let-values ([(new-p p-frames) (icc-pred p rp)])
          (let-values ([(new-c c-frames) (icc-pred c rp)])
            (let-values ([(new-a a-frames) (icc-pred a rp)])
              (values `(if ,new-p ,new-c ,new-a)
                      (append p-frames c-frames a-frames)))))]
        [(begin ,effects ... ,last-pred)
          (let-values ([(new-effects effects-frames) (map-values-and-append (lambda (e) (icc-effect e rp)) effects)])
            (let-values ([(new-last-pred last-pred-frames) (icc-pred last-pred rp)])
              (values (make-begin (append new-effects (list new-last-pred)))
                      (append effects-frames last-pred-frames))))]
        [(,op ,v1 ,v2)
        (let-values ([(new-v1 v1-frames) (icc-value v1 rp #f)])
          (let-values ([(new-v2 v2-frames) (icc-value v2 rp #f)])
            (values `(,op ,new-v1 ,new-v2)
                    (append v1-frames v2-frames))))]                      
        [,else (error 'icc-pred "unsupported predicate" else)]))

    ;; 新增 icc-effect 函数
    (define (icc-effect effect rp)
      (match effect
        [(nop) (values '(nop) '())]
        
        [(set! ,var ,val)
        (match val
          ;; 当 set! 的右侧是一个真正的非尾调用时...
          [(,rator ,rands ...) (guard (is-a-call? val))
            ;; ...执行正确的转换，将整个 set! 替换为一个 begin 块
            (let-values ([(rp-exp nfv-frame) (transform-nontail-call rator rands rp)])
              (values `(begin ,rp-exp (set! ,var ,return-value-register))
                      (list nfv-frame)))]           
          ;; 当 set! 的右侧是其他值 (Triv 或 binop)
          [,else
            (let-values ([(new-val val-frames) (icc-value val rp #f)])
              (values `(set! ,var ,new-val) val-frames))])]

        [(if ,p ,c ,a)
        (let-values ([(new-p p-frames) (icc-pred p rp)])
          (let-values ([(new-c c-frames) (icc-effect c rp)])
            (let-values ([(new-a a-frames) (icc-effect a rp)])
              (values `(if ,new-p ,new-c ,new-a)
                      (append p-frames c-frames a-frames)))))]
        
        [(begin ,effects ... ,last-effect)
          (let-values ([(new-effects effects-frames) (map-values-and-append (lambda (e) (icc-effect e rp)) effects)])
            (let-values ([(new-last-effect last-effect-frames) (icc-effect last-effect rp)])
              (values (make-begin (append new-effects (list new-last-effect)))
                      (append effects-frames last-effect-frames))))]

        [(,rator ,rands ...)(guard (is-a-call? effect))
        (let-values ([(rp-exp nfv-frame) (transform-nontail-call rator rands rp)])
          (values rp-exp (list nfv-frame)))] 
        [,else (error 'icc-effect "unsupported effect" else)]))

  (define (icc-tail exp rp)
    (match exp
      [(if ,p ,c ,a)
      (let-values ([(new-p p-frames) (icc-pred p rp)])
        (let-values ([(new-c c-frames) (icc-tail c rp)])
          (let-values ([(new-a a-frames) (icc-tail a rp)])
            (values `(if ,new-p ,new-c ,new-a)
                    (append p-frames c-frames a-frames)))))]
                    
      [(begin ,effects ... ,tail)
      (let-values ([(new-effects effects-frames) (map-values-and-append (lambda (e) (icc-effect e rp)) effects)])
        (let-values ([(new-tail tail-frames) (icc-tail tail rp)])
          (values (make-begin (append new-effects (list new-tail)))
                  (append effects-frames tail-frames))))]

      [(,op ,v1 ,v2) (guard (is-binop? op))
        (values `(begin (set! ,return-value-register (,op ,v1 ,v2))
                        (,rp ,frame-pointer-register ,return-value-register))
                '())]
                
      [,triv (guard (Triv? triv))
        (values `(begin (set! ,return-value-register ,triv)
                        (,rp ,frame-pointer-register ,return-value-register))
                '())]
      ;; 尾调用逻辑不变，但返回多值
      [(,rator ,rands ...)
      (let* ([param-regs parameter-registers]
              [num-reg-params (length param-regs)]
              [moves
              (let loop ([args rands] [i 0])
                (if (null? args)
                    '()
                    (let ([arg (car args)])
                      (if (< i num-reg-params)
                          (cons `(set! ,(list-ref param-regs i) ,arg) (loop (cdr args) (+ i 1)))
                          (let* ([stack-idx (- i num-reg-params)])
                            (cons `(set! ,(index->frame-var stack-idx) ,arg)
                                  (loop (cdr args) (+ i 1))))))))]
              [live-locs 
              (append (list frame-pointer-register return-address-register)
                      (take param-regs (length rands)) 
                      (let loop ([i 0] [num-stack-args (max 0 (- (length rands) num-reg-params))])
                        (if (= i num-stack-args) '()
                            (cons (index->frame-var i) (loop (+ i 1) num-stack-args))))
                      )])
        (values `(begin
                    ,@(reverse moves) ;; 使用 reverse 确保栈参数先赋值
                    (set! ,return-address-register ,rp)
                    (,rator ,@live-locs))
                '()))] ;; 尾调用不产生 new-frames              
      [,else (error 'icc-tail "unsupported tail expression" else)]))

    (define (icc-value value rp dest-var)
      (match value
        [,triv (guard (Triv? triv)) (values triv '())]

        [(,op ,v1 ,v2) (guard (is-binop? op))
        ;; binop 的操作数也可能是非尾调用
        (let-values ([(new-v1 v1-frames) (icc-value v1 rp #f)])
          (let-values ([(new-v2 v2-frames) (icc-value v2 rp #f)])
            (values `(,op ,new-v1 ,new-v2) (append v1-frames v2-frames))))]

        [(,rator ,rands ...)
        ;; Value 上下文中的非尾调用
        (let-values ([(rp-exp nfv-frame) (transform-nontail-call rator rands rp)])
          (values (if dest-var

                    `(begin ,rp-exp (set! ,dest-var ,return-value-register) ,dest-var)
                      ;; 否则，引入临时变量
                      (let ([tmp (unique-name 'tmp)])
                        `(begin ,rp-exp (set! ,tmp ,return-value-register) ,tmp)))
                  (if (null? nfv-frame) '() (list nfv-frame))))]
        [,else (error 'icc-value "unsupported value" else)]))

    (define (icc-body body rp)
      (match body
        [(locals ,vars ,tail)
        (let-values ([(new-tail frames) (icc-tail tail rp)])
          (values new-tail (apply append vars frames) frames))]
        [,tail
        (let-values ([(new-tail frames) (icc-tail tail rp)])
          (values new-tail (apply append frames) frames))]))

    ; (define (process-body body rp)
    ;    (match body
    ;      [(locals ,vars ,tail)
    ;       `(locals (,@vars ,rp)
    ;                (begin (set! ,rp ,return-address-register)
    ;                       ,(icc-tail tail rp)))]
    ;      [,tail `(locals (,rp)
    ;                      (begin (set! ,rp ,return-address-register)
    ;                             ,(icc-tail tail rp)))]))

    (match program
      [(letrec ,bindings ,main-body)
      (let* ([new-bindings (map (lambda (b) `(,(car b) ,(process-lambda (cadr b)))) bindings)]
              [main-rp (unique-name 'rp)])
          (let-values ([(new-body all-nfvs frames-list) (icc-body main-body main-rp)])
            (let ([final-body `(begin
                                (set! ,main-rp ,return-address-register)
                                ,new-body)])
              `(letrec ,new-bindings
                (locals (,main-rp ,@all-nfvs)
                        ,(if (null? frames-list)
                              final-body
                              `(new-frames ,frames-list ,final-body)))))))]
      [,body (error 'impose-calling-conventions "program must be a letrec" body)])))