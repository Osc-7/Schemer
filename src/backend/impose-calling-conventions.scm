(define impose-calling-conventions
  (lambda (program)

    (define (take ls n) (if (or (zero? n) (null? ls)) '() (cons (car ls) (take (cdr ls) (- n 1)))))
    (define (drop ls n) (if (or (zero? n) (null? ls)) ls (drop (cdr ls) (- n 1))))
    (define (Triv? x) (or (symbol? x) (integer? x)))
    (define (is-binop? x) (memq x '(+ - * sra logand logor)))

    (define (process-lambda lam-exp)
      (match lam-exp
        [(lambda ,formals ,body)
         (let* ([new-rp (unique-name 'rp)]
                [param-regs parameter-registers]
                [num-reg-params (length param-regs)]
                [reg-params (take formals num-reg-params)]
                [stack-params (drop formals num-reg-params)]
                [rp-assign `(set! ,new-rp ,return-address-register)]
                [reg-assigns (map (lambda (p r) `(set! ,p ,r)) reg-params (take param-regs (length reg-params)))]
                [stack-assigns (let loop ([ps stack-params] [i 0])
                                 (if (null? ps) '()
                                     (cons `(set! ,(car ps) ,(index->frame-var i))
                                           (loop (cdr ps) (+ i 1)))))]
                [all-assigns (cons rp-assign (append reg-assigns stack-assigns))]
                [new-body (match body
                            [(locals ,vars ,tail)
                             `(locals (,@vars ,@formals ,new-rp)
                                      (begin ,@all-assigns ,(icc-tail tail new-rp)))]
                            [,tail `(locals (,@formals ,new-rp)
                                             (begin ,@all-assigns ,(icc-tail tail new-rp)))])])
           `(lambda () ,new-body))]
        [,else (error 'process-lambda "expression is not a lambda" else)]))


    (define (icc-tail exp rp)
      (match exp
        ;; 转换三: 返回一个计算值
        [(,op ,v1 ,v2) (guard (is-binop? op))
         `(begin (set! ,return-value-register (,op ,v1 ,v2))
                 (,rp ,frame-pointer-register ,return-value-register))]

        [(if ,p ,c ,a)
         `(if ,p ,(icc-tail c rp) ,(icc-tail a rp))]

        [(begin ,effects ... ,tail)
         `(begin ,@effects ,(icc-tail tail rp))]
        
        [(,rator ,rands ...)
         (let* ([param-regs parameter-registers]
                [num-reg-params (length param-regs)]
;; 保证副作用之间的顺序
                [moves
                 (let loop ([args rands] [i 0])
                   (if (null? args)
                       '()
                       (let ([arg (car args)])
                         (if (< i num-reg-params)
                             ;; 寄存器参数
                             (cons `(set! ,(list-ref param-regs i) ,arg)
                                   (loop (cdr args) (+ i 1)))
                             ;; 栈参数
                             (let* ([stack-idx (- i num-reg-params)]
                                    [scratch-reg return-value-register])
                               (list* `(set! ,scratch-reg ,arg)
                                      `(set! ,(index->frame-var stack-idx) ,scratch-reg)
                                      (loop (cdr args) (+ i 1))))))))]
                [live-locs 
                 (append (list frame-pointer-register return-address-register)
                         (take param-regs (length rands)) 
                         (let loop ([i 0] [num-stack-args (max 0 (- (length rands) num-reg-params))])
                           (if (= i num-stack-args) '()
                               (cons (index->frame-var i) (loop (+ i 1) num-stack-args))))
                         )])
           `(begin
              ,@moves
              (set! ,return-address-register ,rp)
              (,rator ,@live-locs)))]

        [,triv (guard (Triv? triv))
         `(begin (set! ,return-value-register ,triv)
                 (,rp ,frame-pointer-register ,return-value-register))]
                 
        [,else (error 'icc-tail "unsupported tail expression" else)]))
    
    (define (process-body body rp)
       (match body
         [(locals ,vars ,tail)
          `(locals (,@vars ,rp)
                   (begin (set! ,rp ,return-address-register)
                          ,(icc-tail tail rp)))]
         [,tail `(locals (,rp)
                         (begin (set! ,rp ,return-address-register)
                                ,(icc-tail tail rp)))]))

    (match program
      [(letrec ,bindings ,main-body)
       (let* ([main-body-rp (unique-name 'rp)]
              [new-main-body (process-body main-body main-body-rp)]
              [new-bindings (map (lambda (b) (match b [(,_ ,lam) `(,(car b) ,(process-lambda lam))])) bindings)])
         `(letrec ,new-bindings ,new-main-body))]
       [,body 
        (let ([main-rp (unique-name 'rp)])
          (process-body body main-rp))]
      [,else (error 'impose-calling-conventions "program is not a letrec" else)])))