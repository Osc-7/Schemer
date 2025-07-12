(define expose-frame-var
  (lambda (program)
    (define (walk-list l initial-offset)
      (let loop ([ls l] [current-offset initial-offset])
        (if (null? ls)
            (values '() current-offset)
            (let-values ([(new-hd off-hd) (walk (car ls) current-offset)])
              (let-values ([(new-tl off-tl) (loop (cdr ls) off-hd)])
                (values (cons new-hd new-tl) off-tl))))))

    (define (walk exp offset)
      (match exp
        ;; Base case: 替换 frame var, 并根据当前 fp 偏移量调整地址
        [,x (guard (symbol? x))
            (if (frame-var? x)
                ;; 这个计算逻辑是正确的：最终偏移 = 静态偏移 - 动态偏移
                (values (make-disp-opnd frame-pointer-register
                                        (- (ash (frame-var->index x) align-shift) offset))
                        offset)
                (values x offset))]

        [,x (guard (not (pair? x))) (values x offset)]

        [(set! ,var (+ ,rator ,nb)) (guard (and (eq? var frame-pointer-register) (eq? rator frame-pointer-register)))
         (values `(set! ,var (+ ,rator ,nb)) (+ offset nb))]

        [(set! ,var (- ,rator ,nb)) (guard (and (eq? var frame-pointer-register) (eq? rator frame-pointer-register)))
         (values `(set! ,var (- ,rator ,nb)) (- offset nb))]

        [(return-point ,label ,tail)
         (let-values ([(new-tail new-offset) (walk tail offset)])
           (values `(return-point ,label ,new-tail) new-offset))]

        [(if ,p ,t ,e)
         (let-values ([(new-p p-offset) (walk p offset)])
           (let-values ([(new-t t-offset) (walk t p-offset)])
             (let-values ([(new-e e-offset) (walk e p-offset)])
               (when (not (= t-offset e-offset))
                 (error 'expose-frame-var "if branches produce different frame offsets"))
               (values `(if ,new-p ,new-t ,new-e) t-offset))))]

        [(begin ,effects ... ,tail)
         (let-values ([(new-effects final-offset) (walk-list effects offset)])
           (let-values ([(new-tail final-offset-tail) (walk tail final-offset)])
             (values (make-begin (append new-effects (list new-tail))) final-offset-tail)))]
        
        [(set! ,var ,val)
         (let-values ([(new-val val-offset) (walk val offset)])      ; 先处理 RHS，获得新偏移量
           (let-values ([(new-var var-offset) (walk var val-offset)]) ; 再用新偏移量处理 LHS
             (values `(set! ,new-var ,new-val) var-offset)))]
        
        [(letrec ,bindings ,main-body)
         (let-values ([(new-bindings bindings-offset)
                       (let loop ([b bindings] [current-offset offset])
                         (if (null? b)
                             (values '() current-offset)
                             (match (car b)
                               [(,label (lambda () ,body))
                                (let-values ([(new-body off-body) (walk body current-offset)])
                                  (when (not (= off-body current-offset))
                                    (error 'expose-frame-var "lambda body changes frame offset"))
                                  (let-values ([(new-rest off-rest) (loop (cdr b) current-offset)])
                                    (values (cons `(,label (lambda () ,new-body)) new-rest) off-rest)))])))])
           (let-values ([(new-main-body main-offset) (walk main-body bindings-offset)])
             (values `(letrec ,new-bindings ,new-main-body) main-offset)))]
        
      
        [(,rator . ,rands)
         (let-values ([(new-rands final-offset)
                       (walk-list rands offset)])
           ;; 在所有参数求值完毕后，再处理调用操作符本身
           (let-values ([(new-rator rator-offset) (walk rator final-offset)])
             (values (cons new-rator new-rands) rator-offset)))]

        [,else (values else offset)]))

    (let-values ([(result offset) (walk program 0)])
      (unless (= offset 0)
        (error 'expose-frame-var "final frame offset is not zero"))
      result)))