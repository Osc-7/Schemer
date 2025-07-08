(define flatten-set!
  (lambda (program)
    (define (flatten-rhs var rhs)
      (match rhs
        ;; 规则 1: 右侧是 begin
        [(begin ,effects ... ,last-exp)
         `(begin ,@effects ,(flatten-rhs var last-exp))]

        ;; 规则 2: 右侧是 if
        [(if ,p ,c ,a)
         `(if ,p ,(flatten-rhs var c) ,(flatten-rhs var a))]

        ;; 基本情况: 右侧不是 if 或 begin，递归结束
        [,else
         `(set! ,var ,else)]))

    (define (flat exp)
      (match exp
        ;; 对于 set! 表达式，应用 flatten-rhs 规则
        [(set! ,var ,val)
         (flatten-rhs var (flat val))]

        ;; 对于其他复合表达式，递归遍历其子表达式
        [(letrec ,bindings ,main-body)
         (let ([new-main-body (flat main-body)]
               [new-bindings (map (lambda (binding)
                                    (match binding
                                      [(,label (lambda ,formals ,body))
                                       `(,label (lambda ,formals ,(flat body)))]
                                      [,else binding]))
                                  bindings)])
           `(letrec ,new-bindings ,new-main-body))]

        [(locals ,vars ,tail)
         `(locals ,vars ,(flat tail))]
        [(if ,p ,c ,a)
         `(if ,(flat p) ,(flat c) ,(flat a))]
        [(begin ,effects ... ,tail)
         `(begin ,@(map flat effects) ,(flat tail))]
        
        ;; 原子表达式，直接返回
        [,else exp]))

    (flat program)))