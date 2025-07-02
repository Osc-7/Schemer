(define expose-basic-blocks
  (lambda (program)
    ;; 'walk' 是主递归函数，处理 Tail (函数体) 表达式。
    ;; 它返回两个值：转换后的新函数体，以及一个新创建的 letrec 绑定列表。
    (define (walk tail)
      (match tail
        ;; (if pred then-tail else-tail)
        [(if ,pred ,then-e ,else-e)
         (let ([then-label (unique-label 'then)]
               [else-label (unique-label 'else)])
           (let-values ([(pred-bindings pred-code) (walk-pred pred then-label else-label)])
             (let-values ([(then-bindings then-code) (walk then-e)])
               (let-values ([(else-bindings else-code) (walk else-e)])
                 (values
                  (append pred-bindings
                          then-bindings
                          else-bindings
                          `((,then-label (lambda () ,then-code))
                            (,else-label (lambda () ,else-code))))
                  pred-code)))))]

        ;; (begin effect... tail)
        [(begin ,e1 ,e* ... ,last)
         (let-values ([(effect-bindings effect-code) (walk-effect e1 `(begin ,@e* ,last))])
           (values effect-bindings effect-code))]

        ;; 其他情况 (Triv 或无参数的 jump)
        [,else (values '() tail)]))

    ;; 'walk-pred' 处理 Predicate (判断条件) 表达式。
    ;; 它接收三个参数: 判断表达式、为 'true' 时应跳转的标签、为 'false' 时应跳转的标签。
    ;; 返回两个值: 新的绑定列表，以及一个实现跳转的 Tail 表达式。
    (define (walk-pred pred true-label false-label)
      (match pred
        [(true)  (values '() `(,true-label))]
        [(false) (values '() `(,false-label))]

        ;; (relop a b) 这是最基本的情况
        [(,relop ,triv1 ,triv2) (guard (memq relop '(< <= = >= >)))
         (values '() `(if (,relop ,triv1 ,triv2) (,true-label) (,false-label)))]

        ;; (if p1 p2 p3) 嵌套的 if
        [(if ,p1 ,p2 ,p3)
         (let ([p2-label (unique-label 'pred)]
               [p3-label (unique-label 'pred)])
           (let-values ([(p1-bindings p1-code) (walk-pred p1 p2-label p3-label)])
             (let-values ([(p2-bindings p2-code) (walk-pred p2 true-label false-label)])
               (let-values ([(p3-bindings p3-code) (walk-pred p3 true-label false-label)])
                 (values
                  (append p1-bindings
                          p2-bindings
                          p3-bindings
                          `((,p2-label (lambda () ,p2-code))
                            (,p3-label (lambda () ,p3-code))))
                  p1-code)))))]
        
        ;; (begin effect... pred)
        [(begin ,e* ... ,last-pred)
         (let-values ([(effect-bindings effect-code)
                       (walk-effect `(begin ,@e*) `(,last-pred))])
           (let-values ([(pred-bindings pred-code)
                         (walk-pred last-pred true-label false-label)])
             (values (append effect-bindings pred-bindings)
                     (let-values ([(new-bindings new-tail)
                                   (walk `(begin ,effect-code ,pred-code))])
                       (error 'expose-basic-blocks "Unexpected bindings in pred begin")))))]

        [,else (error 'expose-basic-blocks "Invalid predicate form" pred)]))

    ;; 'walk-effect' 处理 Effect (副作用) 表达式。
    ;; 它接收两个参数: 当前要处理的 effect，以及它后面所有的代码 ('continuation')。
    ;; 返回两个值: 新的绑定列表，以及转换后的 Tail 表达式。
    (define (walk-effect effect cont)
      (match effect
        ;; (if pred then-effect else-effect)
        [(if ,pred ,then-e ,else-e)
         (let ([then-label (unique-label 'then)]
               [else-label (unique-label 'else)]
               [join-label (unique-label 'join)])
           (let-values ([(pred-bindings pred-code) (walk-pred pred then-label else-label)])
             (let-values ([(then-bindings then-code) (walk-effect then-e `(,join-label))])
               (let-values ([(else-bindings else-code) (walk-effect else-e `(,join-label))])
                 (let-values ([(cont-bindings cont-code) (walk cont)])
                   (values
                    (append pred-bindings
                            then-bindings
                            else-bindings
                            cont-bindings
                            `((,then-label (lambda () ,then-code))
                              (,else-label (lambda () ,else-code))
                              (,join-label (lambda () ,cont-code))))
                    pred-code))))))]
        
        ;; (begin effect... effect)
        [(begin ,e1 ,e* ...)
         (walk-effect e1 `(begin ,@e* ... ,@cont))]

        ;; 基本的副作用表达式是递归的终点
        [,else (walk `(begin ,effect ,cont))]))

    (match program
      [(letrec ,bindings ,main-body)
       (let-values ([(main-body-bindings new-main-body) (walk main-body)])
         (let ([new-bindings
                (append
                 main-body-bindings
                 (apply
                  append
                  (map
                   (lambda (binding)
                     (match binding
                       [(,label (lambda () ,body))
                        (let-values ([(body-bindings new-body) (walk body)])
                          (cons `(,label (lambda () ,new-body)) body-bindings))]))
                   bindings)))])
           `(letrec ,new-bindings ,new-main-body)))]
      [,else (error 'expose-basic-blocks "Invalid program structure" program)])))