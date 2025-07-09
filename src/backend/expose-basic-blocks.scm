(define expose-basic-blocks
  (lambda (program)
    ;; 'walk' is the main mutually recursive helper with 'walk-pred' and 'walk-effect'.
    ;; It processes a Tail expression and returns two values:
    ;; 1. A list of new (label . lambda) bindings created.
    ;; 2. The new, transformed Tail expression.

    (define (wrap-begin effect tail)
      (match tail
        ;; 如果 tail 已经是一个 begin, 就把 effect 插入到最前面
        [(begin ,exprs ...) `(begin ,effect ,@exprs)]
        ;; 否则，创建一个新的 begin
        [,else `(begin ,effect ,else)]))

    (define (walk tail)
      (match tail
        ;; Case 1: if expression in Tail context
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

        ;; Case 2: begin expression
        [(begin ,effect . ,rest)
         (if (null? rest)
             (walk effect)
             (walk-effect effect `(begin ,@rest)))]

        [(,triv)
        (values '() tail)]

        ;; Base Case: A simple tail (like a jump or a value)
        [,else
        (values '() tail)]))

    (define (walk-pred pred true-label false-label)
      (match pred
        [(true)  (values '() `(,true-label))]
        [(false) (values '() `(,false-label))]
        [(,relop ,triv1 ,triv2) (guard (memq relop '(< <= = >= >)))
         (values '() `(if (,relop ,triv1 ,triv2) (,true-label) (,false-label)))]

        [(begin ,e1 ,e* ... ,last-pred)
          ;; 创建一个新标签，用于effect执行完毕后跳转
          (let ([cont-label (unique-label 'cont)]) 
            (let-values ([(effect-bindings effect-code) (walk-effect `(begin ,e1 ,@e*) `(,cont-label))])
              (let-values ([(pred-bindings pred-code) (walk-pred last-pred true-label false-label)])
                (values
                  (append effect-bindings
                          pred-bindings
                          `((,cont-label (lambda () ,pred-code))))
                  effect-code))))]

        [(if ,p1 ,p2 ,p3)
         (let ([p2-label (unique-label 'pred)]
               [p3-label (unique-label 'pred)])
           (let-values ([(p1-bindings p1-code) (walk-pred p1 p2-label p3-label)])
             (let-values ([(p2-bindings p2-code) (walk-pred p2 true-label false-label)])
               (let-values ([(p3-bindings p3-code) (walk-pred p3 true-label false-label)])
                 (values (append p1-bindings p2-bindings p3-bindings
                                 `((,p2-label (lambda () ,p2-code))
                                   (,p3-label (lambda () ,p3-code))))
                         p1-code)))))]
        [other (error 'expose-basic-blocks "Invalid predicate form" other)]))
    
;; 替换旧的 walk-effect 函数
(define (walk-effect effect cont)
  (match effect
    ;; A7 新增: 处理 return-point
    [(return-point ,rp-label ,inner-tail)
     (let-values ([(cont-bindings cont-code) (walk cont)])
       (let-values ([(tail-bindings tail-code) (walk inner-tail)])
         (values (append tail-bindings
                         cont-bindings
                         `((,rp-label (lambda () ,cont-code))))
                 tail-code)))]

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
    
    [(begin ,e1 ,e* ...)
     (walk-effect e1 `(begin ,@e* ,cont))]

    [(nop)
     (walk cont)]
     
    [,else
      (let-values ([(cont-bindings cont-code) (walk cont)])
        (values cont-bindings (wrap-begin effect cont-code)))]))

    (match program
      [(letrec ,bindings ,main-body)
       (let-values ([(main-bindings new-main) (walk main-body)])
         (let-values ([(lambda-bindings-list new-lambdas)
                       (unzip (map (lambda (binding)
                                     (match binding
                                       [(,label (lambda () ,body))
                                        (let-values ([(body-bindings new-body) (walk body)])
                                      (list body-bindings `(,label (lambda () ,new-body))))]))
                                   bindings))])
           `(letrec ,(append (apply append lambda-bindings-list)
                             main-bindings
                             new-lambdas)
                    ,new-main)))]
      [other (error 'expose-basic-blocks "Invalid program structure" other)])))