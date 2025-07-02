(define flatten-program
  (lambda (program)
    (define flatten-one-tail
      (lambda (tail next-label)
        (match tail
          ;; 匹配条件跳转
          [(if (,op ,a ,b) (,then-label) (,else-label))

           (if (eq? then-label next-label)
               `((if (not (,op ,a ,b)) (jump ,else-label)))

               (if (eq? else-label next-label)
                   `((if (,op ,a ,b) (jump ,then-label)))

                   `((if (,op ,a ,b) (jump ,then-label))
                     (jump ,else-label))))]

          ;; 匹配无条件跳转
          [(,label) (guard (label? label))
           ;; 如果跳转目标不是下一个块，则生成 jump 指令
           (if (eq? label next-label)
               '()
               `((jump ,label)))]

          ;; 匹配 begin 块
          [(begin ,effects ... ,last)
           (append effects (flatten-one-tail last next-label))]

          [,else
           (error 'flatten-program "Invalid tail expression received" else)])))

    ;; 主逻辑
    (match program
      [(letrec ,bindings ,main-body)
       (let ([main-body-code
              (flatten-one-tail
               main-body
               (and (pair? bindings) (caar bindings)))])
         (let loop ([b* bindings])
           (match b*
             ['() '()]
             [(,binding . ,rest)
              (match binding
                [(,label (lambda () ,body))
                 (let ([next-label (and (pair? rest) (caar rest))])
                   (cons label
                         (append (flatten-one-tail body next-label)
                                 (loop rest))))])])))

         `(code ,@(append main-body-code (loop bindings)))])))

      ; [else
      ;  (error 'flatten-program "Invalid program structure for flatten-program" else)]))