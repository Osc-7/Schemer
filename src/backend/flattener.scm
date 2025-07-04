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
          [(,target) ; 匹配任何单元素列表作为跳转目标
          (if (label? target)
              (if (eq? target next-label)
                  '()
                  `((jump ,target)))
              `((jump ,target)))]

          ;; 匹配 begin 块
          [(begin ,effects ... ,last)
           (append effects (flatten-one-tail last next-label))]

          [,else
           (error 'flatten-program "Invalid tail expression received" else)])))

   (match program
      [(letrec ,bindings ,main-body)
       (let ([main-body-code
              (flatten-one-tail
               main-body
               (and (pair? bindings) (caar bindings)))])
         
         (letrec
             ([loop (lambda (b*)
                      (match b*
                        ;; 匹配空列表，返回空列表
                        [() '()] 
                        ;; 匹配非空列表
                        [(,binding . ,rest)
                         (match binding
                           [(,label (lambda () ,body))
                            (let ([next-label (and (pair? rest) (caar rest))])
                              (cons label
                                    (append (flatten-one-tail body next-label)
                                            (loop rest))))])]))])
           
           `(code ,@(append main-body-code (loop bindings)))))])))
      
      ; [,else
      ;  (error 'flatten-program "Invalid program structure for flatten-program" else)]))