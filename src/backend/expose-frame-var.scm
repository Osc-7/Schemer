(define expose-frame-var
  (lambda (program)
    (define walk
      (lambda (expr)
        (match expr
        ;; Base case
          [,x (guard (symbol? x))
              (if (frame-var? x)
                  (make-disp-opnd 'rbp (ash (frame-var->index x) align-shift))
                  x)]

          ;; recursive case
          [(letrec ([,label (lambda () ,[walk -> body])] ...) ,[walk -> main-body])
           `(letrec ([,label (lambda () ,body)] ...) ,main-body)]
          
          [(if ,[walk -> pred] ,[walk -> then-c] ,[walk -> else-c])
           `(if ,pred ,then-c ,else-c)]

          [(begin ,[walk -> effects] ... ,[walk -> tail])
           `(begin ,effects ... ,tail)]

          [(set! ,[walk -> var] (,[op] ,[walk -> triv1] ,[walk -> triv2]))
           `(set! ,var (,op ,triv1 ,triv2))]

          [(set! ,[walk -> var] ,[walk -> triv])
           `(set! ,var ,triv)]

          [(,op ,[walk -> triv1] ,[walk -> triv2]) (guard (memq op '(< <= = >= >)))
           `(,op ,triv1 ,triv2)]

          [(,[walk -> triv])
           `(,triv)]

          [,else else])))

    (walk program)))