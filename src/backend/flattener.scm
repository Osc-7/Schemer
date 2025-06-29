(define flatten-program
  (lambda (program)
    (define flatten-tail
      (lambda (tail)
        (match tail
          [(begin ,effects ... ,last)
           (append effects (flatten-tail last))]

          [(,triv)
           (list `(jump ,triv))]

          [,other-effect
           (list other-effect)])))

    (match program
      ;; 匹配 (letrec bindings main-body)
      [(letrec ,bindings ,main-body)
       (let ([flat-main (flatten-tail main-body)])
         (let ([flat-bindings
                (apply
                 append
                 (map
                  (lambda (binding)
                    (match binding
                      [(,label (lambda () ,body))
                       (cons label (flatten-tail body))]
                      [,else
                       (error 'flatten-program "Invalid letrec binding format" else)]))
                  bindings))]) 
           `(code ,@(append flat-main flat-bindings))))]
      [,else
       (error 'flatten-program "Invalid program structure for flatten-program" else)])))