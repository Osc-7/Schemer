(define introduce-allocation-forms
  (lambda (program)
    (define (process-body body)
      (match body
        [(locals ,vars (frame-conflict ,graph ,tail))
         `(locals ,vars
            (ulocals ()
              (locate ()
                (frame-conflict ,graph ,tail))))]
        
        [,else body]))

    (match program
      [(letrec ,bindings ,main-body)
       (let ([new-main-body (process-body main-body)]
             [new-bindings
              (map
               (lambda (binding)
                 (match binding
                   [(,label (lambda () ,body))
                    `(,label (lambda () ,(process-body body)))]))
               bindings)])
         `(letrec ,new-bindings ,new-main-body))]
      
      [,else program])))