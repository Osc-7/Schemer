(define assign-frame
  (lambda (program)
    (define (process-body body)
      (match body
        [(locals ,locals-vars
           (ulocals ,ulocals-vars
             (spills ,spilled-vars
               (locate ,locate-bindings
                 (frame-conflict ,frame-graph ,tail)))))

         (if (null? spilled-vars)
             body
             (letrec (
                [do-assignment (lambda (spills-to-assign current-assignments)
                  (if (null? spills-to-assign)
                      current-assignments
                      (let* ([v (car spills-to-assign)]
                             [used-fvars (map cadr current-assignments)])
                        
                        ;; 从 fv0 开始寻找一个没有被占用的位置
                        (let loop ([idx 0])
                          (let ([candidate-fv (index->frame-var idx)])
                            (if (memq candidate-fv used-fvars)
                                ;; 如果已被占用，就去寻找下一个
                                (loop (+ idx 1))
                                (do-assignment 
                                  (cdr spills-to-assign)
                                  (cons `(,v ,candidate-fv) current-assignments))))))))])
               
               (let ([new-assignments (do-assignment spilled-vars locate-bindings)])
                  `(locals ,locals-vars
                     (ulocals ,ulocals-vars
                       (locate ,new-assignments
                         (frame-conflict ,frame-graph ,tail)))))))]

        [(locate ,a ,b) body]
        [,else body]))

    (match program
      [(letrec ,bindings ,main-body)
       (let ([new-main-body (process-body main-body)]
             [new-bindings (map (lambda (binding)
                                  (match binding
                                    [(,label (lambda () ,body))
                                     `(,label (lambda () ,(process-body body)))]))
                                bindings)])
         `(letrec ,new-bindings ,new-main-body))]
      [,else program])))