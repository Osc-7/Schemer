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
                [get-var-conflicts (lambda (var graph)
                                     (let ([entry (assq var graph)])
                                       (if entry (cdr entry) '())))]
                [do-assignment (lambda (spills-to-assign current-assignments)
                  (if (null? spills-to-assign)
                      current-assignments
                      (let* ([v (car spills-to-assign)]
                             [used-fvars (map cadr current-assignments)]
                             [v-conflicts (get-var-conflicts v frame-graph)])
                        
                        ;; 从 fv0 开始寻找一个没有被占用且不冲突的位置
                        (let loop ([idx 0])
                          (let ([candidate-fv (index->frame-var idx)])
                            (if (or (memq candidate-fv used-fvars)
                                    (memq candidate-fv v-conflicts))
                                ;; 如果已被占用或存在冲突，就去寻找下一个
                                (loop (+ idx 1))
                                ;; 否则，分配并继续处理下一个spill
                                (do-assignment 
                                  (cdr spills-to-assign)
                                  (cons `(,v ,candidate-fv) current-assignments))))))))])
               
               (let ([new-assignments (do-assignment spilled-vars locate-bindings)])
                  `(locals ,locals-vars
                     (ulocals ,ulocals-vars
                       (locate ,new-assignments
                         (frame-conflict ,frame-graph ,tail)))))))]

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