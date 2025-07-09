(define pre-assign-frame
  (lambda (program)
    (define (spills->locate spilled-vars frame-graph tail)
      (if (null? spilled-vars)
          ;; If there's nothing to spill, just replace `spills` with an empty `locate`.
          `(locate () (frame-conflict ,frame-graph ,tail))
          
          (letrec ([get-var-conflicts (lambda (var graph)
                                        (let ([entry (assq var graph)])
                                          (if entry (cdr entry) '())))]
                   [do-assignment (lambda (spills-to-assign current-assignments)
                                    (if (null? spills-to-assign)
                                        current-assignments
                                        (let* ([v (car spills-to-assign)]
                                               [used-fvars (map cadr current-assignments)]
                                               [v-conflicts (get-var-conflicts v frame-graph)])
                                          (let loop ([idx 0])
                                            (let ([candidate-fv (index->frame-var idx)])
                                              (if (or (memq candidate-fv used-fvars)
                                                      (memq candidate-fv v-conflicts))
                                                  (loop (+ idx 1))
                                                  (do-assignment
                                                   (cdr spills-to-assign)
                                                   (cons `(,v ,candidate-fv) current-assignments))))))))])
            (let ([new-assignments (do-assignment spilled-vars '())])
              `(locate ,new-assignments (frame-conflict ,frame-graph ,tail))))))

    (define (process-body body)
      (match body
        ;; Case 1: Body has non-tail calls and a new-frames wrapper. 
        [(locals ,locals-vars
          (new-frames ,frames-list
            (spills ,spilled-vars
              (frame-conflict ,frame-graph ,tail))))
        `(locals ,(difference locals-vars spilled-vars) ; <-- 修改这里
            (new-frames ,frames-list
              ,(spills->locate spilled-vars frame-graph tail)))]

        ;; Case 2: Body has NO non-tail calls, so no new-frames wrapper.
        [(locals ,locals-vars
          (spills ,spilled-vars
            (frame-conflict ,frame-graph ,tail)))
        `(locals ,(difference locals-vars spilled-vars) ; <-- 修改这里
            ,(spills->locate spilled-vars frame-graph tail))]

        ;; If the body has no spills form at all, do nothing.
        [,else body]))

    ;; The top-level traversal is standard.
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