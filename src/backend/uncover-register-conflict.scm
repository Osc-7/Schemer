(define uncover-register-conflict
  (lambda (program)
    (define (backward body)
      (define (build-conflict-graph vars tail)
        (let ([graph (map (lambda (v) (list v)) vars)])

          (define (live-in-triv triv)
            (if (or (uvar? triv) (register? triv)) (list triv) '()))

          (define (add-conflict! var1 var2)
            (let loop ([g graph])
              (cond
                [(null? g) '()]
                [(eq? (caar g) var1)
                 (let ([conflicts (car g)])
                   (unless (member var2 (cdr conflicts))
                     (set-cdr! conflicts (cons var2 (cdr conflicts)))))]
                [else (loop (cdr g))])))

          (define (add-conflicts! var live-set)
            (cond
              [(uvar? var)
               ;; Original logic for uvar targets
               (for-each
                 (lambda (live-item)
                   (when (not (eq? var live-item))
                     (cond
                       [(uvar? live-item)
                        (add-conflict! var live-item)
                        (add-conflict! live-item var)]
                       [(register? live-item)
                        (add-conflict! var live-item)])))
                 live-set)]
              [(register? var)
               ;; New logic for register targets:
               ;; The destination register conflicts with all live uvars.
               (for-each
                 (lambda (live-item)
                   (when (uvar? live-item)
                     (add-conflict! live-item var)))
                 live-set)]))

          (define (walk-pred pred live-true live-false)
            (match pred
              [(if ,p1 ,p2 ,p3)
               (let ([live-p2 (walk-pred p2 live-true live-false)]
                     [live-p3 (walk-pred p3 live-true live-false)])
                 (walk-pred p1 live-p2 live-p3))]
              [(begin ,effects ... ,last-pred)
               (fold-right walk-effect (walk-pred last-pred live-true live-false) effects)]
              [(,relop ,triv1 ,triv2)
               (union (live-in-triv triv1) (live-in-triv triv2) live-true live-false)]
              [(true) live-true]
              [(false) live-false]               
              [else-pred (error 'uncover-register-conflict "Invalid Predicate expression" else-pred)]))

          (define (walk-effect effect live-set)
            (match effect
              [(nop) live-set]
              [(return-point ,label ,tail)
                (walk-tail tail)]
              [(set! ,var (,binop ,triv1 ,triv2))
               (let* ([live-after (difference live-set (list var))]
                      [live-rhs (union (live-in-triv triv1) (live-in-triv triv2))]
                      [new-live (union live-after live-rhs)])
                 (add-conflicts! var live-after)
                 new-live)]
              [(set! ,var ,triv)
               (let* ([live-after (difference live-set (list var))]
                      [live-rhs (live-in-triv triv)]
                      [new-live (union live-after live-rhs)])
                 (add-conflicts! var (difference live-after live-rhs))
                 new-live)]
              [(if ,pred ,eff1 ,eff2)
               (let* ([live1 (walk-effect eff1 live-set)]
                      [live2 (walk-effect eff2 live-set)])
                 (walk-pred pred live1 live2))]
              [(begin ,effects ... ,last-effect)
               (fold-right walk-effect (walk-effect last-effect live-set) effects)]
              [else-effect (error 'uncover-register-conflict "Invalid Effect expression" else-effect)]))

          (define (walk-tail t)
            (match t
              [(if ,pred ,then-tail ,else-tail)
               (let ([live-then (walk-tail then-tail)]
                     [live-else (walk-tail else-tail)])
                 (walk-pred pred live-then live-else))]
              [(begin ,effects ... ,last-tail)
               (fold-right walk-effect (walk-tail last-tail) effects)]
              [(,trv ,live-locs ...)
               (union (live-in-triv trv) live-locs)]
              [else-tail (error 'uncover-register-conflict "Invalid Tail expression" else-tail)]))
          
          (walk-tail tail)
          graph))

      (match body
        [(locate ,bindings ,tail)
         body]

        [(locals ,local-vars
           (ulocals ,ulocal-vars
             (locate ,frame-bindings
               (frame-conflict ,frame-graph ,tail))))
         
         (let* ([all-vars (union local-vars ulocal-vars)]
                [conflict-graph (build-conflict-graph all-vars tail)])
           `(locals ,local-vars
              (ulocals ,ulocal-vars
                (locate ,frame-bindings
                  (frame-conflict ,frame-graph
                    (register-conflict ,conflict-graph ,tail))))))]
        
        ;; Fallback for a4-style bodies.
        [(locals ,vars-list ,tail)
         (let ([conflict-graph (build-conflict-graph vars-list tail)])
           `(locals ,vars-list (register-conflict ,conflict-graph ,tail)))]
        
        [else-body (error 'uncover-register-conflict "Invalid body structure for uncover-register-conflict" else-body)]))

    (match program
      [(letrec ,bindings ,main-body)
       (let ([new-main-body (backward main-body)]
             [new-bindings (map (lambda (binding)
                                  (match binding
                                    [(,label (lambda () ,body))
                                     `(,label (lambda () ,(backward body)))]
                                    [else-binding (error 'uncover-register-conflict "Invalid binding" else-binding)]))
                                bindings)])
         `(letrec ,new-bindings ,new-main-body))]
      [else-program (error 'uncover-register-conflict "Invalid program structure" else-program)])))