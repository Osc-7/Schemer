(define uncover-register-conflict
  (lambda (program)

    (define (process-body body)
      (match body

        [(locals ,locals-vars
           (ulocals ,ulocals-vars
             (locate ,locate-bindings
               (frame-conflict ,frame-graph ,tail))))

         (let* ([all-vars (union locals-vars ulocals-vars)]
                [register-graph (build-conflict-graph all-vars tail)])
           
           ;; 构建正确的输出结构
           `(locals ,locals-vars
              (ulocals ,ulocals-vars
                (locate ,locate-bindings
                  (frame-conflict ,frame-graph
                    (register-conflict ,register-graph ,tail))))))]

        ; [(locals ,vars-list ,tail)
        ; (let ([register-graph (build-conflict-graph vars-list tail)])
        ;   `(locals ,vars-list (register-conflict ,register-graph ,tail)))]

        ; [(locals ,_ (ulocals ,_ (locate . _))) body]
        [(locate ,a ,b) body]

        [,else (error 'uncover-register-conflict "Invalid body structure" body)]))

    (define (build-conflict-graph vars tail)
      (let ([conflict-graph (map (lambda (v) (list v)) vars)])
        
        (define (live-in-triv triv)
          (if (or (uvar? triv) (register? triv)) (list triv) '()))

        (define (walk-pred pred live-set)
          (match pred
            [(true) live-set]
            [(false) live-set]
            [(,relop ,triv1 ,triv2) (union live-set (live-in-triv triv1) (live-in-triv triv2))]
            [(if ,p1 ,p2 ,p3) (let ([live-true (walk-pred p2 live-set)]) (let ([live-false (walk-pred p3 live-set)]) (walk-pred p1 (union live-true live-false))))]
            [(begin ,effects ... ,last-pred) (let ([live-before-pred (walk-pred last-pred live-set)]) (fold-right walk-effect live-before-pred effects))]
            [else-pred (error 'uncover-register-conflict "Invalid Predicate expression" else-pred)]))

        (define (walk-effect effect live-set)
          (define (add-conflict! graph var1 var2)
            (let loop ([g graph])
              (cond
                [(null? g) '()]
                [(eq? (caar g) var1)
                 (let ([conflicts (car g)])
                   (unless (member var2 (cdr conflicts))
                     (set-cdr! conflicts (cons var2 (cdr conflicts)))))
                 'done]
                [else (loop (cdr g))])))
          
          (define (add-oneway-conflicts! graph uvars reg)
            (for-each (lambda (u)
                        (when (uvar? u)
                          (add-conflict! graph u reg)))
                      uvars))

          (match effect
            [(nop) live-set]
            [(set! ,var (,binop ,triv1 ,triv2))
             (let* ([live-after (difference live-set (list var))]
                    [live-rhs (union (live-in-triv triv1) (live-in-triv triv2))]
                    [conflictees (union live-after live-rhs)])
               (cond
                 [(uvar? var)
                  (for-each
                   (lambda (c)
                     (cond
                       [(uvar? c) (add-conflict! conflict-graph var c) (add-conflict! conflict-graph c var)]
                       [(register? c) (add-conflict! conflict-graph var c)]))
                   conflictees)]
                 [(register? var) (add-oneway-conflicts! conflict-graph conflictees var)]
                 [else #t])
               (union live-after live-rhs))]
            [(set! ,var ,triv)
             (let* ([live-after (difference live-set (list var))]
                    [live-rhs (live-in-triv triv)]
                    [conflictees (difference live-after live-rhs)])
               (cond
                 [(uvar? var)
                  (for-each
                   (lambda (c)
                     (cond
                       [(uvar? c) (add-conflict! conflict-graph var c) (add-conflict! conflict-graph c var)]
                       [(register? c) (add-conflict! conflict-graph var c)]))
                   conflictees)]
                 [(register? var) (add-oneway-conflicts! conflict-graph conflictees var)]
                 [else #t])
               (union live-after live-rhs))]

            [(if ,pred ,eff1 ,eff2) (let* ([live1 (walk-effect eff1 live-set)][live2 (walk-effect eff2 live-set)]) (walk-pred pred (union live1 live2)))]
            [(begin . ,exprs)
             (if (null? exprs)
                 live-set
                 (let* ([final-effect (get-last exprs)]
                        [effects (all-but-last exprs)]
                        [live-before-final (walk-effect final-effect live-set)])
                   (fold-right walk-effect live-before-final effects)))]
            [else-effect (error 'uncover-register-conflict "Invalid Effect expression" else-effect)]))

        (define (all-but-last ls) (if (or (null? ls) (null? (cdr ls))) '() (cons (car ls) (all-but-last (cdr ls)))))
        (define (get-last ls) (if (null? ls) #f (if (null? (cdr ls)) (car ls) (get-last (cdr ls)))))

        (define (walk-tail t live-set)
           (match t
              [(if ,pred ,then-tail ,else-tail) 
                (let ([live-then (walk-tail then-tail live-set)]) 
                  (let ([live-else (walk-tail else-tail live-set)]) 
                      (walk-pred pred (union live-then live-else))))]

              [(begin . ,exprs) (if (null? exprs) live-set (let* ([final-tail (get-last exprs)][effects (all-but-last exprs)][live-before-tail (walk-tail final-tail live-set)]) (fold-right walk-effect live-before-tail effects)))]
              [(,trv ,live-locs ...) (union (live-in-triv trv) `(,live-locs ...))]
              [else-tail (error 'uncover-register-conflict "Invalid Tail expression" else-tail)]))
        
        (walk-tail tail '())
        
        conflict-graph))

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