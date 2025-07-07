(define uncover-frame-conflict
  (lambda (program)

    (define (process-body body)
      (match body
        [(locals ,vars-list ,tail)
         (let ([graph (build-conflict-graph vars-list tail)])
           `(locals ,vars-list (frame-conflict ,graph ,tail)))]
        [,else body]))

    (define (build-conflict-graph vars tail)
      (let ([conflict-graph (map (lambda (v) (list v)) vars)])
        

        (define (live-in-triv triv)
          (if (or (uvar? triv) (frame-var? triv) (register? triv))
              (list triv)
              '()))

        (define (add-conflict! var1 var2)
            (let loop ([g conflict-graph])
              (cond
                [(null? g) '()]
                [(eq? (caar g) var1)
                 (let ([conflicts (car g)])
                   (unless (member var2 (cdr conflicts))
                     (set-cdr! conflicts (cons var2 (cdr conflicts)))))]
                [else (loop (cdr g))])))
        
        (define (add-conflicts! var live-set)
            (when (uvar? var)
                (for-each
                    (lambda (live-item)
                        (when (not (eq? var live-item))
                            (cond
                                ;; uvar-uvar 冲突
                                [(uvar? live-item)
                                 (add-conflict! var live-item)
                                 (add-conflict! live-item var)]
                                ;; uvar-fvar 和 uvar-register 冲突
                                [(or (frame-var? live-item) (register? live-item))
                                 (add-conflict! var live-item)]
                                )))
                    live-set)))

        (define (walk-pred pred live-true live-false)
          (match pred
            [(true) live-true]
            [(false) live-false]
            [(,relop ,triv1 ,triv2) (union (live-in-triv triv1) (live-in-triv triv2) live-true live-false)]
            [(if ,p1 ,p2 ,p3)
             (let ([live-p2 (walk-pred p2 live-true live-false)]
                   [live-p3 (walk-pred p3 live-true live-false)])
               (walk-pred p1 live-p2 live-p3))]
            [(begin ,effects ... ,last-pred)
             (fold-right walk-effect (walk-pred last-pred live-true live-false) effects)]
            [else (error 'uncover-frame-conflict "Invalid Predicate" pred)]))

        (define (walk-effect effect live-set)
          (match effect
            [(nop) live-set]
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
               (add-conflicts! var (difference live-after live-rhs)) ; move-related
               new-live)]
            [(if ,pred ,eff1 ,eff2)
             (let* ([live1 (walk-effect eff1 live-set)]
                    [live2 (walk-effect eff2 live-set)])
               (walk-pred pred live1 live2))]
            [(begin ,effects ... ,last-effect)
             (fold-right walk-effect (walk-effect last-effect live-set) effects)]
            [else (error 'uncover-frame-conflict "Invalid Effect" effect)]))

        (define (walk-tail t)
          (match t
             [(if ,pred ,then-tail ,else-tail)
              (walk-pred pred (walk-tail then-tail) (walk-tail else-tail))]
             [(begin ,effects ... ,last-tail)
              (fold-right walk-effect (walk-tail last-tail) effects)]
             [(,triv . ,live-locs)
              (union (live-in-triv triv) live-locs)]
             [else (error 'uncover-frame-conflict "Invalid Tail" t)]))
        
        (walk-tail tail)
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
      [else program])))