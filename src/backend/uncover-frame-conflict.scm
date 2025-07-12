(define uncover-frame-conflict
  (lambda (program)

    (define (process-body body)
      (match body
        [(locals ,vars-list (new-frames ,frames-list ,tail))
        (let-values ([(graph call-lives) (build-conflict-graph vars-list tail)])
          (let ([spills (filter uvar? call-lives)])
            `(locals ,vars-list
                (new-frames ,frames-list
                  (spills ,spills
                    (frame-conflict ,graph
                      (call-live ,call-lives ,tail)))))))]
        [,else (error 'uncover-frame-conflict "body structure invalid" body)]))

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
                                [(uvar? live-item)
                                 (add-conflict! var live-item)
                                 (add-conflict! live-item var)]
                                [(or (frame-var? live-item) (register? live-item))
                                 (add-conflict! var live-item)]
                                )))
                    live-set)))

        (define (walk-pred pred live-true live-false cl-true cl-false)
          (match pred
            [(true) (values live-true cl-true)]
            [(false) (values live-false cl-false)]

            [(if ,p1 ,p2 ,p3)
             (let-values ([(live-p2 cl-p2) (walk-pred p2 live-true live-false cl-true cl-false)])
               (let-values ([(live-p3 cl-p3) (walk-pred p3 live-true live-false cl-true cl-false)])
                 (let-values ([(live-p1 cl-p1) (walk-pred p1 live-p2 live-p3 cl-p2 cl-p3)])
                   (values live-p1 cl-p1))))]

            [(begin ,effects ... ,last-pred)
              (let-values ([(live-after-effects cl-after-effects) 
                            (walk-pred last-pred live-true live-false cl-true cl-false)])
                (let loop ([fx effects] [live-in live-after-effects] [cl-in cl-after-effects])
                  (if (null? fx)
                      (values live-in cl-in)
                      (let-values ([(live-out cl-out) (loop (cdr fx) live-in cl-in)])
                        (walk-effect (car fx) live-out cl-out)))))]
            [(,relop ,triv1 ,triv2) 
             (values (union (live-in-triv triv1) (live-in-triv triv2) live-true live-false)
                     (union cl-true cl-false))]
            [else (error 'uncover-frame-conflict "Invalid Predicate" pred)]))

        (define (walk-effect effect live-set call-lives)
          (match effect
            [(nop) (values live-set call-lives)]
            [(return-point ,label ,tail)
             (let-values ([(live-before-tail cl-tail) (walk-tail tail)])
               (for-each
                 (lambda (call-var)
                   (add-conflicts! call-var live-set))
                 live-before-tail)
               (let ([call-live-vars (filter (lambda (v) (or (uvar? v) (frame-var? v))) live-set)])
                 (values (union live-set live-before-tail)
                         (union call-lives call-live-vars cl-tail))))]

            [(set! ,var (alloc ,size))
             (let* ([live-after (difference live-set (list var))]
                    [live-rhs (live-in-triv size)]
                    [new-live (union live-after live-rhs)])
               (add-conflicts! var live-after)
               (values new-live call-lives))]

            ;; A8 修改: 处理 (set! var (mref base offset))
            [(set! ,var (mref ,base ,offset))
             (let* ([live-after (difference live-set (list var))]
                    [live-rhs (union (live-in-triv base) (live-in-triv offset))]
                    [new-live (union live-after live-rhs)])
               (add-conflicts! var live-after)
               (values new-live call-lives))]

            ;; A8 修改: 处理 (mset! base offset val) 作为一个 Effect
            [(mset! ,base ,offset ,val)
             (let* ([live-rhs (union (live-in-triv base) (live-in-triv offset) (live-in-triv val))]
                   [new-live (union live-set live-rhs)])
               ;; mset! 不定义新变量，所以只更新活性集，不添加冲突
               (values new-live call-lives))]
               
            [(set! ,var (,binop ,triv1 ,triv2))
             (let* ([live-after (difference live-set (list var))]
                    [live-rhs (union (live-in-triv triv1) (live-in-triv triv2))]
                    [new-live (union live-after live-rhs)])
               (add-conflicts! var live-after)
               (for-each (lambda (rhs-var) (add-conflicts! rhs-var live-after)) live-rhs)
               (values new-live call-lives))]
            [(set! ,var ,triv)
             (let* ([live-after-inst (difference live-set (list var))]
                    [live-rhs (live-in-triv triv)]
                    [live-at-def-point (union live-after-inst live-rhs)])
               (if (and (uvar? var) (equal? triv var))
                   (add-conflicts! var live-after-inst)
                   (if (and (uvar? var) (uvar? triv) (equal? live-rhs (list triv)))
                       (add-conflicts! var (difference live-at-def-point (list triv)))
                       (add-conflicts! var live-at-def-point)))
               (for-each (lambda (rhs-var) (add-conflicts! rhs-var live-after-inst)) live-rhs)
               (values live-at-def-point call-lives))]
            [(if ,pred ,eff1 ,eff2)
             (let-values ([(live1 cl1) (walk-effect eff1 live-set call-lives)])
               (let-values ([(live2 cl2) (walk-effect eff2 live-set call-lives)])
                 (let-values ([(live-p cl-p) (walk-pred pred live1 live2 cl1 cl2)])
                   (values live-p cl-p))))]

            [(begin ,effects ... ,last-effect)
            (let-values ([(live-after-effects cl-after-effects) (walk-effect last-effect live-set call-lives)])
              (let loop ([fx effects] [live-in live-after-effects] [cl-in cl-after-effects])
                (if (null? fx)
                    (values live-in cl-in)
                    (let-values ([(live-out cl-out) (loop (cdr fx) live-in cl-in)])
                      (walk-effect (car fx) live-out cl-out)))))]
            [else (error 'uncover-frame-conflict "Invalid Effect" effect)]))

        (define (walk-tail t)
          (match t
             [(if ,pred ,then-tail ,else-tail)
              (let-values ([(live-then cl-then) (walk-tail then-tail)])
                (let-values ([(live-else cl-else) (walk-tail else-tail)])
                  (let-values ([(live-p cl-p) (walk-pred pred live-then live-else cl-then cl-else)])
                    (values live-p cl-p))))]

              [(begin ,effects ... ,last-tail)
                (let-values ([(live-after-effects cl-after-effects) (walk-tail last-tail)])
                  (let loop ([fx effects] [live-in live-after-effects] [cl-in cl-after-effects])
                    (if (null? fx)
                        (values live-in cl-in)
                        (let-values ([(live-out cl-out) (loop (cdr fx) live-in cl-in)])
                          (walk-effect (car fx) live-out cl-out)))))]
                          
             [(alloc ,size)
              (values (live-in-triv size) '())]
             [(mref ,base ,offset)
              (values (union (live-in-triv base) (live-in-triv offset)) '())]

             [(,triv . ,live-locs)
              (values (union (live-in-triv triv) live-locs) '())]
             [else (error 'uncover-frame-conflict "Invalid Tail" t)]))
        
        (let-values ([(live-set call-lives) (walk-tail tail)])
          (values conflict-graph (remove-duplicates call-lives)))))

    (match program
      [(letrec ,bindings ,main-body)
       (let ([new-main-body (process-body main-body)]
             [new-bindings (map (lambda (binding)
                                  (match binding
                                    [(,label (lambda () ,body))
                                     `(,label (lambda () ,(process-body body)))]))
                                bindings)])
         `(letrec ,new-bindings ,new-main-body))]
      [,else (error 'uncover-frame-conflict "program must be a letrec" else)])))