(define uncover-frame-conflict
  (lambda (program)

    (define (process-body body)
      (match body
        ;; Case 1: Body has non-tail calls and a new-frames wrapper.
        [(locals ,vars-list (new-frames ,frames-list ,tail))
        (let-values ([(graph call-lives) (build-conflict-graph vars-list tail)])
          (let ([spills (filter uvar? call-lives)])
            `(locals ,vars-list
                (new-frames ,frames-list
                  (spills ,spills
                    (frame-conflict ,graph
                      (call-live ,call-lives ,tail)))))))]

        ;; Case 2: Body has NO non-tail calls, so no new-frames wrapper.
        [(locals ,vars-list ,tail)
        (let-values ([(graph call-lives) (build-conflict-graph vars-list tail)])
          ;; In this case, call-lives will be empty.
          (let ([spills (filter uvar? call-lives)])
            `(locals ,vars-list
                (spills ,spills
                  (frame-conflict ,graph
                    (call-live ,call-lives ,tail))))))]

        [,else (error 'uncover-frame-conflict "body structure invalid" body)]))

    ;; A7: 修正，不再传递可变集合，而是返回收集到的 call-lives
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

        ;; A7: walk-* 函数现在返回 (values live-set collected-call-lives)
        (define (walk-pred pred live-true live-false cl-true cl-false)
          (match pred
            [(true) (values live-true cl-true)]
            [(false) (values live-false cl-false)]
            [(,relop ,triv1 ,triv2) 
             (values (union (live-in-triv triv1) (live-in-triv triv2) live-true live-false)
                     (union cl-true cl-false))]
            [(if ,p1 ,p2 ,p3)
             (let-values ([(live-p2 cl-p2) (walk-pred p2 live-true live-false cl-true cl-false)])
               (let-values ([(live-p3 cl-p3) (walk-pred p3 live-true live-false cl-true cl-false)])
                 (walk-pred p1 live-p2 live-p3 cl-p2 cl-p3)))]
            [(begin ,effects ... ,last-pred)
              (let-values ([(live-after-effects cl-after-effects) 
                            (walk-pred last-pred live-true live-false cl-true cl-false)])
                (let loop ([fx effects] [live-in live-after-effects] [cl-in cl-after-effects])
                  (if (null? fx)
                      (values live-in cl-in)
                      (let-values ([(live-out cl-out) (loop (cdr fx) live-in cl-in)])
                        (walk-effect (car fx) live-out cl-out)))))]
            [else (error 'uncover-frame-conflict "Invalid Predicate" pred)]))

        ;; A7: walk-effect 现在返回 (values live-set collected-call-lives)
        (define (walk-effect effect live-set call-lives)
          (match effect
            [(nop) (values live-set call-lives)]

            [(return-point ,label ,tail)
             (let-values ([(live-before-tail cl-tail) (walk-tail tail)])
               (values (union live-set live-before-tail)
                       (union call-lives live-set cl-tail)))] ;; 将 live-set (即 call-live) 加入收集列表

            [(set! ,var (,binop ,triv1 ,triv2))
             (let* ([live-after (difference live-set (list var))]
                    [live-rhs (union (live-in-triv triv1) (live-in-triv triv2))]
                    [new-live (union live-after live-rhs)])
               (add-conflicts! var live-after)
               (values new-live call-lives))]

            [(set! ,var ,triv)
             (let* ([live-after (difference live-set (list var))]
                    [live-rhs (live-in-triv triv)]
                    [new-live (union live-after live-rhs)])
               (add-conflicts! var (difference live-after live-rhs)) ; move-related
               (values new-live call-lives))]
               
            [(if ,pred ,eff1 ,eff2)
             (let-values ([(live1 cl1) (walk-effect eff1 live-set call-lives)])
               (let-values ([(live2 cl2) (walk-effect eff2 live-set call-lives)])
                 (walk-pred pred live1 live2 cl1 cl2)))]
            [(begin ,effects ... ,last-effect)
             (let-values ([(final-live final-cl) (walk-effect last-effect live-set call-lives)])
               (fold-right
                (lambda (effect acc)
                  (match-let* ([(values live cl) acc])
                    (walk-effect effect live cl)))
                (values final-live final-cl)
                effects))]
            [else (error 'uncover-frame-conflict "Invalid Effect" effect)]))

        ;; A7: walk-tail 现在返回 (values live-set collected-call-lives)
        (define (walk-tail t)
          (match t
             [(if ,pred ,then-tail ,else-tail)
              (let-values ([(live-then cl-then) (walk-tail then-tail)])
                (let-values ([(live-else cl-else) (walk-tail else-tail)])
                  (walk-pred pred live-then live-else cl-then cl-else)))]
              [(begin ,effects ... ,last-tail)
                (let-values ([(live-after-effects cl-after-effects) (walk-tail last-tail)])
                  (let loop ([fx effects] [live-in live-after-effects] [cl-in cl-after-effects])
                    (if (null? fx)
                        (values live-in cl-in)
                        (let-values ([(live-out cl-out) (loop (cdr fx) live-in cl-in)])
                          (walk-effect (car fx) live-out cl-out)))))]
             [(,triv . ,live-locs)
              (values (union (live-in-triv triv) live-locs) '())]
             [else (error 'uncover-frame-conflict "Invalid Tail" t)]))
        
        ;; 启动分析
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