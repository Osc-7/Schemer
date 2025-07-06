(define uncover-frame-conflict
  (lambda (program)

    (define (process-body body)
      (match body
        [(locals ,vars-list ,tail)
         (let ([graph (build-conflict-graph vars-list tail)])
           `(locals ,vars-list (frame-conflict ,graph ,tail)))]
        [,else body]))

    (define (build-conflict-graph vars tail)
      ;; 初始化冲突图，每个变量一个空冲突列表
      (let ([conflict-graph (map (lambda (v) (list v)) vars)])
        
        ;; 辅助函数：判断一个 triv 是否是需要帧冲突分析的位置 (uvar 或 fvar)
        (define (is-frame-location? loc)
          (or (uvar? loc) (frame-var? loc)))

        ;; 辅助函数：从 triv 中提取出需要分析的 frame location
        (define (live-in-triv triv)
          (if (is-frame-location? triv)
              (list triv)
              '()))

        ;; 辅助函数：向图中添加冲突
        (define (add-conflict! var1 var2)
          (let ([entry1 (assq var1 conflict-graph)]
                [entry2 (assq var2 conflict-graph)])
            (when (and entry1 (not (memq var2 (cdr entry1))))
              (set-cdr! entry1 (cons var2 (cdr entry1))))
            (when (and entry2 (not (memq var1 (cdr entry2))))
              (set-cdr! entry2 (cons var1 (cdr entry2))))))

        (define (add-conflicts! var live-set)
            (when (uvar? var)
                (for-each
                    (lambda (live-item)
                        (when (and (is-frame-location? live-item) (not (eq? var live-item)))
                           (add-conflict! var live-item)))
                    live-set)))

        ;; 逆向分析 Predicate
        (define (walk-pred pred live-true live-false)
          (match pred
            [(true) live-true]
            [(false) live-false]
            [(,relop ,triv1 ,triv2) 
             (union live-true live-false (live-in-triv triv1) (live-in-triv triv2))]
            [(if ,p1 ,p2 ,p3)
             (let ([live-p2 (walk-pred p2 live-true live-false)]
                   [live-p3 (walk-pred p3 live-true live-false)])
               (walk-pred p1 live-p2 live-p3))]
            [(begin ,effects ... ,last-pred)
             (fold-right walk-effect (walk-pred last-pred live-true live-false) effects)]
            [else (error 'uncover-frame-conflict "Invalid Predicate" pred)]))

        ;; 逆向分析 Effect
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
                    [live-rhs (live-in-triv triv)])
               ;; 对于 move 指令 (set! x y)，只有当 y 不是 x 时，x 和 y 之后的活跃变量才冲突
               (add-conflicts! var (if (eq? var triv) 
                                       live-after 
                                       (difference live-after live-rhs)))
               (union live-after live-rhs))]
            [(if ,pred ,eff1 ,eff2)
             (let* ([live1 (walk-effect eff1 live-set)]
                    [live2 (walk-effect eff2 live-set)])
               (walk-pred pred live1 live2))]
            [(begin ,effects ... ,last-effect)
             (fold-right walk-effect (walk-effect last-effect live-set) effects)]
            [else (error 'uncover-frame-conflict "Invalid Effect" effect)]))

        ;; 逆向分析 Tail
        (define (walk-tail t)
          (match t
             [(if ,pred ,then-tail ,else-tail)
              (walk-pred pred (walk-tail then-tail) (walk-tail else-tail))]
             [(begin ,effects ... ,last-tail)
              (fold-right walk-effect (walk-tail last-tail) effects)]
             [(,triv . ,live-locs)
              ;; 关键修正: 只从 live-locs 中过滤出 uvar 和 fvar
              (let ([frame-live-locs (filter is-frame-location? live-locs)])
                (union (live-in-triv triv) frame-live-locs))]
             [else (error 'uncover-frame-conflict "Invalid Tail" t)]))
        
        ;; 开始分析
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