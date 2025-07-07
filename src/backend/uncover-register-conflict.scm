(define uncover-register-conflict
  (lambda (program)
    (define (backward body)
      (define (process-body vars tail)
        (let ([conflict-graph (map (lambda (v) (list v)) vars)])

          (define (live-in-triv triv)
            (if (or (uvar? triv) (register? triv)) (list triv) '()))
          (define (walk-pred pred live-true live-false)
            (match pred

              [(begin ,effects ... ,last-pred)
              (let ([live-before-pred (walk-pred last-pred live-true live-false)])
                ; (display ">>> walk-pred begin live-before-pred: ") ; (display live-before-pred) ; (newline)
                (fold-right (lambda (e acc)
                              ; (display ">>> walk-effect in begin: ") ; (display e) ; (newline)
                              (walk-effect e acc))
                            live-before-pred
                            effects))]
              [(if ,p1 ,p2 ,p3)
              (let ([new-live-true (walk-pred p2 live-true live-false)]
                    [new-live-false (walk-pred p3 live-true live-false)])
                (walk-pred p1 new-live-true new-live-false))]

              [(,relop ,triv1 ,triv2)
              (let ([live-set (union live-true live-false)])
                (union live-set (live-in-triv triv1) (live-in-triv triv2)))]

              [(true) live-true]
              [(false) live-false]

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
              ; (display ">>> Analyzing (set! ") ; (display var) ; (display " = ") ; (display binop)
              ; (display " ...)") ; (newline)
              ; (display "    Incoming live-set: ") ; (display live-set) ; (newline)
              (let* ([live-after (difference live-set (list var))]
                      [live-rhs (union (live-in-triv triv1) (live-in-triv triv2))]

                      ;; --- 修复开始 ---
                      ;; 冲突变量集合应为 live-after 和 live-rhs 的并集，
                      ;; 同时需要排除 var 自身。
                      [conflictees (union live-after (difference live-rhs (list var)))])
                      ;; --- 修复结束 ---
                (cond
                  [(uvar? var)
                    (for-each
                      (lambda (c)
                        (cond
                          ;; 修正：只为 uvar 添加双向冲突
                          [(uvar? c) 
                          (add-conflict! conflict-graph var c) 
                          (add-conflict! conflict-graph c var)]
                          ;; 修正：为 register 添加单向冲突
                          [(register? c) 
                          (add-conflict! conflict-graph var c)]))
                      conflictees)]
                  [(register? var)
                    (add-oneway-conflicts! conflict-graph conflictees var)])
                (union live-after live-rhs))]


              [(set! ,var ,triv)
               ; (display ">>> Analyzing (set! ") ; (display var) ; (display " ...)") ; (newline)
               ; (display "    Incoming live-set: ") ; (display live-set) ; (newline)
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
                   [(register? var)
                    (add-oneway-conflicts! conflict-graph conflictees var)])
                 ;; 正确返回新的活性集合
                 (union live-after live-rhs))]

              [(if ,pred ,eff1 ,eff2)
               (let* ([live1 (walk-effect eff1 live-set)]
                      [live2 (walk-effect eff2 live-set)]
                      [live-pred (walk-pred pred live1 live2)])

                   ; (display ">>> Live set before IF: ") ; (display live-pred) ; (newline)

                 live-pred)]

              ;; in walk-effect
              [(begin . ,exprs)
              (if (null? exprs)
                  live-set
                  (let* ([final-effect (get-last exprs)]
                          [effects (all-but-last exprs)]
                          [live-before-final (walk-effect final-effect live-set)])
                    (fold-right (lambda (e acc) (walk-effect e acc)) live-before-final effects)))]
              [else-effect (error 'uncover-register-conflict "Invalid Effect expression" else-effect)]))

          (define (all-but-last ls) (if (or (null? ls) (null? (cdr ls))) '() (cons (car ls) (all-but-last (cdr ls)))))
          (define (get-last ls) (if (null? ls) #f (if (null? (cdr ls)) (car ls) (get-last (cdr ls)))))

          (define (walk-tail t live-set)
            (match t
              [(if ,pred ,then-tail ,else-tail)
               (let ([live-then (walk-tail then-tail live-set)]
                     [live-else (walk-tail else-tail live-set)])
                 (walk-pred pred live-then live-else))]

              [(begin . ,exprs)
               (if (null? exprs) live-set
                   (let* ([final-tail (get-last exprs)]
                          [effects (all-but-last exprs)]
                          [live-before-tail (walk-tail final-tail live-set)])
                     (fold-right (lambda (e acc) (walk-effect e acc)) live-before-tail effects)))]

              ;; 这个分支是正确的，保持不变
              [(,trv ,live-locs ...)
               (let ([initial-live-set (if (list? `(,live-locs ...)) `(,live-locs ...) (list `(,live-locs ...)))])
                 (union (live-in-triv trv) initial-live-set))]

              [else-tail (error 'uncover-register-conflict "Invalid Tail expression" else-tail)]))

          (walk-tail tail '())
          `(locals ,vars (register-conflict ,conflict-graph ,tail))))

      (match body
        [(locals ,vars-list ,tail)
         (process-body vars-list tail)]
        [else-body (error 'uncover-register-conflict "Program body must be a 'locals' form" else-body)]))

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