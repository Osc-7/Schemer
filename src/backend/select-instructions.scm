;; 文件: Schemer/src/backend/select-instructions.scm

(define select-instructions
  (lambda (program)

    (define (is-operand-register? op) (or (register? op) (uvar? op)))
    (define (is-operand-memory? op) (or (frame-var? op) (disp-opnd? op)))
    (define (is-operand-immediate? op) (integer? op))
    (define (is-commutative? op) (memq op '(+ * logand logor)))

    (define (map-walk walk-fn expr-list)
      (if (null? expr-list)
          (values '() '())
          (let-values ([(new-hd u1) (walk-fn (car expr-list))])
            (let-values ([(new-tl u2) (map-walk walk-fn (cdr expr-list))])
              (values (cons new-hd new-tl) (union u1 u2))))))

    (define (walk-tail tail)
      (match tail
        [(if ,p ,t ,e)
         (let-values ([(new-p u1) (walk-pred p)])
           (let-values ([(new-t u2) (walk-tail t)])
             (let-values ([(new-e u3) (walk-tail e)])
               (values `(if ,new-p ,new-t ,new-e) (union u1 u2 u3)))))]
        [(begin ,effects ... ,last)
         (let-values ([(new-effects u1) (map-walk walk-effect effects)])
           (let-values ([(new-last u2) (walk-tail last)])
             (values (make-begin (append new-effects (list new-last))) (union u1 u2))))]
        [,else (values else '())]))

    (define (walk-pred pred)
      (match pred
        [(if ,p1 ,p2 ,p3)
         (let-values ([(new-p1 u1) (walk-pred p1)])
           (let-values ([(new-p2 u2) (walk-pred p2)])
             (let-values ([(new-p3 u3) (walk-pred p3)])
               (values `(if ,new-p1 ,new-p2 ,new-p3) (union u1 u2 u3)))))]
        [(begin ,effects ... ,last)
         (let-values ([(new-effects u1) (map-walk walk-effect effects)])
           (let-values ([(new-last u2) (walk-pred last)])
             (values (make-begin (append new-effects (list new-last))) (union u1 u2))))]
        [(,relop ,triv1 ,triv2)
         (cond
           [(and (is-operand-memory? triv1) (is-operand-memory? triv2))
            (let ([tmp (unique-name 't)])
              (values `(begin (set! ,tmp ,triv1) (,relop ,tmp ,triv2)) (list tmp)))]
           [(and (is-operand-immediate? triv1) (not (is-operand-immediate? triv2)))
            (let ([new-relop (case relop [(<) '>] [(<=) '>=] [(>) '<] [(>=) '<=] [(=) '=])])
              (if (eq? new-relop '=) (values pred '()) (values `(,new-relop ,triv2 ,triv1) '())))]
           [else (values pred '())])]
        [,else (values pred '())]))

    (define (walk-effect effect)
      (match effect
        [(nop) (values effect '())]
        [(if ,p ,t ,e)
         (let-values ([(new-p u1) (walk-pred p)])
           (let-values ([(new-t u2) (walk-effect t)])
             (let-values ([(new-e u3) (walk-effect e)])
               (values `(if ,new-p ,new-t ,new-e) (union u1 u2 u3)))))]
        [(begin ,effects ...)
         (let-values ([(new-effects u) (map-walk walk-effect effects)])
           (values (make-begin new-effects) u))]

        [(set! ,var (,binop ,triv1 ,triv2))
         (cond
           ;; --- 优先级1：处理内存到内存操作，这是最严格的限制 ---
           [(and (is-operand-memory? triv1) (is-operand-memory? triv2))
            (let ([tmp (unique-name 't)])
              ;; 转换为 mem = reg + mem 的形式，这是最高效的 mem-mem 实现
              (values (make-begin `((set! ,tmp ,triv1) (set! ,var (,binop ,tmp ,triv2))))
                      (list tmp)))]
                      
           ;; --- 优先级2：处理其他目标是内存地址的情况 ---
           [(is-operand-memory? var)
            (cond
              ;; 情况A: 已经是合法的 mem-op 形式, e.g., (set! fv0 (+ fv0 rax))
              [(and (equal? var triv1) (or (is-operand-register? triv2) (is-operand-immediate? triv2)))
               (values effect '())]
              ;; 情况B: 可通过交换律变成合法形式, e.g., (set! fv0 (+ rax fv0))
              [(and (is-commutative? binop) (equal? var triv2) (is-operand-register? triv1))
               (walk-effect `(set! ,var (,binop ,triv2 ,triv1)))]
              ;; 情况C: 优化！使用目标内存作为临时存储，避免新uvar, e.g., (set! fv0 (+ rax t.4))
              [(is-operand-register? triv1)
               (values (make-begin `((set! ,var ,triv1) (set! ,var (,binop ,var ,triv2)))) '())]
              ;; 情况D: 其他情况，必须使用新uvar
              [else
               (let ([tmp (unique-name 't)])
                 (values (make-begin `((set! ,tmp ,triv1) (set! ,tmp (,binop ,tmp ,triv2)) (set! ,var ,tmp)))
                         (list tmp)))]
            )]

           ;; --- 优先级3：处理目标是寄存器/uvar 的情况 (逻辑同上次修复) ---
           [(equal? var triv1)
            (values effect '())]
           [(and (is-commutative? binop) (equal? var triv2))
            (walk-effect `(set! ,var (,binop ,triv2 ,triv1)))]
           [(and (is-commutative? binop) (is-operand-immediate? triv1))
            (walk-effect `(set! ,var (,binop ,triv2 ,triv1)))]
           [(not (equal? var triv2))
            (values (make-begin `((set! ,var ,triv1) (set! ,var (,binop ,var ,triv2)))) '())]
           [else
            (let ([tmp (unique-name 't)])
              (values (make-begin `((set! ,tmp ,triv1) (set! ,var (,binop ,tmp ,triv2))))
                      (list tmp)))]
         )]

        [(set! ,var ,triv)
         (cond
           [(and (is-operand-memory? var) (is-operand-memory? triv))
            (let ([tmp (unique-name 't)])
              (values (make-begin `((set! ,tmp ,triv) (set! ,var ,tmp))) (list tmp)))]
           [(and (is-operand-memory? var) (label? triv))
            (let ([tmp (unique-name 't)])
              (values (make-begin `((set! ,tmp ,triv) (set! ,var ,tmp))) (list tmp)))]
           [(and (not (is-operand-register? var)) (is-operand-immediate? triv) (not (int32? triv)))
            (let ([tmp (unique-name 't)])
              (values (make-begin `((set! ,tmp ,triv) (set! ,var ,tmp))) (list tmp)))]
           [else (values effect '())])]
        [,else (values else '())]))

    (define (process-body body)
      (match body
        [(locals ,l-vars (ulocals ,u-vars (locate ,loc-binds (frame-conflict ,graph ,tail))))
         (let-values ([(new-tail new-uvars) (walk-tail tail)])
           `(locals ,l-vars (ulocals ,(union u-vars new-uvars) (locate ,loc-binds (frame-conflict ,graph ,new-tail)))))]
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