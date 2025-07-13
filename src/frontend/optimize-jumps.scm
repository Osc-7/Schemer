(define (optimize-jumps p)
  ;; 辅助函数：判断一个函数体是否为纯跳转
  (define (is-jump? body)
    (match body
      [(,label) (label? label)]
      [,else #f]))

  ;; 辅助函数：解析跳转链，找到最终目标
  (define (resolve-target start-label jump-map)
    (let loop ([current-label start-label]
               [visited (list start-label)])
      (let ([next-target (assq current-label jump-map)])
        (if (and next-target (not (memq (cadr next-target) visited)))
            ;; 如果有下一个目标，并且没有形成环，继续寻找
            (loop (cadr next-target) (cons (cadr next-target) visited))
            ;; 否则，当前就是最终目标
            current-label))))
  (define (remove-duplicates ls equal-proc)
    ;; This helper checks if an item is a member of a list using the provided
    ;; comparison procedure.
    (define (is-member? item lst)
      (cond
        [(null? lst) #f]
        [(equal-proc item (car lst)) #t]
        [else (is-member? item (cdr lst))]))

    (letrec ([loop (lambda (in out)
                    (if (null? in)
                        (reverse out)
                        (let ([item (car in)])
                          ;; Use the new helper instead of memq
                          (if (is-member? item out)
                              (loop (cdr in) out)
                              (loop (cdr in) (cons item out))))))])
      (loop ls '())))
  (match p
    [(letrec ,bindings ,body)
     (let-values ([(complex-bindings initial-jump-map)
                   (let loop ([b* bindings] [complex '()] [jumps '()])
                     (if (null? b*)
                         (values (reverse complex) (reverse jumps))
                         (let* ([current (car b*)]
                                [label (car current)]
                                [lambda-expr (cadr current)]
                                [lambda-body (caddr lambda-expr)])
                           (if (is-jump? lambda-body)
                               (loop (cdr b*)
                                     complex
                                     (cons `(,label ,(car lambda-body)) jumps))
                               (loop (cdr b*)
                                     (cons current complex)
                                     jumps)))))])
       (let* ([resolved-jump-map
               (map (lambda (entry)
                      `(,(car entry) ,(resolve-target (cadr entry) initial-jump-map)))
                    initial-jump-map)]
              [final-jump-map (remove-duplicates resolved-jump-map equal?)])

         (define (rewrite-body b)
           (match b
             ;; 原子或非跳转调用，直接返回
             [,x (guard (not (pair? x))) x]
             [(,rator ,rands ...)
              (if (null? rands)
                  ;; 这是一个跳转
                  (let ([new-target (assq rator final-jump-map)])
                    (if new-target
                        `(,(cadr new-target)) 
                        b)) ; 保持不变
                  ;; 非跳转，递归处理参数
                  `(,rator ,@(map rewrite-body rands)))]
             [(if ,tst ,thn ,els)
              `(if ,(rewrite-body tst) ,(rewrite-body thn) ,(rewrite-body els))]
             [(begin ,exprs ...)
              `(begin ,@(map rewrite-body exprs))]
             [(set! ,var ,val)
              `(set! ,var ,(rewrite-body val))]
             [,else b]))
         
         (let ([rewritten-bindings
                (map (lambda (binding)
                       (match binding
                         [(,label (lambda () ,lambda-body))
                          `(,label (lambda () ,(rewrite-body lambda-body)))]
                         [else binding])) ; 处理带参数的 lambda（如果有的话）
                     complex-bindings)]
               [rewritten-main-body (rewrite-body body)])
           `(letrec ,rewritten-bindings ,rewritten-main-body))))]
    [,else p]))