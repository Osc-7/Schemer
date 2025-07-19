(define (optimize-known-call p)
  ;; known-map 是一个关联列表 '((var . label) ...)，用于跟踪已知过程
  (define (walk expr known-map)
    (match expr

      [,x (guard (or (symbol? x) (immediate? x)(uvar? x) (boolean? x))) x]
      [(quote ,d) expr]

      [(letrec ,letrec-bindings (closures ,closures-bindings ,closures-body))
       (let* (
              [new-knowns (map (lambda (b) (cons (car b) (cadr b))) closures-bindings)]
              [extended-map (append new-knowns known-map)])

         (let (
               [new-letrec-bindings
                (map (lambda (binding)
                       (let ([label (car binding)]
                             [lambda-exp (cadr binding)])
                         (match lambda-exp
                           [(lambda ,params ,body)
                            `(,label (lambda ,params ,(walk body extended-map)))]
                           [else
                            (error 'optimize-known-call
                                   "letrec requires a lambda" lambda-exp)])))
                     letrec-bindings)])

           `(letrec ,new-letrec-bindings
              (closures ,closures-bindings
                        ,(walk closures-body extended-map)))))]

      [(if ,c ,t ,f) `(if ,(walk c known-map) ,(walk t known-map) ,(walk f known-map))]
      [(let ,b ,body) `(let ,b ,(walk body known-map))] ; let 的绑定值不会是过程，无需处理
      [(begin ,e1 ,e* ...) `(begin ,@(map (lambda (e) (walk e known-map)) (cons e1 e*)))]
      [(bind-free ,p ,body) `(bind-free ,p ,(walk body known-map))]

      ;;处理函数调用
      [(,rator ,rands ...) (guard (symbol? rator))
       (let ([found (assoc rator known-map)])
         (if found
             ;; 情况A: 已知调用。用标签替换变量。
             (let ([label (cdr found)])
               `(,label ,@(map (lambda (r) (walk r known-map)) rands)))
             ;; 情况B: 未知调用。正常递归。
             `(,rator ,@(map (lambda (r) (walk r known-map)) rands))))]
      
      ;; 处理操作符不是符号的调用 (例如 ((lambda ...) 1))
      [(,rator ,rands ...)
       `(,(walk rator known-map) ,@(map (lambda (r) (walk r known-map)) rands))]

      [,else expr]))

  (walk p '()))