(define (convert-complex-datum p)
  (define collected-datums '())

  (define (datum->expr datum)
    (cond
      ;; 对于 cons 对，递归地使用 `cons` 来构建。
      [(pair? datum)
       `(cons ,(datum->expr (car datum)) ,(datum->expr (cdr datum)))]
      ;; 对于向量，生成一个 `let` 表达式来分配向量内存，
      ;; 然后使用一个 `begin` 块和一系列 `vector-set!` 来填充其内容。
      [(vector? datum)
       (let ([tmp (unique-name 'vec)]
             [len (vector-length datum)]
             [elements (vector->list datum)])
         ;; 修正 (1): 向量长度 `len` 需要被 quote
         `(let ([,tmp (make-vector ',len)])
            (begin
              ,@(let loop ([i 0] [elems elements])
                  (if (null? elems)
                      '()
                      ;; 修正 (2): 向量索引 `i` 需要被 quote
                      (cons `(vector-set! ,tmp ',i ,(datum->expr (car elems)))
                            (loop (+ i 1) (cdr elems)))))
              ,tmp)))]
      
      ;; 对于立即数（如数字、布尔值、空列表等），直接引用它们。
      [else `(quote ,datum)]))


  (define (walk e)
    (match e
      [(quote ,datum)
       (if (or (pair? datum) (vector? datum))
           (let ([tmp (unique-name 'c)]) 
             (set! collected-datums (cons (cons tmp datum) collected-datums))
             tmp)
           e)]

      [(if ,test ,conseq ,alt) 
       `(if ,(walk test) ,(walk conseq) ,(walk alt))]
      [(begin ,exprs ... ,last-expr) 
       `(begin ,@(map walk exprs) ,(walk last-expr))]
      [(lambda (,formals ...) ,body) 
       `(lambda (,@formals) ,(walk body))]
      [(let ([,vars ,inits] ...) ,body)
       `(let ,(map (lambda (v i) `[,v ,(walk i)]) vars inits)
          ,(walk body))]
      [(letrec ([,vars ,inits] ...) ,body)
       `(letrec ,(map (lambda (v i) `[,v ,(walk i)]) vars inits)
          ,(walk body))]
      [(set! ,var ,val) 
       `(set! ,var ,(walk val))]
      [(,op ,args ...) 
       `(,(walk op) ,@(map walk args))]

      [,atom e]))


  ;; 1. 遍历并重写整个程序，同时收集所有复杂的常量。
  (define rewritten-program (walk p))

  ;; 2. 如果收集到了复杂的常量，就用一个 `let` 将重写后的程序包裹起来。
  (if (null? collected-datums)
      ;; 如果没有找到任何复杂的 datum，直接返回重写后的程序。
      rewritten-program
      ;; 否则，构造一个 let 表达式来绑定所有收集到的常量。
      (let ([bindings (map (lambda (pair)
                             (let ([var (car pair)]
                                   [datum (cdr pair)])
                               `[,var ,(datum->expr datum)]))
                           ;; 反转列表，以确保 let 绑定的顺序与常量在代码中出现的顺序一致。
                           (reverse collected-datums))])
        `(let ,bindings
           ,rewritten-program))))