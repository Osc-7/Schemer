(define (specify-representation p)
  ;;; car/cdr 的内存偏移量
  (define offset-car (- disp-car tag-pair))
  (define offset-cdr (- disp-cdr tag-pair))
  ;;; 向量长度和数据区的内存偏移量
  (define offset-vector-length (- disp-vector-length tag-vector))
  (define offset-vector-data (- disp-vector-data tag-vector))

  (define offset-procedure-code (- disp-procedure-code tag-procedure))
  (define offset-procedure-data (- disp-procedure-data tag-procedure))

  ;;; 处理产生值的表达式 <Value>
  (define (Value v)
    (match v
      ;; 将 quote 的立即数转换为它们对应的 ptr 整数表示
      [(quote ,imm)
       (cond
         [(fixnum? imm) (fxsll imm shift-fixnum)] ; 整数左移 shift-fixnum 位
         [(eq? imm #t) $true]                     ; #t  -> $true
         [(eq? imm #f) $false]                   ; #f  -> $false
         [(null? imm) $nil]                      ; '() -> $nil
         [else (error 'specify-representation "unrecognized immediate" imm)])]

      [(+ ,v1 ,v2) `(+ ,(Value v1) ,(Value v2))]
      [(- ,v1 ,v2) `(- ,(Value v1) ,(Value v2))]

       ;; 乘法特殊处理
      [(* ,v1 ,v2)
       (let ([val1 (Value v1)] [val2 (Value v2)])
         (if (integer? val1)
             ;; 如果操作数是常量，在编译时进行移位
             `(* ,(fxsra val1 shift-fixnum) ,val2)
             `(* ,val1 (sra ,val2 ,shift-fixnum))))]

      ;; cons 转换为内存分配和初始化
      [(cons ,v1 ,v2)
       (let ([tmp-car (unique-name 'tmp-car)] [tmp-cdr (unique-name 'tmp-cdr)] [tmp (unique-name 'tmp)])
         `(let ([,tmp-car ,(Value v1)] [,tmp-cdr ,(Value v2)])
            (let ([,tmp (+ (alloc ,size-pair) ,tag-pair)])
              (begin
                (mset! ,tmp ,offset-car ,tmp-car)
                (mset! ,tmp ,offset-cdr ,tmp-cdr)
                ,tmp))))]

      ;; car/cdr 转换为 mref
      [(car ,v1) `(mref ,(Value v1) ,offset-car)]
      [(cdr ,v1) `(mref ,(Value v1) ,offset-cdr)]

      [(void) $void]

      ;; 向量操作
      [(make-vector ,v1)
       (let ([tmp-size (unique-name 'size)] [tmp-vec (unique-name 'vec)])
         (let ([val1 (Value v1)])
           (if (integer? val1)
               ;; 长度是常量
               (let ([size-in-bytes (+ disp-vector-data val1)])
                 `(let ([,tmp-vec (+ (alloc ,size-in-bytes) ,tag-vector)])
                    (begin
                      (mset! ,tmp-vec ,offset-vector-length ,val1)
                      ,tmp-vec)))
               ;; 长度是变量
               `(let ([,tmp-size ,val1])
                  (let ([,tmp-vec (+ (alloc (+ ,disp-vector-data ,tmp-size)) ,tag-vector)])
                    (begin
                      (mset! ,tmp-vec ,offset-vector-length ,tmp-size)
                      ,tmp-vec))))))]
      [(vector-length ,v1) `(mref ,(Value v1) ,offset-vector-length)]
      [(vector-ref ,v1 ,v2)
       (let ([val1 (Value v1)] [val2 (Value v2)])
         (if (integer? val2)
             ;; 索引是常量，编译时计算最终偏移
             `(mref ,val1 ,(+ offset-vector-data val2))
             `(mref ,val1 (+ ,offset-vector-data ,val2))))]
      
      ;; 匹配 (make-procedure label (quote n))
      [(make-procedure ,label (quote ,n))
            (let ([proc-size (* (+ n 1) 8)]) ; 1 个字用于存代码指针, n 个字用于存自由变量
              (let ([proc-ptr (unique-name 'proc)])
                `(let ([,proc-ptr (alloc ,proc-size)])
                    (begin
                      (mset! ,proc-ptr ,disp-procedure-code ,label)
                      ;; 返回带标签的指针
                      (logor ,proc-ptr ,tag-procedure)))))]

      ;; 实现 (procedure-ref proc (quote n))
      [(procedure-ref ,proc (quote ,n))
      (let ([final-offset (+ offset-procedure-data (* n 8))])
        `(mref ,(Value proc) ,final-offset))]

      ;; 实现 (procedure-code proc)
      [(procedure-code ,proc)
      `(mref ,(Value proc) ,offset-procedure-code)]


      [(if ,p ,v1 ,v2) `(if ,(Pred p) ,(Value v1) ,(Value v2))]
      [(begin ,effects ... ,val)
       (let ([new-effects (map Effect effects)]
             [new-val (Value val)])
         (make-begin `(,@new-effects ,new-val)))]
      [(let ([,uvars ,values] ...) ,body)
       `(let ,(map (lambda (u v) `[,u ,(Value v)]) uvars values)
          ,(Value body))]

      [(,f ,args ...) `(,(Value f) ,@(map Value args))]

      ;; 基本情况
      [,v v]))

  ;;; 处理产生断言的表达式 <Pred>
  (define (Pred p)
    (match p
      [(true) `(true)]
      [(false) `(false)]

      ;; 关系操作符直接作用于 ptr
      [(<= ,v1 ,v2) `(<= ,(Value v1) ,(Value v2))]
      [(< ,v1 ,v2) `(< ,(Value v1) ,(Value v2))]
      [(= ,v1 ,v2) `(= ,(Value v1) ,(Value v2))]
      [(>= ,v1 ,v2) `(>= ,(Value v1) ,(Value v2))]
      [(> ,v1 ,v2) `(> ,(Value v1) ,(Value v2))]

      ;; eq? 和 null? 转换为整数比较
      [(eq? ,v1 ,v2) `(= ,(Value v1) ,(Value v2))]
      [(null? ,v1) `(= ,(Value v1) ,$nil)]

      ;; 类型谓词转换为位运算和比较
      [(fixnum? ,v1) `(= (logand ,(Value v1) ,mask-fixnum) ,tag-fixnum)]
      [(boolean? ,v1) `(= (logand ,(Value v1) ,mask-boolean) ,tag-boolean)]
      [(pair? ,v1) `(= (logand ,(Value v1) ,mask-pair) ,tag-pair)]
      [(vector? ,v1) `(= (logand ,(Value v1) ,mask-vector) ,tag-vector)]
      [(procedure? ,v)
       `(= (logand ,(Value v) ,mask-procedure) ,tag-procedure)]

      [(if ,p1 ,p2 ,p3) `(if ,(Pred p1) ,(Pred p2) ,(Pred p3))]
      [(begin ,effects ... ,pred)
       (let ([new-effects (map Effect effects)]
             [new-pred (Pred pred)])
         (make-begin `(,@new-effects ,new-pred)))]
      [(let ([,uvars ,values] ...) ,body)
       `(let ,(map (lambda (u v) `[,u ,(Value v)]) uvars values)
          ,(Pred body))]

      [,else (error 'specify-representation "not a valid predicate form" p)]))

  ;;; 处理产生副作用的表达式 <Effect>
  (define (Effect e)
    (match e
      [(nop) `(nop)]

      ;; set! 操作转换为 mset!
      [(set-car! ,v1 ,v2) `(mset! ,(Value v1) ,offset-car ,(Value v2))]
      [(set-cdr! ,v1 ,v2) `(mset! ,(Value v1) ,offset-cdr ,(Value v2))]
      [(vector-set! ,v1 ,v2 ,v3)
       (let ([val1 (Value v1)] [val2 (Value v2)] [val3 (Value v3)])
         (if (integer? val2)
             ;; 索引是常量，编译时计算最终偏移
             `(mset! ,val1 ,(+ offset-vector-data val2) ,val3)
             `(mset! ,val1 (+ ,offset-vector-data ,val2) ,val3)))]

      [(procedure-set! ,proc (quote ,n) ,val)
      (let ([final-offset (+ offset-procedure-data (* n 8))])
        `(mset! ,(Value proc) ,final-offset ,(Value val)))]
                 
      ;; 递归处理复合结构
      [(if ,p ,e1 ,e2) `(if ,(Pred p) ,(Effect e1) ,(Effect e2))]
      [(begin ,effects ... ,effect)
       (make-begin (map Effect (append effects (list effect))))]
      [(let ([,uvars ,values] ...) ,body)
       `(let ,(map (lambda (u v) `[,u ,(Value v)]) uvars values)
          ,(Effect body))]
      [(,f ,args ...) `(,(Value f) ,@(map Value args))]
      [,else (error 'specify-representation "not a valid effect form" e)]))

  (match p
    [(letrec ([,labels (lambda (,formals ...) ,bodies)] ...) ,main-body)
     `(letrec
        ,(map (lambda (label formals body)
                `(,label (lambda ,formals ,(Value body))))
              labels formals bodies)
        ,(Value main-body))]
    [,else (error 'specify-representation "program must be a letrec" p)]))
