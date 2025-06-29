(define generate-x86-64
  (lambda (program)
    (define gen-binop
      (lambda (binop)
      (match binop
      [+ 'addq]
      [- 'subq]
      [* 'imulq]
      [logand 'andq]
      [logor 'orq]
      [sra 'sarq])))

    ;; 分发函数：处理单条指令或单个标签
    (define gen-instruction
      (lambda (instr)
        (match instr
          [,label (guard (label? label))
                  (emit-label label)]

          [(jump ,target)
           (emit-jump 'jmp target)]

          [(set! ,var1 (sra ,triv1 ,triv2))
           (let ([tmp 'r10])
             (emit 'movq triv1 tmp)
             (emit 'sarq triv2 tmp)
             (emit 'movq tmp var1))]

          [(set! ,var1 (,[gen-binop -> op] ,var2 ,triv))
           (guard (equal? var1 var2))
           (emit op triv var1)]

          [(set! ,var1 (,op ,triv1 ,triv2))
           (let ([tmp 'r10])
             (emit 'movq triv1 tmp)
             (emit (gen-binop op) triv2 tmp)
             (emit 'movq tmp var1))]

          [(set! ,var ,val)
           (cond
             [(and (register? var) (label? val))
              (emit 'leaq val var)]
             [else
              (emit 'movq val var)])]
           
          [,else
           (error 'generate-x86-64 "unrecognized instruction in 'code' block" else)])))

    ;; 主逻辑
    (match program
      [(code ,instrs ...)
       (emit-program
        (for-each gen-instruction instrs))]
        
      [,else
       (error 'generate-x86-64 "program input must be a 'code' form" else)])))