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

    (define gen-cond-jump
      (lambda (op negated?)
        (let ([op-map (if negated?
                          '((= . jne) (< . jge) (<= . jg) (> . jle) (>= . jl))
                          '((= . je) (< . jl) (<= . jle) (> . jg) (>= . jge)))])
          (let ([jump-instr (cdr (assq op op-map))])
            (if jump-instr
                jump-instr
                (error 'gen-cond-jump "Unsupported relational op" op))))))

    ;; 分发函数：处理单条指令或单个标签
    (define gen-instruction
      (lambda (instr)
        (match instr
          [(nop)
          (emit 'nop)]

          [,label (guard (label? label))
                  (emit-label label)]

          [(jump ,target)
           (emit-jump 'jmp target)]

          [(if (not (,op ,a ,b)) (jump ,target))
           (emit 'cmpq b a)
           (emit-jump (gen-cond-jump op #t) target)]

          [(if (,op ,a ,b) (jump ,target))
           (emit 'cmpq b a)
           (emit-jump (gen-cond-jump op #f) target)]

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