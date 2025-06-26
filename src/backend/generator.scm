(define generate-x86-64
  (lambda (program)
    (define gen-binop
      (lambda (binop)
        (match binop
          [+ 'addq]
          [- 'subq]
          [* 'imulq])))

    (define gen-statement
        (lambda (stmt)
            (match stmt
                [(set! ,var1 (,[gen-binop -> binop] ,var1 ,var2)) (emit binop var2 var1)]
                [(set! ,var1 ,var2) (emit 'movq var2 var1)])))

    (match program
      [(begin ,stmts* ...) (emit-program (for-each gen-statement stmts*))])))