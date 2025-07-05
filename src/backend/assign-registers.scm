(define assign-registers
  (lambda (program)

    (define (process-body body)
         ;; --- 辅助函数定义 ---
         (define (get-neighbors node graph)
           (let ([entry (assq node graph)])
             (if entry (cdr entry) '())))

         (define (get-neighbor-colors neighbors assignments)
           (if (null? neighbors)
               '()
               (let ([assignment (assq (car neighbors) assignments)])
                 (if assignment
                     (cons (cdr assignment) (get-neighbor-colors (cdr neighbors) assignments))
                     (get-neighbor-colors (cdr neighbors) assignments)))))

         (define (get-pre-forbidden-colors neighbors)
           (filter register? neighbors))
         
         (define (find-available-color all-regs forbidden-regs)
           (let loop ([regs all-regs])
             (cond
               [(null? regs) #f] ; 没有可用的了
               [(memq (car regs) forbidden-regs) (loop (cdr regs))]
               [else (car regs)])))

        (define (find-node-to-color nodes graph k)
          (if (null? nodes)
              #f ; 如果没有节点，返回 #f
              (letrec ([get-degree (lambda (node current-graph)
                                    (let ([entry (assq node current-graph)])
                                      (if entry (length (cdr entry)) 0)))]
                       [loop (lambda (nodes-to-check spill-candidate)
                               (if (null? nodes-to-check)
                                   spill-candidate
                                   (let* ([current-node (car nodes-to-check)]
                                          [degree (get-degree current-node graph)])
                                     (if (< degree k)
                                         current-node
                                         (loop (cdr nodes-to-check) spill-candidate)))))])
                (loop nodes (car nodes)))))

        (define (remove-node-from-graph node graph)
          (if (not node)
              graph
              (let ([graph-without-node (filter (lambda (entry) (not (eq? (car entry) node))) graph)])
                (map (lambda (entry)
                       (cons (car entry) (remove node (cdr entry))))
                     graph-without-node))))
        
         ;; --- 核心图着色递归函数 ---
        (define (color-graph nodes current-graph)
          (if (null? nodes)
              '()
              (let* ([k (length registers)]
                    [picked-node (find-node-to-color nodes current-graph k)]
                    [remaining-nodes (remove picked-node nodes)]
                    [simplified-graph (remove-node-from-graph picked-node current-graph)]
                    [assignments-for-rest (color-graph remaining-nodes simplified-graph)])
            
                (if (not picked-node)
                    '() ; 如果没有节点被选中，说明已经处理完了
                    (let* ([neighbors (get-neighbors picked-node current-graph)]
                          [neighbor-colors (get-neighbor-colors neighbors assignments-for-rest)]
                          [pre-forbidden-colors (get-pre-forbidden-colors neighbors)]
                          [all-forbidden-colors (union neighbor-colors pre-forbidden-colors)]
                          [chosen-color (find-available-color registers all-forbidden-colors)])
                      
                      (if chosen-color
                          (cons (cons picked-node chosen-color) assignments-for-rest)
                          ;; 如果没有可用的颜色（寄存器），则抛出错误
                          (error 'assign-registers 
                                "Cannot assign a register for variable. Register spilling is required, but not yet implemented." 
                                picked-node)))))))
      (match body
        [(locals ,vars (register-conflict ,conflict-graph ,tail)) 
         (if (null? vars)
             `(locate () ,tail) ; 如果没有变量，直接返回空的 locate
             (let ([assignments (color-graph vars conflict-graph)])
               (if (= (length assignments) (length vars))
                   `(locate ,(map (lambda (p) `(,(car p) ,(cdr p))) assignments) ,tail)
                   (error 'assign-registers "Failed to assign registers for all variables." (list 'vars vars 'assignments assignments)))))]
        
        [,else (error 'process-body "Invalid body for assign-registers" else)]))

    (match program
      [(letrec ,bindings ,main-body)
       (let ([new-main-body (process-body main-body)]
             [new-bindings (map (lambda (binding)
                                  (match binding
                                    [(,label (lambda () ,body))
                                     `(,label (lambda () ,(process-body body)))]
                                    [,else-binding (error 'assign-registers "Invalid binding structure" else-binding)]))
                                bindings)])
         `(letrec ,new-bindings ,new-main-body))]
      [,else (error 'assign-registers "Invalid program structure" else)])))