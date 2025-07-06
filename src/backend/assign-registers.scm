(define assign-registers
  (lambda (program)
    (define (process-body body)
      ;; --- Helper functions ---
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
            [(null? regs) #f]
            [(memq (car regs) forbidden-regs) (loop (cdr regs))]
            [else (car regs)])))

      (define (remove-node-from-graph node graph)
        (if (not node)
            graph
            (let ([graph-without-node (filter (lambda (entry) (not (eq? (car entry) node))) graph)])
              (map (lambda (entry)
                    (cons (car entry) (remove node (cdr entry))))
                  graph-without-node))))
      
      (define (color-graph all-nodes graph unspillable-nodes allocatable-registers)
        (let ([k (length allocatable-registers)])
          (letrec (
            ;; 修改点: 确保不会选择 unspillable-nodes 作为溢出候选
            [find-node-to-color (lambda (nodes current-graph)
              (let loop ([nodes-to-check nodes] [spill-candidate #f])
                (if (null? nodes-to-check)
                    spill-candidate
                    (let* ([current-node (car nodes-to-check)]
                           [degree (let ([e (assq current-node current-graph)]) (if e (length (cdr e)) 0))])
                      (cond
                        ;; 优先选择低度数节点
                        [(< degree k) current-node]
                        ;; 如果是高都数节点，只在它不是"不可溢出"时，才考虑作为候选
                        [(not (memq current-node unspillable-nodes))
                         (loop (cdr nodes-to-check) (or spill-candidate current-node))]
                        ;; 如果是不可溢出变量，则跳过
                        [else (loop (cdr nodes-to-check) spill-candidate)])))))]

            [simplify (lambda (nodes-to-simplify current-graph)
              (if (null? nodes-to-simplify)
                  (values '() '())
                  (let* ([picked-node (find-node-to-color nodes-to-simplify current-graph)]
                         ;; 如果 picked-node 为 #f，说明所有高都数节点都是不可溢出的，这是个错误
                         [_ (when (not picked-node)
                              (error 'assign-registers "Cannot find a node to color or spill. Unspillable constraints not met." unspillable-nodes))]
                         [remaining-nodes (remove picked-node nodes-to-simplify)]
                         [simplified-graph (remove-node-from-graph picked-node current-graph)])
                    
                    (let-values ([(assignments-for-rest spills-for-rest) 
                                  (simplify remaining-nodes simplified-graph)])
                      
                      (let* ([neighbors (get-neighbors picked-node graph)]
                             [neighbor-colors (get-neighbor-colors neighbors assignments-for-rest)]
                             [pre-forbidden-colors (get-pre-forbidden-colors neighbors)]
                             [used-colors (map cdr assignments-for-rest)]
                             [all-forbidden-colors (union used-colors neighbor-colors pre-forbidden-colors)]
                             [chosen-color (find-available-color allocatable-registers all-forbidden-colors)])
                        
                        (if chosen-color
                            (values (cons (cons picked-node chosen-color) assignments-for-rest)
                                    spills-for-rest)
                            ;; 如果找不到颜色，并且是不可溢出变量，则报错
                            (if (memq picked-node unspillable-nodes)
                                (error 'assign-registers "Failed to assign a register to an unspillable variable" picked-node)
                                (values assignments-for-rest
                                        (cons picked-node spills-for-rest)))))))))])
          
          (simplify all-nodes graph))))      
      
      (match body
        

        [(locals ,locals-vars
           (ulocals ,ulocals-vars
             (locate ,locate-bindings
               (frame-conflict ,frame-graph
                 (register-conflict ,register-graph ,tail)))))
         
         (let ([all-vars (union locals-vars ulocals-vars)])
           (if (null? all-vars)
               `(locate ,locate-bindings ,tail)
               (let ([allocatable-registers (difference registers (list frame-pointer-register return-address-register))])
                 (let-values ([(assignments spills) 
                               (color-graph all-vars register-graph ulocals-vars allocatable-registers)])
                   
                   (if (null? spills)
                       (let ([formatted-assignments 
                              (map (lambda (p) `(,(car p) ,(cdr p))) assignments)])
                         `(locate ,(append locate-bindings formatted-assignments) ,tail))
                       
                       `(locals ,(difference locals-vars spills)
                          (ulocals ,ulocals-vars
                            (spills ,spills
                              (locate ,locate-bindings
                                (frame-conflict ,frame-graph ,tail))))))))))]
        [(locate ,binding ,bind) body]
        [,else (error 'assign-registers "Invalid body structure for assign-registers" body)]))

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