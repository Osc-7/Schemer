(define assign-registers
  (lambda (program)

    (define (process-body body)
      (match body
        [(locate ,bindings ,tail)
         body]

        [(locals ,local-vars
           (ulocals ,ulocal-vars
             (locate ,frame-bindings
               (frame-conflict ,frame-graph
                 (register-conflict ,register-graph ,tail)))))
        (let* ([spillable-vars local-vars]
              [unspillable-vars ulocal-vars]
              ;; 1. Get the list of variables already in the frame.
              [frame-vars (map car frame-bindings)]
              [all-vars (union spillable-vars unspillable-vars)]
              ;; 2. Compute the set of variables that truly need coloring.
              [vars-to-color (difference all-vars frame-vars)]
              ;; 3. Pass this corrected list to the coloring function.
              (coloring-result (color-graph vars-to-color register-graph unspillable-vars))
              (assignments (car coloring-result))
              (spills (cdr coloring-result)))
          (if (null? spills)
              ;; Success! Now the append will not have duplicates.
              `(locate ,(append (map (lambda (p) `(,(car p) ,(cdr p))) assignments) frame-bindings) ,tail)
               ;; Failure. Some variables must be spilled.
               `(locals ,(difference spillable-vars spills)
                  (ulocals ,ulocal-vars
                    (spills ,spills
                      (locate ,frame-bindings
                        (frame-conflict ,frame-graph ,tail)))))))]

        ;; Fallback for A4-style input structure.
        [(locals ,vars (register-conflict ,conflict-graph ,tail))
         (let* ([coloring-result (color-graph vars conflict-graph '())] ; No unspillables in A4
                (assignments (car coloring-result))
                (spills (cdr coloring-result)))
          ;  (if (null? spills)
          ;      `(locate ,(map (lambda (p) `(,(car p) ,(cdr p))) assignments) ,tail)
          ;      (error 'assign-registers "Spilling required but not handled in this path (A4 compatibility). Spills:" spills)))]
            (if (null? spills)
               ;; Success! All variables assigned to registers.
               `(locate ,(map (lambda (p) `(,(car p) ,(cdr p))) assignments) ,tail)
               ;; Failure. Some variables must be spilled.
               `(locals ,(difference spillable-vars spills)
                  (ulocals ,ulocal-vars
                    (spills ,spills
                      (locate ,frame-bindings
                        (frame-conflict ,frame-graph ,tail)))))))]

        [,else (error 'assign-registers "Invalid body structure for assign-registers" else)]))

    (define (color-graph nodes graph unspillable-nodes)
      (define (get-neighbors node g)
        (let ([entry (assq node g)])
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

      (define (find-node-to-color nodes-to-color current-graph k)
        (let ([spillable-nodes (difference nodes-to-color unspillable-nodes)])
          (letrec ([get-degree (lambda (node) (length (get-neighbors node current-graph)))]
                   [find-low-degree (lambda (n-list)
                                      (if (null? n-list)
                                          #f
                                          (let ([node (car n-list)])
                                            (if (< (get-degree node) k)
                                                node
                                                (find-low-degree (cdr n-list))))))])
            ;; 1. Try to find any low-degree node.
            (or (find-low-degree nodes-to-color)
                ;; 2. If not found, try to find a high-degree SPILLABLE node.
                (if (null? spillable-nodes)
                    ;; 3. If no spillable nodes left, try to find a high-degree UNSPILLABLE node.
                    ;;    This might work if a neighbor gets spilled later.
                    (if (null? unspillable-nodes)
                        #f ; No nodes left at all
                        (car unspillable-nodes))
                    (car spillable-nodes))))))

      (define (remove-node-from-graph node graph-to-mod)
        (if (not node)
            graph-to-mod
            (let ([graph-without-node (filter (lambda (entry) (not (eq? (car entry) node))) graph-to-mod)])
              (map (lambda (entry)
                     (cons (car entry) (remove node (cdr entry))))
                   graph-without-node))))

      (letrec ([do-color (lambda (nodes-to-color current-graph)
                           (if (null? nodes-to-color)
                               (cons '() '()) ; (assignments . spills)
                               (let* ([k (length registers)]
                                      [picked-node (find-node-to-color nodes-to-color current-graph k)]
                                      [remaining-nodes (remove picked-node nodes-to-color)]
                                      [simplified-graph (remove-node-from-graph picked-node current-graph)]
                                      (recursive-result (do-color remaining-nodes simplified-graph))
                                      (assignments-for-rest (car recursive-result))
                                      (spills-from-rest (cdr recursive-result)))
                                 (if (not picked-node)
                                     (cons '() '())
                                     (let* ([neighbors (get-neighbors picked-node graph)]
                                            [neighbor-colors (get-neighbor-colors neighbors assignments-for-rest)]
                                            [pre-forbidden-colors (get-pre-forbidden-colors neighbors)]
                                            [all-forbidden-colors (union neighbor-colors pre-forbidden-colors)]
                                            [chosen-color (find-available-color registers all-forbidden-colors)])
                                       (if chosen-color
                                           (cons (cons (cons picked-node chosen-color) assignments-for-rest)
                                                 spills-from-rest)
                                           ;; Spilling logic
                                           (if (memq picked-node unspillable-nodes)
                                               (error 'assign-registers 
                                                      "Could not assign a register to an unspillable variable:" 
                                                      picked-node)
                                               (cons assignments-for-rest
                                                     (cons picked-node spills-from-rest)))))))))])
        (do-color nodes graph)))

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