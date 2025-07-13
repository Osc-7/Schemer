(define assign-new-frame
  (lambda (program)
      (define relop?
      
          (lambda (op)
              (cond
                  [(eq? op '<) #t]
                  [(eq? op '<=) #t]
                  [(eq? op '>) #t]
                  [(eq? op '>=) #t]
                  [(eq? op '=) #t]
                  [(eq? op '!=) #t]
                  [else #f])))
      (define binop?
          (lambda (op)
              (match op
                  [+ #t]
                  [- #t]
                  [* #t]
                  [/ #t]
                  [logand #t]
                  [logor #t]
                  [sra #t]
                  [,x #f])))
    ;; This helper contains the core logic for the pass.
    (define (do-assignment-and-rewrite locals-vars frames-list locate-bindings graph call-live-vars tail)
      ;; --- 1. Calculate current function's frame size n ---
      (let* ([frame-size
              (let ([max-idx -1])
                (for-each
                 (lambda (v)
                   (let* ([loc (if (uvar? v)
                                   (let ([b (assq v locate-bindings)]) (if b (cadr b) #f))
                                   v)])
                     (when (frame-var? loc)
                       (set! max-idx (max max-idx (frame-var->index loc))))))
                 call-live-vars)
                (+ max-idx 1))]
             [frame-offset (* frame-size (expt 2 align-shift))])

        ;; --- 2. Assign frame locations to all nfvs ---
        (let* ([nfv-assigns
                (let ([all-nfvs (apply append frames-list)])
                  (if (null? all-nfvs)
                      '()
                      (let loop ([frames frames-list] [assignments '()])
                        (if (null? frames)
                            (reverse assignments)
                            (let* ([nfvs-in-frame (car frames)]
                                   [new-assigns (let map-nfvs ([nfvs nfvs-in-frame] [idx frame-size])
                                                  (if (null? nfvs)
                                                      '()
                                                      (cons `(,(car nfvs) ,(index->frame-var idx))
                                                            (map-nfvs (cdr nfvs) (+ idx 1)))))])
                              (loop (cdr frames) (append new-assigns assignments)))))))]
               [final-locate-bindings (append nfv-assigns locate-bindings)]
               [all-nfvs (apply append frames-list)])

          ;; --- 3. Recursively rewrite the tail, inserting fp adjustment instructions ---
          (let ([new-tail
                  (letrec ([rewrite
                            (lambda (exp)
                              (match exp
                                ;; --- 核心转换逻辑 ---
                                [(return-point ,label ,inner-tail)
                                `(begin
                                    (set! ,frame-pointer-register (+ ,frame-pointer-register ,frame-offset))
                                    (return-point ,label ,(rewrite inner-tail))
                                    (set! ,frame-pointer-register (- ,frame-pointer-register ,frame-offset)))]

                                ;; --- 递归遍历 ---
                                [(if ,p ,c ,a)
                                ;; 关键：对谓词 p 也要进行递归！
                                `(if ,(rewrite p) ,(rewrite c) ,(rewrite a))] 
                                [(begin ,exprs ...)
                                `(begin ,@(map rewrite exprs))]
                                
                                [(,op ,rand1 ,rand2) (guard (or (binop? op) (relop? op)))
                                `(,op ,(rewrite rand1) ,(rewrite rand2))]
                                
                                [(,rator ,rands ...) (guard (or (uvar? rator) (label? rator)))
                                `(,(rewrite rator) ,@(map rewrite rands))]
                                
                                ;; Base case: 对于不包含子表达式的节点，原样返回
                                [,else else]))])
                    (rewrite tail))])
            
            ;; --- 4. Assemble the final Body ---
            `(locals ,(difference locals-vars all-nfvs)
               (ulocals ()
                 (locate ,final-locate-bindings
                   (frame-conflict ,graph ,new-tail))))))))

    (define (process-body body)
      (match body
        ;; Case 1: Body has non-tail calls and a new-frames wrapper.
        [(locals ,l-vars (new-frames ,f-list (locate ,loc-b (frame-conflict ,g (call-live ,cl-v ,t)))))
         (do-assignment-and-rewrite l-vars f-list loc-b g cl-v t)]

        ;; Case 2: Body has NO non-tail calls, so no new-frames wrapper.
        [(locals ,l-vars (locate ,loc-b (frame-conflict ,g (call-live ,cl-v ,t))))
         (do-assignment-and-rewrite l-vars '() loc-b g cl-v t)]

        ;; If the body doesn't have the expected wrappers, do nothing.
        [,else body]))

    ;; Top-level traversal logic
    (match program
      [(letrec ,bindings ,main-body)
       (let ([new-main-body (process-body main-body)]
             [new-bindings (map (lambda (binding)
                                  (match binding
                                    [(,label (lambda () ,body))
                                     `(,label (lambda () ,(process-body body)))]))
                                bindings)])
         `(letrec ,new-bindings ,new-main-body))]
      [,else program])))