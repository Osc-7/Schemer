(define discard-call-live
  (lambda (program)

    (define (all-but-last ls)
      (if (or (null? ls) (null? (cdr ls)))
          '()
          (cons (car ls) (all-but-last (cdr ls)))))

    (define (get-last ls)
      (if (null? ls)
          #f
          (if (null? (cdr ls))
              (car ls)
              (get-last (cdr ls)))))

    (define (walk-tail tail)
      (match tail
        [(if ,pred ,then-tail ,else-tail)
         `(if ,pred ,(walk-tail then-tail) ,(walk-tail else-tail))]
        
        [(begin . ,exprs)
         (if (null? exprs)
             '(begin)
             (let ([effects (all-but-last exprs)]
                   [final-tail (get-last exprs)])
               `(begin ,@effects ,(walk-tail final-tail))))]

        [(,triv . ,locs) (guard (not (null? locs)))
         `(,triv)]

        [,else else]))

    (define (process-body body)
      (match body
        [(locate ,bindings ,tail)
         `(locate ,bindings ,(walk-tail tail))]
        [,else else]))

    (match program
      [(letrec ,bindings ,main-body)
       (let ([new-main-body (process-body main-body)]
             [new-bindings (map (lambda (binding)
                                  (match binding
                                    [(,label (lambda () ,body))
                                     `(,label (lambda () ,(process-body body)))]
                                    [,else-binding (error 'discard-call-live "Invalid binding structure" else-binding)]))
                                bindings)])
         `(letrec ,new-bindings ,new-main-body))]
      [,else (error 'discard-call-live "Invalid program structure" else)])))