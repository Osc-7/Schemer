(define (lift-letrec p)
  (let-values ([(new-p bindings) (lift p)])
    `(letrec ,bindings ,new-p)))

(define (lift-list es)
  (if (null? es)
      (values '() '())
      (let-values ([(lifted-car bindings-from-car) (lift (car es))])
        (let-values ([(lifted-cdr bindings-from-cdr) (lift-list (cdr es))])
          (values (cons lifted-car lifted-cdr)
                  (append bindings-from-car bindings-from-cdr))))))

(define (lift e)
  ;; Recursively traverses an expression 'e', separating it into a transformed
  ;; expression and a list of lifted bindings.
  (match e
    ;; Base cases: atoms and quotes have no inner expressions or bindings.
    [,x (guard (not (pair? x)))
     (values e '())]
    [(quote ,_)
     (values e '())]

    [(letrec ([,labels (lambda ,formals ,bodies)] ...) ,body-expr)
     (let-values ([(lifted-body bindings-from-body) (lift body-expr)])
       (let-values ([(lifted-bodies bindings-from-bodies) (lift-list bodies)])
         (let ([current-bindings (map (lambda (label formal-list body)
                                        `(,label (lambda ,formal-list ,body)))
                                      labels formals lifted-bodies)])
           (values lifted-body
                   (append current-bindings bindings-from-body bindings-from-bodies)))))]

    [(let ([,vars ,rhs-exprs] ...) ,body-expr)
     (let-values ([(lifted-rhs-exprs bindings-from-rhs) (lift-list rhs-exprs)])
       (let-values ([(lifted-body bindings-from-body) (lift body-expr)])
         (values `(let ,(map list vars lifted-rhs-exprs) ,lifted-body)
                 (append bindings-from-rhs bindings-from-body))))]

    [(if ,test-expr ,then-expr ,else-expr)
     (let-values ([(lifted-test bindings-from-test) (lift test-expr)])
       (let-values ([(lifted-then bindings-from-then) (lift then-expr)])
         (let-values ([(lifted-else bindings-from-else) (lift else-expr)])
           (values `(if ,lifted-test ,lifted-then ,lifted-else)
                   (append bindings-from-test bindings-from-then bindings-from-else)))))]
    
    [(begin ,expr1 ,exprs ...)
      (let-values ([(lifted-expr-list all-bindings) (lift-list (cons expr1 exprs))])
        (values (make-begin lifted-expr-list) all-bindings))]

    [(,rator ,rands ...)
     (let-values ([(lifted-rator bindings-from-rator) (lift rator)])
       (let-values ([(lifted-rands bindings-from-rands) (lift-list rands)])
         (values `(,lifted-rator ,@lifted-rands)
                 (append bindings-from-rator bindings-from-rands))))]

    [,else (error 'lift "Unknown expression form --" e)]))