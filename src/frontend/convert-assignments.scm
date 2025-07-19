(define (convert-assignments program)
  ;; Partitions a list into two lists based on a predicate.
  (define (partition pred lst)
    (let loop ([lst lst] [pass '()] [fail '()])
      (if (null? lst)
          (list (reverse pass) (reverse fail))
          (if (pred (car lst))
              (loop (cdr lst) (cons (car lst) pass) fail)
              (loop (cdr lst) pass (cons (car lst) fail))))))

  ;; The main conversion logic is handled by a recursive procedure 'Pass'.
  ;; It takes a list of all assigned variables currently in scope.
  (define (Pass assigned-in-scope)
    (lambda (expr)
      (cond
        ;; Case 1: Expression is a symbol (a variable reference).
        ;; This is the most direct way to handle variable transformations.
        [(symbol? expr)
         (if (memq expr assigned-in-scope)
             `(car ,expr) ; If assigned, wrap in `car`.
             expr)]      ; Otherwise, it's a regular variable.

        ;; Case 2: Expression is not a pair (e.g., number, #t, '()). It's a literal.
        [(not (pair? expr))
         expr]

        ;; Case 3: Expression is a list, use `match` to deconstruct.
        [else
         (match expr
           [(quote ,_) expr]

           ;; A set! must be for an assigned variable.
           [(set! ,uvar ,val)
            `(set-car! ,uvar ,((Pass assigned-in-scope) val))]

           ;; For a 'let' expression, transform its assigned bindings.
           [(let ([,vars ,rhss] ...) (assigned ,assigned-vars ,body))
            (let* ([bindings (map list vars rhss)]
                   [new-scope (append assigned-vars assigned-in-scope)]
                   [p (partition (lambda (b) (memq (car b) assigned-vars)) bindings)]
                   [asgn-bindings (car p)]
                   [un-bindings (cadr p)]
                   ;; Generate temporary variables for the initial values of assigned variables.
                   [tmp-bindings (map (lambda (b)
                                        (list (unique-name 't) (cadr b)))
                                      asgn-bindings)]
                   ;; Create the cell allocation bindings, e.g., (x (cons t.1 (void))).
                   [cell-bindings (map (lambda (orig-b tmp-b)
                                         (list (car orig-b) `(cons ,(car tmp-b) (void))))
                                       asgn-bindings
                                       tmp-bindings)])
              `(let (,@(map (lambda (b) `(,(car b) ,((Pass assigned-in-scope) (cadr b)))) un-bindings)
                     ,@(map (lambda (b) `(,(car b) ,((Pass assigned-in-scope) (cadr b)))) tmp-bindings))
                 (let (,@cell-bindings)
                   ,((Pass new-scope) body))))]

           ;; For a 'lambda' expression, transform its assigned parameters.
           [(lambda ,params (assigned ,assigned-vars ,body))
            (let* ([new-scope (append assigned-vars assigned-in-scope)]
                   [p (partition (lambda (param) (memq param assigned-vars)) params)]
                   [asgn-params (car p)]
                   [un-params (cadr p)]
                   ;; Generate temporary parameters for the assigned ones.
                   [tmp-params (map (lambda (p) (unique-name 't)) asgn-params)]
                   ;; Create cell allocation bindings.
                   [cell-bindings (map (lambda (orig-p tmp-p)
                                         `(,orig-p (cons ,tmp-p (void))))
                                       asgn-params
                                       tmp-params)])
              `(lambda (,@un-params ,@tmp-params)
                 (let (,@cell-bindings)
                   ,((Pass new-scope) body))))]

           ;; Recursive cases for other forms
           [(if ,tst ,thn ,els)
            `(if ,((Pass assigned-in-scope) tst)
                 ,((Pass assigned-in-scope) thn)
                 ,((Pass assigned-in-scope) els))]
           [(begin ,exprs ...)
            `(begin ,@(map (Pass assigned-in-scope) exprs))]
           [(letrec ([,vars ,rhss] ...) ,body)
            `(letrec ,(map (lambda (v r) `[,v ,((Pass assigned-in-scope) r)]) vars rhss)
               ,((Pass assigned-in-scope) body))]

           ;; A procedure call.
           [(,proc ,args ...)
            `(,((Pass assigned-in-scope) proc) ,@(map (Pass assigned-in-scope) args))]

           ;; Fallback for any other expression structure.
           [,else else])])))
           
  ;; Start the conversion process with an empty scope of assigned variables.
  ((Pass '()) program))
