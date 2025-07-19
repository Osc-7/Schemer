(define (parse-scheme program)
  (let ([env (make-empty-environment)])
    ((Expr env) program)))

;; -----------------------------------------------------------------------------
;; Main Parser Expression Dispatcher
;; -----------------------------------------------------------------------------

;; Parses an expression `expr` in the context of `env`.
;; The environment `env` is a pair: (variable-map . handler-map)
;; - variable-map: alist mapping source variables to unique variables. `((var . uvar) ...)`
;; - handler-map: alist mapping keywords/primitives to handler procedures. `((name . proc) ...)`
(define-who (Expr env)
  (lambda (expr)
    (match expr
      ;; Keyword/Primitive Application: (proc arg ...)
      [(,proc ,e* ...) (guard (and (symbol? proc) (assoc proc (cdr env))))
        ((cdr (assoc proc (cdr env))) env expr)]

      ;; Procedure Application: (rator rand ...)
      [(,rator ,rand* ...)
        `(,((Expr env) rator) ,@(map (Expr env) rand*))]

      ;; Constants: #t, #f, (), 123
      [,constant (guard (or (boolean? constant) (null? constant) (fixnum? constant)))
        (handle_constant constant)]
      
      ;; Variable Reference
      [,var (guard (symbol? var))
        (let ([var-uvar-pair (assoc var (car env))])
          (if var-uvar-pair
              (cdr var-uvar-pair)
              (error who "Unbound variable ~s" var)))]
      
      [,x (error who "Invalid syntax ~s" x)])))

;; -----------------------------------------------------------------------------
;; Keyword and Primitive Handlers
;; -----------------------------------------------------------------------------

;; Handle `and` expressions.
(define-who (handle_and env expr)
  (match expr
    [(and ,e* ...)
      (let ([x* (map (Expr env) e*)])
        (if (null? x*)
            '(quote #t)
            (let f ([x* x*])
              (if (null? (cdr x*))
                  (car x*)
                  (let ([temp_var (unique-name 'and)])
                    `(let ([,temp_var ,(car x*)])
                       (if ,temp_var
                           ,(f (cdr x*))
                           ,temp_var)))))))]
    [,x (error who "Invalid and syntax: ~s" x)]))

;; Handle `or` expressions.
(define-who (handle_or env expr)
  (match expr
    [(or ,e* ...)
      (let ([x* (map (Expr env) e*)])
        (if (null? x*)
            '(quote #f)
            (let f ([x* x*])
              (if (null? (cdr x*))
                  (car x*)
                  (let ([new_var (unique-name 'or)])
                    `(let ([,new_var ,(car x*)])
                       (if ,new_var ,new_var ,(f (cdr x*)))))))))]
    [,x (error who "Invalid or syntax: ~s" x)]))

;; Handle `not` expressions.
(define-who (handle_not env expr)
  (match expr
    [(not ,e) `(if ,((Expr env) e) '#f '#t)]
    [,x (error who "Invalid not syntax: ~s" x)]))

;; Convert an unquoted constant to its quoted form.
(define-who (handle_constant constant)
  (match constant
    [#t '(quote #t)]
    [#f '(quote #f)]
    [() '(quote ())]
    [,n (guard (fixnum-range? n)) `(quote ,n)]
    [,x (error who "Invalid constant, possibly out of fixnum range: ~s" x)]))

;; Recursively validate a datum within a `quote`.
(define-who (handle_datum datum)
  (match datum
    [#(,sub_datum* ...)
      (for-each handle_datum sub_datum*)
      `(quote ,datum)]
    [(,pair_car . ,pair_cdr)
      (handle_datum pair_car)
      (handle_datum pair_cdr)
      `(quote ,datum)]
    [,atomic_datum (handle_constant atomic_datum)]))

;; Handle `quote` expressions.
(define-who (handle_quote env expr)
  (match expr
    [(quote ,datum) (handle_datum datum)]
    [,x (error who "Invalid quote syntax: ~s" x)]))

;; Handle `if` expressions (one- and two-armed).
(define-who (handle_if env expr)
  (match expr
    [(if ,pred ,conseq)
      `(if ,((Expr env) pred) ,((Expr env) conseq) (void))]
    [(if ,pred ,conseq ,alt)
      `(if ,((Expr env) pred) ,((Expr env) conseq) ,((Expr env) alt))]
    [,x (error who "Invalid if syntax: ~s" x)]))

;; Helper to wrap multiple body expressions in a `begin`.
(define (make-begin exprs)
  (if (= (length exprs) 1) (car exprs) `(begin ,@exprs)))

;; Handle `begin` expressions.
(define-who (handle_begin env expr)
  (match expr
    [(begin ,e* ... ,e)
      (make-begin (map (Expr env) `(,@e* ,e)))]
    [(begin) (error who "Empty begin expression is not allowed")]
    [,x (error who "Invalid begin syntax: ~s" x)]))

;; Handle `lambda` expressions.
(define-who (handle_lambda env expr)
  (match expr
    [(lambda ,var* ,body1 ,body* ...) (guard (set? var*))
      (let-values ([(uvars new_env) (make_new_env env var*)])
        `(lambda ,uvars ,((Expr new_env) (make-begin `(,body1 ,@body*)))))]
    [,x (error who "Invalid lambda syntax: ~s" x)]))

;; Handle `let` expressions.
(define-who (handle_let env expr)
  (match expr
    [(let ([,var* ,e*] ...) ,body1 ,body* ...) (guard (set? var*))
      (let ([parsed-e* (map (Expr env) e*)])
        (let-values ([(uvars new_env) (make_new_env env var*)])
          `(let ,(map list uvars parsed-e*)
             ,((Expr new_env) (make-begin `(,body1 ,@body*))))))]
    [,x (error who "Invalid let syntax: ~s" x)]))

;; Handle `letrec` expressions.
(define-who (handle_letrec env expr)
  (match expr
    [(letrec ([,var* ,e*] ...) ,body1 ,body* ...) (guard (set? var*))
      (let-values ([(uvars new_env) (make_new_env env var*)])
        `(letrec ,(map list uvars (map (Expr new_env) e*))
           ,((Expr new_env) (make-begin `(,body1 ,@body*)))))]
    [,x (error who "Invalid letrec syntax: ~s" x)]))

;; Handle `set!` expressions.
(define-who (handle_set! env expr)
  (match expr
    [(set! ,var ,e) (guard (symbol? var))
      (let ([var-uvar-pair (assoc var (car env))])
        (if var-uvar-pair
            `(set! ,(cdr var-uvar-pair) ,((Expr env) e))
            (error who "Cannot set! unbound variable: ~s" var)))]
    [,x (error who "Invalid set! syntax: ~s" x)]))

;; A generic handler for bound variables when they appear in operator position.
;; This is needed to correctly shadow keywords. e.g., (let ([if +]) (if 1 2))
(define-who (handle_var env expr)
  (match expr
    [(,var ,e* ...)
      (let ([uvar (cdr (assoc var (car env)))])
        `(,uvar ,@(map (Expr env) e*)))]
    [,x (error who "Invalid variable application syntax: ~s" x)]))

;; Factory for creating handlers for primitives with a fixed number of arguments.
(define-who (handle_prim op_num)
  (lambda (env expr)
    (match expr
      [(,prim ,e* ...)
        (if (= (length e*) op_num)
            `(,prim ,@(map (Expr env) e*))
            (error who "Incorrect number of arguments for ~s. Expected ~s, got ~s." prim op_num (length e*)))]
      [,x (error who "Invalid primitive syntax: ~s" x)])))

;; -----------------------------------------------------------------------------
;; Environment Management
;; -----------------------------------------------------------------------------

;; Creates the initial environment with all keywords and primitives.
(define-who (make-empty-environment)
  (define prim0 (handle_prim 0))
  (define prim1 (handle_prim 1))
  (define prim2 (handle_prim 2))
  (define prim3 (handle_prim 3))
  (cons
    '() ; Initial variable-map is empty
    `((and . ,handle_and)
      (or . ,handle_or)
      (not . ,handle_not)
      (quote . ,handle_quote)
      (if . ,handle_if)
      (begin . ,handle_begin)
      (lambda . ,handle_lambda)
      (let . ,handle_let)
      (letrec . ,handle_letrec)
      (set! . ,handle_set!)
      (void . ,prim0)
      (car . ,prim1)
      (cdr . ,prim1)
      (make-vector . ,prim1)
      (vector-length . ,prim1)
      (procedure-code . ,prim1)
      (boolean? . ,prim1)
      (fixnum? . ,prim1)
      (null? . ,prim1)
      (pair? . ,prim1)
      (vector? . ,prim1)
      (procedure? . ,prim1)
      (cons . ,prim2)
      (vector-ref . ,prim2)
      (make-procedure . ,prim2)
      (procedure-ref . ,prim2)
      (eq? . ,prim2)
      (set-car! . ,prim2)
      (set-cdr! . ,prim2)
      (+ . ,prim2)
      (- . ,prim2)
      (* . ,prim2)
      (= . ,prim2)
      (< . ,prim2)
      (> . ,prim2)
      (<= . ,prim2)
      (>= . ,prim2)
      (vector-set! . ,prim3)
      (procedure-set! . ,prim3))))

;; Extends an environment with new variable bindings.
;; Returns two values: a list of the new unique variables, and the new environment.
(define (make_new_env env var*)
  (let ([new_uvars (map (lambda (x) (unique-name x)) var*)])
    (values new_uvars
      (cons (append (map cons var* new_uvars) (car env))
            (append (map (lambda (x) (cons x handle_var)) var*) (cdr env))))))
