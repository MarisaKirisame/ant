;; Environment = list of (int . value) pairs
((define null? (xs)
    (null xs)
)

((define eq? (a b)
    (eq a b))

((define symbol? (a)
    (symbol a))

((define pair? (a)
    (pair a))

((define atom? (a)
    (atom a))

((define else () (quote 0))

((define caar (xs) (car (car xs)))

((define cdar (xs) (cdr (car xs)))

((define cddar (xs) (cdr (cdr (car xs))))

((define cadar (xs) (car (cdr (car xs))))

((define caddar (xs) (car (cdr (cdr (car xs)))))

((define caddr (xs) (car (cdr (cdr xs))))

((define cadr (xs) (car (cdr xs)))

((define lookup (x env)
  (cond ((null? env) (error -1))
        ((eq? x (caar env)) (cdar env))
        ((quote 0) (lookup x (cdr env)))))

((define pairlis (xs ys env)
  (if (null? xs)
      env
      (cons (cons (car xs) (car ys))
            (pairlis (cdr xs) (cdr ys) env))))

((define isvar (x) (and (pair? x) (eq? (car x) 'var))) ;; symbol `var` is specially supported, and has no runtime behavior

((define evlis (exps env)
  (if (null? exps)
      '()
      (cons (eval* (car exps) env)
            (evlis (cdr exps) env))))

((define evcon (clauses env)
  (cond ((null? clauses) (error -2))
        ((eval* (caar clauses) env)
         (eval* (cadar clauses) env))
        (else
         (evcon (cdr clauses) env))))

((define eval* (exp env)
  (cond
    ;; bare number
    ((num exp)
     exp)

    ;; quoted constant: (quote x)
    ((and (pair? exp) (eq? (car exp) 'quote))
     (cadr exp))

    ;; variable
    ((isvar exp)
     (lookup (cadr exp) env))

    ;; special forms built in
    ((and (pair? exp) (eq? (car exp) 'atom))
     (atom? (eval* (cadr exp) env)))

    ((and (pair? exp) (eq? (car exp) 'eq))
     (eq? (eval* (cadr exp) env)
          (eval* (caddr exp) env)))

    ((and (pair? exp) (eq? (car exp) 'car))
     (car (eval* (cadr exp) env)))

    ((and (pair? exp) (eq? (car exp) 'cdr))
     (cdr (eval* (cadr exp) env)))

    ((and (pair? exp) (eq? (car exp) 'cons))
     (cons (eval* (cadr exp) env)
           (eval* (caddr exp) env)))

    ((and (pair? exp) (eq? (car exp) 'cond))
     (evcon (cdr exp) env))

    ;; function call where operator is a variable
    ((and (pair? exp) (isvar (car exp)))
      ;; (cons (lookup (cadar exp) env)
      ;;             (cdr exp))
     (eval* (cons (lookup (cadar exp) env)
                  (cdr exp))
            env))

    ;; ( (lambda (x ...) body) arg1 arg2 ...)
    ((and (pair? exp) (and (pair? (car exp))
          (eq? (caar exp) 'lambda)))
       ((defvar params (cadar exp))
       ((defvar body   (caddar exp))
       ((defvar args   (evlis (cdr exp) env))
       ((defvar env*   (pairlis params args env))
       (eval* body env*))))))

    ;; ( (define f (...) body) kont )
    ((and (pair? exp) (and (pair? (car exp))
          (eq? (caar exp) 'define)))
       ((defvar fname  (cadar exp))
       ((defvar lam    (cons 'lambda (cddar exp)))
       ((defvar env*   (cons (cons fname lam) env))
       (eval* (cadr exp) env*))))
       )

    (else
     (error -3))))

((define evalquote (fn args)
  (eval* (cons fn args) '()))

(eval* '((define 0 (1) (var 1)) ( (var 0) 42 )) '())

))))))))))))))))))))
