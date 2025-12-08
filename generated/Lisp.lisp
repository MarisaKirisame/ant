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

((define else () 0)

((define caar (xs) (car (car xs)))

((define cdar (xs) (cdr (car xs)))

((define cddr (xs) (cdr (cdr xs)))

((define cddar (xs) (cdr (cdr (car xs))))

((define cadar (xs) (car (cdr (car xs))))

((define caddar (xs) (car (cdr (cdr (car xs)))))

((define cadddr (xs) (car (cdr (cdr (cdr xs)))))

((define caddr (xs) (car (cdr (cdr xs))))

((define cadr (xs) (car (cdr xs)))

((define lookup (x env)
  (cond ((null? env) (error -1))
        ((eq? x (caar env)) (cdar env))
        (else (lookup x (cdr env)))))

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

    ;; variable
    ((isvar exp)
     (lookup (cadr exp) env))

    ;; quoted constant: (quote x)
    ((and (pair? exp) (eq? (car exp) 'quote))
     (cadr exp))

    ;; true and false symbols
    ((and (symbol? exp) (eq? exp 'true)) 0)

    ((and (symbol? exp) (eq? exp 'false)) '())

    ((and (and (pair? exp) (eq? (car exp) 'and) ) (pair? (cdr exp)) )
     (if (eval* (cadr exp) env)
         (if (eval* (caddr exp) env)
             true
             false)
         false))

    ((and (and (and (pair? exp) (eq? (car exp) 'if) ) (pair? (cdr exp)) ) (pair? (cddr exp)))
     (if (eval* (cadr exp) env)
         (eval* (caddr exp) env)
         (eval* (cadddr exp) env)))

    ;; special forms built in
    ((and (pair? exp) (eq? (car exp) 'null))
     (null? (eval* (cadr exp) env)))

    ((and (pair? exp) (eq? (car exp) 'error))
     (error (eval* (cadr exp) env)))

    ((and (pair? exp) (eq? (car exp) 'pair))
     (pair? (eval* (cadr exp) env)))

    ((and (pair? exp) (eq? (car exp) 'num))
     (num (eval* (cadr exp) env)))

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
       (eval* (cadr exp) env*)))))

    ;; ( (defvar i val) kont )
    ((and (pair? exp) (and (pair? (car exp))
          (eq? (caar exp) 'defvar)))
       ((defvar vname  (cadar exp))
       ((defvar val    (eval* (caddar exp) env))
       ((defvar env*   (cons (cons vname val) env))
       (eval* (cadr exp) env*)))))

    (else
     (error -3))))

((define evalquote (fn args)
  (eval* (cons fn args) '()))

;; (eval* '((define 0 (1) (var 1)) ( (var 0) 42 )) '())

(eval* '((define 0 (22) (null (var 22))) ((define 1 (23 24) (eq (var 23) (var 24))) ((define 2 (25) (symbol (var 25))) ((define 3 (26) (pair (var 26))) ((define 4 (27) (atom (var 27))) ((define 5 () 0) ((define 6 (28) (car (car (var 28)))) ((define 7 (29) (cdr (car (var 29)))) ((define 8 (30) (cdr (cdr (var 30)))) ((define 9 (31) (cdr (cdr (car (var 31))))) ((define 10 (32) (car (cdr (car (var 32))))) ((define 11 (33) (car (cdr (cdr (car (var 33)))))) ((define 12 (34) (car (cdr (cdr (cdr (var 34)))))) ((define 13 (35) (car (cdr (cdr (var 35))))) ((define 14 (36) (car (cdr (var 36)))) ((define 15 (37 38) (cond (((var 0) (var 38)) (error -1)) (((var 1) (var 37) ((var 6) (var 38))) ((var 7) (var 38))) ((var 5) ((var 15) (var 37) (cdr (var 38)))))) ((define 16 (39 40 41) (if ((var 0) (var 39)) (var 41) (cons (cons (car (var 39)) (car (var 40))) ((var 16) (cdr (var 39)) (cdr (var 40)) (var 41))))) ((define 17 (42) (and ((var 3) (var 42)) ((var 1) (car (var 42)) (quote var)))) ((define 18 (43 44) (if ((var 0) (var 43)) (quote ()) (cons ((var 20) (car (var 43)) (var 44)) ((var 18) (cdr (var 43)) (var 44))))) ((define 19 (45 46) (cond (((var 0) (var 45)) (error -2)) (((var 20) ((var 6) (var 45)) (var 46)) ((var 20) ((var 10) (var 45)) (var 46))) ((var 5) ((var 19) (cdr (var 45)) (var 46))))) ((define 20 (47 48) (cond ((num (var 47)) (var 47)) (((var 17) (var 47)) ((var 15) ((var 14) (var 47)) (var 48))) ((and ((var 3) (var 47)) ((var 1) (car (var 47)) (quote quote))) ((var 14) (var 47))) ((and ((var 2) (var 47)) ((var 1) (var 47) (quote true))) 0) ((and ((var 2) (var 47)) ((var 1) (var 47) (quote false))) (quote ())) ((and (and ((var 3) (var 47)) ((var 1) (car (var 47)) (quote and))) ((var 3) (cdr (var 47)))) (if ((var 20) ((var 14) (var 47)) (var 48)) (if ((var 20) ((var 13) (var 47)) (var 48)) true false) false)) ((and (and (and ((var 3) (var 47)) ((var 1) (car (var 47)) (quote if))) ((var 3) (cdr (var 47)))) ((var 3) ((var 8) (var 47)))) (if ((var 20) ((var 14) (var 47)) (var 48)) ((var 20) ((var 13) (var 47)) (var 48)) ((var 20) ((var 12) (var 47)) (var 48)))) ((and ((var 3) (var 47)) ((var 1) (car (var 47)) (quote null))) ((var 0) ((var 20) ((var 14) (var 47)) (var 48)))) ((and ((var 3) (var 47)) ((var 1) (car (var 47)) (quote error))) (error ((var 20) ((var 14) (var 47)) (var 48)))) ((and ((var 3) (var 47)) ((var 1) (car (var 47)) (quote pair))) ((var 3) ((var 20) ((var 14) (var 47)) (var 48)))) ((and ((var 3) (var 47)) ((var 1) (car (var 47)) (quote num))) (num ((var 20) ((var 14) (var 47)) (var 48)))) ((and ((var 3) (var 47)) ((var 1) (car (var 47)) (quote atom))) ((var 4) ((var 20) ((var 14) (var 47)) (var 48)))) ((and ((var 3) (var 47)) ((var 1) (car (var 47)) (quote eq))) ((var 1) ((var 20) ((var 14) (var 47)) (var 48)) ((var 20) ((var 13) (var 47)) (var 48)))) ((and ((var 3) (var 47)) ((var 1) (car (var 47)) (quote car))) (car ((var 20) ((var 14) (var 47)) (var 48)))) ((and ((var 3) (var 47)) ((var 1) (car (var 47)) (quote cdr))) (cdr ((var 20) ((var 14) (var 47)) (var 48)))) ((and ((var 3) (var 47)) ((var 1) (car (var 47)) (quote cons))) (cons ((var 20) ((var 14) (var 47)) (var 48)) ((var 20) ((var 13) (var 47)) (var 48)))) ((and ((var 3) (var 47)) ((var 1) (car (var 47)) (quote cond))) ((var 19) (cdr (var 47)) (var 48))) ((and ((var 3) (var 47)) ((var 17) (car (var 47)))) ((var 20) (cons ((var 15) ((var 10) (var 47)) (var 48)) (cdr (var 47))) (var 48))) ((and ((var 3) (var 47)) (and ((var 3) (car (var 47))) ((var 1) ((var 6) (var 47)) (quote lambda)))) ((defvar 49 ((var 10) (var 47))) ((defvar 50 ((var 11) (var 47))) ((defvar 51 ((var 18) (cdr (var 47)) (var 48))) ((defvar 52 ((var 16) (var 49) (var 51) (var 48))) ((var 20) (var 50) (var 52))))))) ((and ((var 3) (var 47)) (and ((var 3) (car (var 47))) ((var 1) ((var 6) (var 47)) (quote define)))) ((defvar 53 ((var 10) (var 47))) ((defvar 54 (cons (quote lambda) ((var 9) (var 47)))) ((defvar 55 (cons (cons (var 53) (var 54)) (var 48))) ((var 20) ((var 14) (var 47)) (var 55)))))) ((and ((var 3) (var 47)) (and ((var 3) (car (var 47))) ((var 1) ((var 6) (var 47)) (quote defvar)))) ((defvar 56 ((var 10) (var 47))) ((defvar 57 ((var 20) ((var 11) (var 47)) (var 48))) ((defvar 58 (cons (cons (var 56) (var 57)) (var 48))) ((var 20) ((var 14) (var 47)) (var 58)))))) ((var 5) (error -3)))) ((define 21 (59 60) ((var 20) (cons (var 59) (var 60)) (quote ()))) ((var 20) (quote ((define 0 (1) (var 1)) ((var 0) 42))) (quote ())))))))))))))))))))))))) '())

;; (eval* '((defvar 0 (var 1)) (var 0)) (cons (cons 1 2) '() ))

;; (eval* (cons 'quote (cons 1 '())) '())

;; (eval* '(quote (var 1)) '())

;; (eval* 'false '())

;; (eval* '(if false 1 2) '())

;; (eval* '(var 3) (cons (cons 3 42) '() ))

;; (eval* '(error -100) '())

;; (eval* '(and true false) '())

;; (eval* '(and true true) '())

;; (eval* '(pair '()) '())

;; (eval* '(pair '(1)) '())

))))))))))))))))))))))
