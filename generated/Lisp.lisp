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
    ((and (symbol? exp) (eq? exp 'else)) 0)

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

    ((and (pair? exp) (eq? (car exp) 'symbol))
     (symbol? (eval* (cadr exp) env)))

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
     (error -3)
     )))

((define evalquote (fn args)
  (eval* (cons fn args) '()))

;; (eval* '((define 0 (1) (var 1)) ( (var 0) 42 )) '())

;; (eval* '((define 0 (21) (null (var 21))) ((define 1 (22 23) (eq (var 22) (var 23))) ((define 2 (24) (symbol (var 24))) ((define 3 (25) (pair (var 25))) ((define 4 (26) (atom (var 26))) ((define 5 (27) (car (car (var 27)))) ((define 6 (28) (cdr (car (var 28)))) ((define 7 (29) (cdr (cdr (var 29)))) ((define 8 (30) (cdr (cdr (car (var 30))))) ((define 9 (31) (car (cdr (car (var 31))))) ((define 10 (32) (car (cdr (cdr (car (var 32)))))) ((define 11 (33) (car (cdr (cdr (cdr (var 33)))))) ((define 12 (34) (car (cdr (cdr (var 34))))) ((define 13 (35) (car (cdr (var 35)))) ((define 14 (36 37) (cond (((var 0) (var 37)) (error -1)) (((var 1) (var 36) ((var 5) (var 37))) ((var 6) (var 37))) (else ((var 14) (var 36) (cdr (var 37)))))) ((define 15 (38 39 40) (if ((var 0) (var 38)) (var 40) (cons (cons (car (var 38)) (car (var 39))) ((var 15) (cdr (var 38)) (cdr (var 39)) (var 40))))) ((define 16 (41) (and ((var 3) (var 41)) ((var 1) (car (var 41)) (quote var)))) ((define 17 (42 43) (if ((var 0) (var 42)) (quote ()) (cons ((var 19) (car (var 42)) (var 43)) ((var 17) (cdr (var 42)) (var 43))))) ((define 18 (44 45) (cond (((var 0) (var 44)) (error -2)) (((var 19) ((var 5) (var 44)) (var 45)) ((var 19) ((var 9) (var 44)) (var 45))) (else ((var 18) (cdr (var 44)) (var 45))))) ((define 19 (46 47) (cond ((num (var 46)) (var 46)) (((var 16) (var 46)) ((var 14) ((var 13) (var 46)) (var 47))) ((and ((var 3) (var 46)) ((var 1) (car (var 46)) (quote quote))) ((var 13) (var 46))) ((and ((var 2) (var 46)) ((var 1) (var 46) (quote else))) 0) ((and ((var 2) (var 46)) ((var 1) (var 46) (quote true))) 0) ((and ((var 2) (var 46)) ((var 1) (var 46) (quote false))) (quote ())) ((and (and ((var 3) (var 46)) ((var 1) (car (var 46)) (quote and))) ((var 3) (cdr (var 46)))) (if ((var 19) ((var 13) (var 46)) (var 47)) (if ((var 19) ((var 12) (var 46)) (var 47)) true false) false)) ((and (and (and ((var 3) (var 46)) ((var 1) (car (var 46)) (quote if))) ((var 3) (cdr (var 46)))) ((var 3) ((var 7) (var 46)))) (if ((var 19) ((var 13) (var 46)) (var 47)) ((var 19) ((var 12) (var 46)) (var 47)) ((var 19) ((var 11) (var 46)) (var 47)))) ((and ((var 3) (var 46)) ((var 1) (car (var 46)) (quote null))) ((var 0) ((var 19) ((var 13) (var 46)) (var 47)))) ((and ((var 3) (var 46)) ((var 1) (car (var 46)) (quote error))) (error ((var 19) ((var 13) (var 46)) (var 47)))) ((and ((var 3) (var 46)) ((var 1) (car (var 46)) (quote pair))) ((var 3) ((var 19) ((var 13) (var 46)) (var 47)))) ((and ((var 3) (var 46)) ((var 1) (car (var 46)) (quote symbol))) ((var 2) ((var 19) ((var 13) (var 46)) (var 47)))) ((and ((var 3) (var 46)) ((var 1) (car (var 46)) (quote num))) (num ((var 19) ((var 13) (var 46)) (var 47)))) ((and ((var 3) (var 46)) ((var 1) (car (var 46)) (quote atom))) ((var 4) ((var 19) ((var 13) (var 46)) (var 47)))) ((and ((var 3) (var 46)) ((var 1) (car (var 46)) (quote eq))) ((var 1) ((var 19) ((var 13) (var 46)) (var 47)) ((var 19) ((var 12) (var 46)) (var 47)))) ((and ((var 3) (var 46)) ((var 1) (car (var 46)) (quote car))) (car ((var 19) ((var 13) (var 46)) (var 47)))) ((and ((var 3) (var 46)) ((var 1) (car (var 46)) (quote cdr))) (cdr ((var 19) ((var 13) (var 46)) (var 47)))) ((and ((var 3) (var 46)) ((var 1) (car (var 46)) (quote cons))) (cons ((var 19) ((var 13) (var 46)) (var 47)) ((var 19) ((var 12) (var 46)) (var 47)))) ((and ((var 3) (var 46)) ((var 1) (car (var 46)) (quote cond))) ((var 18) (cdr (var 46)) (var 47))) ((and ((var 3) (var 46)) ((var 16) (car (var 46)))) ((var 19) (cons ((var 14) ((var 9) (var 46)) (var 47)) (cdr (var 46))) (var 47))) ((and ((var 3) (var 46)) (and ((var 3) (car (var 46))) ((var 1) ((var 5) (var 46)) (quote lambda)))) ((defvar 48 ((var 9) (var 46))) ((defvar 49 ((var 10) (var 46))) ((defvar 50 ((var 17) (cdr (var 46)) (var 47))) ((defvar 51 ((var 15) (var 48) (var 50) (var 47))) ((var 19) (var 49) (var 51))))))) ((and ((var 3) (var 46)) (and ((var 3) (car (var 46))) ((var 1) ((var 5) (var 46)) (quote define)))) ((defvar 52 ((var 9) (var 46))) ((defvar 53 (cons (quote lambda) ((var 8) (var 46)))) ((defvar 54 (cons (cons (var 52) (var 53)) (var 47))) ((var 19) ((var 13) (var 46)) (var 54)))))) ((and ((var 3) (var 46)) (and ((var 3) (car (var 46))) ((var 1) ((var 5) (var 46)) (quote defvar)))) ((defvar 55 ((var 9) (var 46))) ((defvar 56 ((var 19) ((var 10) (var 46)) (var 47))) ((defvar 57 (cons (cons (var 55) (var 56)) (var 47))) ((var 19) ((var 13) (var 46)) (var 57)))))) (else (error -3)))) ((define 20 (58 59) ((var 19) (cons (var 58) (var 59)) (quote ()))) ((var 19) (quote ((define 0 (1) (var 1)) ((var 0) 42))) (quote ()))))))))))))))))))))))) '())

(eval* '((define 0 (21) (null (var 21))) ((define 1 (22 23) (eq (var 22) (var 23))) ((define 2 (24) (symbol (var 24))) ((define 3 (25) (pair (var 25))) ((define 4 (26) (atom (var 26))) ((define 5 (27) (car (car (var 27)))) ((define 6 (28) (cdr (car (var 28)))) ((define 7 (29) (cdr (cdr (var 29)))) ((define 8 (30) (cdr (cdr (car (var 30))))) ((define 9 (31) (car (cdr (car (var 31))))) ((define 10 (32) (car (cdr (cdr (car (var 32)))))) ((define 11 (33) (car (cdr (cdr (cdr (var 33)))))) ((define 12 (34) (car (cdr (cdr (var 34))))) ((define 13 (35) (car (cdr (var 35)))) ((define 14 (36 37) (cond (((var 0) (var 37)) (error -1)) (((var 1) (var 36) ((var 5) (var 37))) ((var 6) (var 37))) (else ((var 14) (var 36) (cdr (var 37)))))) ((define 15 (38 39 40) (if ((var 0) (var 38)) (var 40) (cons (cons (car (var 38)) (car (var 39))) ((var 15) (cdr (var 38)) (cdr (var 39)) (var 40))))) ((define 16 (41) (and ((var 3) (var 41)) ((var 1) (car (var 41)) (quote var)))) ((define 17 (42 43) (if ((var 0) (var 42)) (quote ()) (cons ((var 19) (car (var 42)) (var 43)) ((var 17) (cdr (var 42)) (var 43))))) ((define 18 (44 45) (cond (((var 0) (var 44)) (error -2)) (((var 19) ((var 5) (var 44)) (var 45)) ((var 19) ((var 9) (var 44)) (var 45))) (else ((var 18) (cdr (var 44)) (var 45))))) ((define 19 (46 47) (cond ((num (var 46)) (var 46)) (((var 16) (var 46)) ((var 14) ((var 13) (var 46)) (var 47))) ((and ((var 3) (var 46)) ((var 1) (car (var 46)) (quote quote))) ((var 13) (var 46))) ((and ((var 2) (var 46)) ((var 1) (var 46) (quote else))) 0) ((and ((var 2) (var 46)) ((var 1) (var 46) (quote true))) 0) ((and ((var 2) (var 46)) ((var 1) (var 46) (quote false))) (quote ())) ((and (and ((var 3) (var 46)) ((var 1) (car (var 46)) (quote and))) ((var 3) (cdr (var 46)))) (if ((var 19) ((var 13) (var 46)) (var 47)) (if ((var 19) ((var 12) (var 46)) (var 47)) true false) false)) ((and (and (and ((var 3) (var 46)) ((var 1) (car (var 46)) (quote if))) ((var 3) (cdr (var 46)))) ((var 3) ((var 7) (var 46)))) (if ((var 19) ((var 13) (var 46)) (var 47)) ((var 19) ((var 12) (var 46)) (var 47)) ((var 19) ((var 11) (var 46)) (var 47)))) ((and ((var 3) (var 46)) ((var 1) (car (var 46)) (quote null))) ((var 0) ((var 19) ((var 13) (var 46)) (var 47)))) ((and ((var 3) (var 46)) ((var 1) (car (var 46)) (quote error))) (error ((var 19) ((var 13) (var 46)) (var 47)))) ((and ((var 3) (var 46)) ((var 1) (car (var 46)) (quote pair))) ((var 3) ((var 19) ((var 13) (var 46)) (var 47)))) ((and ((var 3) (var 46)) ((var 1) (car (var 46)) (quote symbol))) ((var 2) ((var 19) ((var 13) (var 46)) (var 47)))) ((and ((var 3) (var 46)) ((var 1) (car (var 46)) (quote num))) (num ((var 19) ((var 13) (var 46)) (var 47)))) ((and ((var 3) (var 46)) ((var 1) (car (var 46)) (quote atom))) ((var 4) ((var 19) ((var 13) (var 46)) (var 47)))) ((and ((var 3) (var 46)) ((var 1) (car (var 46)) (quote eq))) ((var 1) ((var 19) ((var 13) (var 46)) (var 47)) ((var 19) ((var 12) (var 46)) (var 47)))) ((and ((var 3) (var 46)) ((var 1) (car (var 46)) (quote car))) (car ((var 19) ((var 13) (var 46)) (var 47)))) ((and ((var 3) (var 46)) ((var 1) (car (var 46)) (quote cdr))) (cdr ((var 19) ((var 13) (var 46)) (var 47)))) ((and ((var 3) (var 46)) ((var 1) (car (var 46)) (quote cons))) (cons ((var 19) ((var 13) (var 46)) (var 47)) ((var 19) ((var 12) (var 46)) (var 47)))) ((and ((var 3) (var 46)) ((var 1) (car (var 46)) (quote cond))) ((var 18) (cdr (var 46)) (var 47))) ((and ((var 3) (var 46)) ((var 16) (car (var 46)))) ((var 19) (cons ((var 14) ((var 9) (var 46)) (var 47)) (cdr (var 46))) (var 47))) ((and ((var 3) (var 46)) (and ((var 3) (car (var 46))) ((var 1) ((var 5) (var 46)) (quote lambda)))) ((defvar 48 ((var 9) (var 46))) ((defvar 49 ((var 10) (var 46))) ((defvar 50 ((var 17) (cdr (var 46)) (var 47))) ((defvar 51 ((var 15) (var 48) (var 50) (var 47))) ((var 19) (var 49) (var 51))))))) ((and ((var 3) (var 46)) (and ((var 3) (car (var 46))) ((var 1) ((var 5) (var 46)) (quote define)))) ((defvar 52 ((var 9) (var 46))) ((defvar 53 (cons (quote lambda) ((var 8) (var 46)))) ((defvar 54 (cons (cons (var 52) (var 53)) (var 47))) ((var 19) ((var 13) (var 46)) (var 54)))))) ((and ((var 3) (var 46)) (and ((var 3) (car (var 46))) ((var 1) ((var 5) (var 46)) (quote defvar)))) ((defvar 55 ((var 9) (var 46))) ((defvar 56 ((var 19) ((var 10) (var 46)) (var 47))) ((defvar 57 (cons (cons (var 55) (var 56)) (var 47))) ((var 19) ((var 13) (var 46)) (var 57)))))) (else (error -3)))) ((define 20 (58 59) ((var 19) (cons (var 58) (var 59)) (quote ()))) ((var 19) '1 (quote ()))))))))))))))))))))))) '())


)))))))))))))))))))))
