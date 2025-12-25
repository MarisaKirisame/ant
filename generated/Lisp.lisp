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

    ;; bare symbols for booleans and else
    ((symbol? exp)
     (cond ((eq? exp 'else) 0)
           ((eq? exp 'true) 0)
           ((eq? exp 'false) '())
           (else (error -3))))

    ;; lists
    ((pair? exp)
     (cond
       ;; special forms and builtins keyed by symbol
       ((symbol? (car exp))
        (cond
          ;; quoted constant: (quote x)
          ((eq? (car exp) 'quote)
           (cadr exp))

          ((and (eq? (car exp) 'and)
                (pair? (cdr exp)))
           (if (eval* (cadr exp) env)
               (if (eval* (caddr exp) env)
                   true
                   false)
               false))

          ((and (eq? (car exp) 'if)
                (pair? (cdr exp))
                (pair? (cddr exp)))
           (if (eval* (cadr exp) env)
               (eval* (caddr exp) env)
               (eval* (cadddr exp) env)))

          ((eq? (car exp) 'null)
           (null? (eval* (cadr exp) env)))

          ((eq? (car exp) 'error)
           (error (eval* (cadr exp) env)))

          ((eq? (car exp) 'pair)
           (pair? (eval* (cadr exp) env)))

          ((eq? (car exp) 'symbol)
           (symbol? (eval* (cadr exp) env)))

          ((eq? (car exp) 'num)
           (num (eval* (cadr exp) env)))

          ((eq? (car exp) 'atom)
           (atom? (eval* (cadr exp) env)))

          ((eq? (car exp) 'eq)
           (eq? (eval* (cadr exp) env)
                (eval* (caddr exp) env)))

          ((eq? (car exp) 'car)
           (car (eval* (cadr exp) env)))

          ((eq? (car exp) 'cdr)
           (cdr (eval* (cadr exp) env)))

          ((eq? (car exp) 'cons)
           (cons (eval* (cadr exp) env)
                 (eval* (caddr exp) env)))

          ((eq? (car exp) 'cond)
           (evcon (cdr exp) env))

          (else
           (error -3))))

       ;; function call where operator is a variable
       ((isvar (car exp))
        (eval* (cons (lookup (cadar exp) env)
                     (cdr exp))
               env))

       ;; ( (lambda (x ...) body) arg1 arg2 ...)
       ((and (pair? (car exp))
             (eq? (caar exp) 'lambda))
        ((defvar params (cadar exp))
        ((defvar body   (caddar exp))
        ((defvar args   (evlis (cdr exp) env))
        ((defvar env*   (pairlis params args env))
        (eval* body env*))))))

       ;; ( (define f (...) body) kont )
       ((and (pair? (car exp))
             (eq? (caar exp) 'define))
        ((defvar fname  (cadar exp))
        ((defvar lam    (cons 'lambda (cddar exp)))
        ((defvar env*   (cons (cons fname lam) env))
        (eval* (cadr exp) env*)))))

       ;; ( (defvar i val) kont )
       ((and (pair? (car exp))
             (eq? (caar exp) 'defvar))
        ((defvar vname  (cadar exp))
        ((defvar val    (eval* (caddar exp) env))
        ((defvar env*   (cons (cons vname val) env))
        (eval* (cadr exp) env*)))))

       (else
        (error -3))))

    (else
     (error -3)
     )))

((define wrap (code)
  `((define 0 (20) (null #20)) ((define 1 (21 22) (eq #21 #22)) ((define 2 (23) (symbol #23)) ((define 3 (24) (pair #24)) ((define 4 (25) (atom #25)) ((define 5 (26) (car (car #26))) ((define 6 (27) (cdr (car #27))) ((define 7 (28) (cdr (cdr #28))) ((define 8 (29) (cdr (cdr (car #29)))) ((define 9 (30) (car (cdr (car #30)))) ((define 10 (31) (car (cdr (cdr (car #31))))) ((define 11 (32) (car (cdr (cdr (cdr #32))))) ((define 12 (33) (car (cdr (cdr #33)))) ((define 13 (34) (car (cdr #34))) ((define 14 (35 36) (cond ((#0 #36) (error -1)) ((#1 #35 (#5 #36)) (#6 #36)) (else (#14 #35 (cdr #36))))) ((define 15 (37 38 39) (if (#0 #37) #39 (cons (cons (car #37) (car #38)) (#15 (cdr #37) (cdr #38) #39)))) ((define 16 (40) (and (#3 #40) (#1 (car #40) 'var))) ((define 17 (41 42) (if (#0 #41) '() (cons (#19 (car #41) #42) (#17 (cdr #41) #42)))) ((define 18 (43 44) (cond ((#0 #43) (error -2)) ((#19 (#5 #43) #44) (#19 (#9 #43) #44)) (else (#18 (cdr #43) #44)))) ((define 19 (45 46) (cond ((num #45) #45) ((#16 #45) (#14 (#13 #45) #46)) ((#2 #45) (cond ((#1 #45 'else) 0) ((#1 #45 'true) 0) ((#1 #45 'false) '()) (else (error -3)))) ((#3 #45) (cond ((#2 (car #45)) (cond ((#1 (car #45) 'quote) (#13 #45)) ((and (#1 (car #45) 'and) (#3 (cdr #45))) (if (#19 (#13 #45) #46) (if (#19 (#12 #45) #46) true false) false)) ((and (#1 (car #45) 'if) (#3 (cdr #45)) (#3 (#7 #45))) (if (#19 (#13 #45) #46) (#19 (#12 #45) #46) (#19 (#11 #45) #46))) ((#1 (car #45) 'null) (#0 (#19 (#13 #45) #46))) ((#1 (car #45) 'error) (error (#19 (#13 #45) #46))) ((#1 (car #45) 'pair) (#3 (#19 (#13 #45) #46))) ((#1 (car #45) 'symbol) (#2 (#19 (#13 #45) #46))) ((#1 (car #45) 'num) (num (#19 (#13 #45) #46))) ((#1 (car #45) 'atom) (#4 (#19 (#13 #45) #46))) ((#1 (car #45) 'eq) (#1 (#19 (#13 #45) #46) (#19 (#12 #45) #46))) ((#1 (car #45) 'car) (car (#19 (#13 #45) #46))) ((#1 (car #45) 'cdr) (cdr (#19 (#13 #45) #46))) ((#1 (car #45) 'cons) (cons (#19 (#13 #45) #46) (#19 (#12 #45) #46))) ((#1 (car #45) 'cond) (#18 (cdr #45) #46)) (else (error -3)))) ((#16 (car #45)) (#19 (cons (#14 (#9 #45) #46) (cdr #45)) #46)) ((and (#3 (car #45)) (#1 (#5 #45) 'lambda)) ((defvar 47 (#9 #45)) ((defvar 48 (#10 #45)) ((defvar 49 (#17 (cdr #45) #46)) ((defvar 50 (#15 #47 #49 #46)) (#19 #48 #50)))))) ((and (#3 (car #45)) (#1 (#5 #45) 'define)) ((defvar 51 (#9 #45)) ((defvar 52 (cons 'lambda (#8 #45))) ((defvar 53 (cons (cons #51 #52) #46)) (#19 (#13 #45) #53))))) ((and (#3 (car #45)) (#1 (#5 #45) 'defvar)) ((defvar 54 (#9 #45)) ((defvar 55 (#19 (#10 #45) #46)) ((defvar 56 (cons (cons #54 #55) #46)) (#19 (#13 #45) #56))))) (else (error -3)))) (else (error -3)))) (#19 (quote ,code) '()) ))))))))))))))))))))
  )

((define eval_wrap (code)
  (eval* (wrap code) '())
  )

(eval* %CODE% '())

;; (eval_wrap '((define 0 (1) (var 1)) ((var 0) 42)))

;; ;; pure lambda application
;; (eval_wrap '((lambda (0) (var 0)) 99))

;; ;; closure capturing an outer variable
;; (eval_wrap '(((lambda (0) ((lambda (1) (var 0)) 42)) 7)))

;; ;; defvar introduces bindings visible to subsequent forms
;; (eval_wrap '((defvar 0 (quote 123)) (var 0)))

;; multi-argument function created via define
;; (eval_wrap '((define 0 (1 2) (cons (var 1) (var 2))) ((var 0) 1 (quote (2)))))

))))))))))))))))))))))
