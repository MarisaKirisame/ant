;; Environment = list of (int . value) pairs

(define null? (xs)
    (null xs)
)

(define eq? (a b)
    (eq a b))

(define else () (quote 0))

(define caar (xs) (car (car xs)))
(define cdar (xs) (cdr (car xs)))
(define cadar (xs) (car (cdr (car xs))))
(define cadr (xs) (car (cdr xs)))

(define lookup (x env)
  (cond ((null? env) (error -1))
        ((eq? x (caar env)) (cadar env))
        ((quote 0) (lookup x (cdr env)))))

(lookup 1 '( (1 2) (0 1) ))

; ((define 12 (13 14) (cond ((#0 #14) (error 0)) ((#2 #13 (#6 #14)) (#8 #14)) ((quote 0) (#12 #13 (cdr #14))))) (#12 1 (quote (1 2 3 4))))

; (define (pairlis xs ys env)
;   (if (null? xs)
;       env
;       (cons (cons (car xs) (car ys))
;             (pairlis (cdr xs) (cdr ys) env))))
; 
; (define (evlis exps env)
;   (if (null? exps)
;       '()
;       (cons (eval* (car exps) env)
;             (evlis (cdr exps) env))))
; 
; (define (evcon clauses env)
;   (cond ((null? clauses) (error "no cond clause matched"))
;         ((eval* (caar clauses) env)
;          (eval* (cadar clauses) env))
;         (else
;          (evcon (cdr clauses) env))))
; 
; (define (eval* exp env)
;   (cond
;     ;; variable
;     ((symbol? exp)
;      (lookup exp env))
; 
;     ;; quoted constant: (quote x)
;     ((and (pair? exp) (eq? (car exp) 'quote))
;      (cadr exp))
; 
;     ;; special forms built in
;     ((and (pair? exp) (eq? (car exp) 'atom))
;      (atom? (eval* (cadr exp) env)))
; 
;     ((and (pair? exp) (eq? (car exp) 'eq))
;      (eq? (eval* (cadr exp) env)
;           (eval* (caddr exp) env)))
; 
;     ((and (pair? exp) (eq? (car exp) 'car))
;      (car (eval* (cadr exp) env)))
; 
;     ((and (pair? exp) (eq? (car exp) 'cdr))
;      (cdr (eval* (cadr exp) env)))
; 
;     ((and (pair? exp) (eq? (car exp) 'cons))
;      (cons (eval* (cadr exp) env)
;            (eval* (caddr exp) env)))
; 
;     ((and (pair? exp) (eq? (car exp) 'cond))
;      (evcon (cdr exp) env))
; 
;     ;; function call where operator is a symbol
;     ((symbol? (car exp))
;      (eval* (cons (lookup (car exp) env)
;                   (cdr exp))
;             env))
; 
;     ;; ( (lambda (x ...) body) arg1 arg2 ...)
;     ((and (pair? (car exp))
;           (eq? (caar exp) 'lambda))
;      (let* ((params (cadar exp))
;             (body   (caddar exp))
;             (args   (evlis (cdr exp) env))
;             (env*   (pairlis params args env)))
;        (eval* body env*)))
; 
;     ;; ( (label f (lambda (...) body)) arg1 arg2 ...)
;     ((and (pair? (car exp))
;           (eq? (caar exp) 'label))
;      (let* ((fname  (cadar exp))
;             (fdef   (caddar exp))
;             (env*   (cons (cons fname (car exp)) env)))
;        (eval* (cons fdef (cdr exp)) env*)))
; 
;     (else
;      (error "bad expression" exp))))
; 
; ;; McCarthy’s entry point was essentially:
; ;;   evalquote[fn; args] = apply[fn; args; NIL]
; ;; In this style you can write:
; (define (evalquote fn args)
;   (eval* (cons fn args) '()))
