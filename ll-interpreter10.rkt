;;
;; FILE:       ll-interpreter.rkt
;; AUTHOR:     Zachary Abbott
;; DATE:       11/18/11
;; COMMENT:    Homework 10 ll-interpreter
;; MODIFIED:   11/23/11 by Zach Abbott
;; CHANGE:     Tested functions

(load "environment.rkt")

(define ll/arith->first-exp car)
(define ll/arith->second-exp caddr)
(define ll/arith->operator cadr)
(define ll/number-exp? number?)

(define ll/exp?
  (lambda (exp)
    (or (ll/number-exp? exp)
        (ll/varref-exp?  exp)
        (ll/unary-operation? exp)
        (ll/operation-exp? exp)
        (ll/with-do-exp? exp))))

(define ll/operators '(+ - * / % @))

(define ll/operator?
  (lambda (exp)
    (member exp ll/operators)))

(define ll/operation-exp?
  (lambda (exp)
    (and (list? exp)
         (= (length exp) 3)
         (ll/exp? (ll/arith->first-exp exp))
         (ll/operator? (ll/arith->operator exp))
         (ll/exp? (ll/arith->second-exp exp)))))

(define ll/eval-exp
  (lambda (exp env)
    (cond ((ll/number-exp? exp) exp)
          ((ll/varref-exp? exp) (apply-ff env exp))
          ((ll/unary-operation? exp) 
           (ll/eval-unary-helper (ll/unary->op exp)
                                 (ll/eval-exp(ll/unary->body exp)
                                             env))) 
          ((ll/operation-exp? exp)  
           (ll/eval-helper (ll/arith->operator exp)
                           (ll/eval-exp(ll/arith->first-exp exp)  env)
                           (ll/eval-exp(ll/arith->second-exp exp) env)))
          ((ll/with-do-exp? exp)  (ll/eval-exp (ll/with->body exp) 
                                               (extend-ff 
                                                (ll/with->variable exp)
                                                (ll/eval-exp (ll/with->value exp) env)
                                                env)))
          (else
           (error "ll -- illegal expression" exp)))))

(define ll/eval-helper
  (lambda (operator exp1 exp2)
    (cond
      ((eq? operator '+) (+ exp1 exp2))
      ((eq? operator '-) (- exp1 exp2))
      ((eq? operator '*) (* exp1 exp2))
      ((eq? operator '%) (modulo exp1 exp2))
      ((eq? operator '/) (/ exp1 exp2))
      (else (error "ll -- illegal operator" exp)))))

(define ll/eval-unary-helper
  (lambda (unary-op exp)
    (cond
      ((eq? unary-op 'sqrt) (sqrt exp))
      ((eq? unary-op '-   ) (- exp))
      (else (error "ll -- illegal unary operator" exp)))))

(define ll/pre-process
  (lambda (exp)
    (cond
      ((ll/number-exp? exp) exp)
      ((ll/varref-exp? exp) exp)
      ((ll/unary-operation? exp) (list (ll/unary->op exp)
                                       (ll/pre-process (ll/unary->body exp))))
      ((ll/operation-exp? exp) (if(eq?(ll/arith->operator exp) '@)
                                  (list (list(ll/pre-process(ll/arith->first-exp exp)) 
                                             '+ 
                                             (ll/pre-process(ll/arith->second-exp exp)))
                                        '/ 2)
                                  (list(ll/pre-process(ll/arith->first-exp exp))
                                       (ll/arith->operator exp)
                                       (ll/pre-process(ll/arith->second-exp exp)))))
      ((ll/with-do-exp? exp)   (list 'with (ll/with->variable exp)
                                     '= (ll/pre-process (ll/with->value exp))
                                     'do (ll/pre-process (ll/with->body exp))))
      (else
       (error "ll-- illegal expression" exp)))))

;2

(define ll/primitives '(pi e))

(define ll/primitive? 
  (lambda (exp)
    (member exp ll/primitives)))

(define ll/varref-exp? symbol?)

(define environment (extend-ff 'pi 3.1415926 (extend-ff 'e 2.7182818 (empty-ff))))

;3

(define ll/eval
  (lambda (exp)
    (ll/eval-exp (ll/pre-process exp) environment)))

;4

(define ll/unary-ops '(- sqrt))

(define ll/unary->op car)

(define ll/unary->body cadr)

(define ll/unary-operator?
  (lambda (exp)
    (member exp ll/unary-ops)))

(define ll/unary-operation?
  (lambda (exp)
    (and (= (length exp) 2)
         (ll/unary-operator? (ll/unary->op exp))
         (ll/exp? (ll/unary->body exp)))))

;5

(define ll/with->with car)

(define ll/with->variable cadr)

(define ll/with->in caddr)

(define ll/with->do
  (lambda (exp)
    (car(cddddr exp))))

(define ll/with->= caddr)

(define ll/with->value cadddr)

(define ll/with->body
  (lambda (exp)
    (cadr (cddddr exp))))

(define ll/with-do-exp?
  (lambda (exp)
    (and (= (length exp) 6)
         (eq? (ll/with->with exp) 'with)
         (ll/varref-exp? (ll/with->variable exp))
         (eq? (ll/with->= exp) '=)
         (ll/exp? (ll/with->value exp))
         (eq? (ll/with->do exp) 'do)
         (ll/exp? (ll/with->body exp)))))
