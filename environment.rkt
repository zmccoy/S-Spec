;;
;; FILE:       environment.rkt
;; AUTHOR:     Zachary Abbott
;; DATE:       11/18/11
;; COMMENT:    Homework 10 environment with "tests"
;; MODIFIED:   11/23/11 by Zach Abbott
;; CHANGE:     Tested functions

(define pair->value cdr)

(define empty-ff 
  (lambda ()
    '()))

(define extend-ff
  (lambda (sym val ff)
    (cons (cons sym val)
          ff)))

(define apply-ff
  (lambda (ff sym)
    (if(assoc sym ff)
       (pair->value(assoc sym ff))
       (error "error: finite funciton -- argument not in domain" sym))))

(define extend-ff*
  (lambda (symbol-list value-list ff)
    (if (null? symbol-list) 
        ff
        (extend-ff (car symbol-list) (car value-list)
                   (extend-ff* (cdr symbol-list)
                               (cdr value-list)
                               ff)))))

