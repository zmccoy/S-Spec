


;Something to the point of
;
;(define x 
;  '(describe "Test of homework problems"
;             
;             (
;              (it "should accept negative numbers and return it"
;                  (
;                   (* 3 3)
;                   (should-be eq? 9 9)
;                   
;                   )
;                  )
;              
;              (it "should do something else"
;                  (
;                   (* 3 3)
;                   (should-be eq? 3 3)
;                   )
;                  )
;              )))
;
;(define fail
;  '(describe "Test of homework problems"
;             
;             (
;              (it "should accept negative numbers and return it"
;                  (
;                   (* 3 3)
;                   (should-be positive? (* -3 3))
;                   
;                   )
;                  )
;              
;              (it "should do something else"
;                  (
;                   (* 3 3)
;                   (should-be eq? 3 (* 4 4))
;                   )
;                  )
;              )))
;
;
;(define it-exp  '(it "should accept negative numbers and return it"
;                     (
;                      (* 3 3)
;                      (should-be equal? 1 (* 3 1))
;                      )
;                     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define s-tests?
  (lambda (lot)
    (every? (map it-exp? lot))))

(define s-spec?
  (lambda (exp)
    (and (list? exp)
         (eq? (length exp) 3)
         (eq? (s-spec->describe exp) 'describe)
         (string? (s-spec->description exp))
         (s-tests? (s-spec->list-of-tests exp)))))

(define s-spec->describe car)
(define s-spec->description cadr)
(define s-spec->list-of-tests caddr)

;list-of-tests ::= ()
; (test . list-of-tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   It-Expressions and Should-be

(define it-exp? 
  (lambda (test-exp)
    (and (list? test-exp)
         (eq? (length test-exp) 3)
         (eq? (it-exp->it test-exp) 'it)
         (string? (it-exp->description test-exp))
         (valid-body? test-exp))))

(define it-exp->it car)
(define it-exp->description cadr)
(define it-exp->body caddr)

;should-be-exp <operator> expected actual
;should-be-exp <unary-op> actual
;Example:  (should-be eq? 3 (* 1 3))
;          (should-be > 4 (* 1 3))
;          (should-be positive? 3)
 

(define it-exp->should-exp
  (lambda (it-exp)
    (car (last (it-exp->body it-exp)))))

;2nd value in should-exp ACTUAL
(define it-exp->actual 
  (lambda (it-exp)
    (let ((should-exp (it-exp->should-exp it-exp)))
      (if(= (length should-exp) 3)
         (eval (caddr should-exp))
         (eval (cadddr should-exp))))))

;1st value in should-exp EXPECTED
(define it-exp->expected
  (lambda (it-exp)
    (let ((should-exp (it-exp->should-exp it-exp)))
         (if(= (length should-exp) 3) ;If its unary it should expect true
            #t
            (caddr should-exp)))))

(define should-be
  (lambda (operator . values)
    (cond
      ((= (length values) 1) (operator (car values)))
      ((= (length values) 2) (operator (car values) (cadr values)))
      (else
       (error "Error -- Must have 1 or 2 values (Actual and Expected) depending on type of op (unary or binary)." values)))))

;Gets to the should-be keyword
(define it-exp->should-be
  (lambda (exp)
    (caar (last (it-exp->body exp))))) 

(define valid-body?
  (lambda (exp)
    (eq? (it-exp->should-be exp) 'should-be)))


;=============================================================
; S-Spec

(define s-spec
  (lambda (spec)
    (letrec ((spec-list (s-spec-eval spec))
             (spec-length (length spec-list))
             (test-list (s-spec->list-of-tests spec)))
             
      (begin
        (display "Testing: ")
        (display (s-spec->description spec))
        (newline)
        (display-break)
        (if (every? spec-list)
            (display-passed-test spec-length)
            (display-failed-test spec-length spec-list test-list))))))
            

(define display-break
  (lambda ()
    (display "...............................................\n")))

(define display-passed-test
  (lambda (spec-length)
    (newline)
    (display-line spec-length "of" spec-length "test(s) passed")))

(define display-failed-test
  (lambda (spec-length spec-list test-list)
    (newline)
    (display-line (count-false spec-list) "test(s) of" spec-length "failed.")
    (newline)
    (newline)
    (display-failed-descriptions (get-spots spec-list) test-list)))

(define display-failed-descriptions
  (lambda (pos-of-failed test-list)
      (if(null? pos-of-failed)
         (begin
           (display "Finished")
           (newline))
         (let ((current-test (list-ref test-list (car pos-of-failed))))
           (display (it-exp->description current-test))
           (newline)
           (display-line "Expected" (it-exp->expected current-test) "Got" (it-exp->actual current-test))
           (newline)
           (newline)
           (display-failed-descriptions (cdr pos-of-failed) test-list)))))
;=================================================================
; Evaluator

(define s-spec-eval
  (lambda (spec)
    (cond
      ((s-spec? spec) (s-spec-eval (s-spec->list-of-tests spec)))
      ((s-tests? spec) (map s-spec-eval spec))
      ((it-exp? spec) (eval (cons 'begin (it-exp->body spec))))
      (else
       (error "Error -- Wrong format for S-Spec ")))))

;================================================================
; Utilities and Helpers

(define last
  (lambda (exp)
    (list-tail exp (- (length exp) 1))))

(define every?
  (lambda (lst)
    (if(null? lst)
       #t
       (if(car lst)
          (every? (cdr lst))
          #f))))


(define count-false
  (lambda (list-of-t-f)
    (count-helper list-of-t-f 0)))

(define count-helper
  (lambda (list-of-t-f counter)
    (if(null? list-of-t-f)
       counter
       (if(eq? (car list-of-t-f) #f)
          (count-helper (cdr list-of-t-f) (+ counter 1))
          (count-helper (cdr list-of-t-f) counter)))))

;Obtain zero-based value of #f in list
(define get-spots-helper
  (lambda (list-of-t-f spots index)
    (if (null? list-of-t-f)
        (reverse spots)
        (if(eq? (car list-of-t-f) #f)
           (get-spots-helper (cdr list-of-t-f) (cons index spots) (+ 1 index))
           (get-spots-helper (cdr list-of-t-f) spots (+ 1 index))))))

(define get-spots
  (lambda (list-of-t-f)
    (get-spots-helper list-of-t-f '() 0)))

(define display-line
  (lambda words
    (for-each display-line-helper words)))

(define display-line-helper
  (lambda (word)
    (begin
      (display word)
      (display " "))))


;Things to do in S-Spec
; 1) Count how many tests (DONE)
; 2) Display the Testing Description (DONE)
; 3) How many tests passed how many failed (DONE)
; 4) Which failed and the description of that test (DONE)
; 5) If all tests pass then say # of # tests passed (DONE)
; 6) Should-be can take symbol / operator (DONE)  
; 11) Need to check for the new version of should-be (DONE)
; 7) Make function to take t f list and give back zero based spots of each (DONE)
;10) then you can use another function to get that spot and say whatever is needed. (DONE)
;11) should-be should take VARIABLEARITY instead.  1 or 2 make sure works (DONE)

; 8) Test on "REAL" data.  Make a test for an assignment (DONE)
; 9) Display line function (Doesn't accept (newline) but it shows structure better this way). (DONE)