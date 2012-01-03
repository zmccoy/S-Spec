(load "s-spec.rkt")
(load "ll-interpreter10.rkt")


(define test
  '(describe "Test of homework problems from homework 10"
             
             (
              (it "Should return an empty-ff"
                  ((should-be eq? '() (empty-ff))))
              
              (it "Should return the extension"
                  ((should-be equal? '((bar . 5) (foo . 2)) (extend-ff 'bar 5 (extend-ff 'foo 2 (empty-ff))))))
              
              (it "Should be an expression"
                  ((should-be ll/exp? 'pi))) 
              
              (it "Should be a varref"
                  ((should-be ll/varref-exp? 'e)))
              
              (it "Should be an expression in our language"
                  ((should-be ll/exp? '(2 * e))))
              
              (it "Should be an expression in our language"
                  ((should-be ll/exp? '(2 * (2 * (2 * (2 * (2 * (2 * e)))))))))
              
              (it "Should pre-process into 'pi"
                  ((should-be eq? 'pi (ll/pre-process 'pi))))
              
              (it "Should pre-process the symbols from the environment"
                  ((should-be equal? '(((3.14 - 2) / e) % (15 / (pi % 2))) (ll/pre-process '(((3.14 - 2) / e) % (15 / (pi % 2)))))))
              
              (it "Should pre-process variables for environment"
                  ((should-be equal? '((2 * e) + ((pi + 29) / 2)) (ll/pre-process '((2 * e) + (pi @ 29))))))
              
              (it "Should evaluate pi into it's actual value"
                  ((should-be = 3.1415926 (ll/eval 'pi))))
              
              (it "Should evaluate the expression with environment variables"
                  ((should-be = 1.4033533772139797 (ll/eval '(((3.14 - 2) / e) @ (15 / (pi * 2)))))))
              
              (it "Should evaluate correctly with env variables"
                  ((should-be = 21.5073599 (ll/eval '((2 * e) + (pi @ 29))))))
              
              (it "Should pre-process - and env variables correctly"
                  ((should-be equal? '(- ((pi + 3) / 2)) (ll/pre-process '(- (pi @ 3))))))
              
              (it "Should evaluate with the - sign correctly"
                  ((should-be = -3.0707963 (ll/eval '(- (pi @ 3))))))
              
              (it "Should evaluate correctly with - sign"
                  ((should-be = 2 (ll/eval '(((130 + (- 2)) / 4) % (15 / (17 % 6)))))))
              
              (it "Should evaluate correctly"
                  ((should-be = 2 (ll/eval '(((130 - 2) / 4) % (15 / (17 % (sqrt 36))))))))
              
              (it "Should pre-process the with-do exp to right form."
                  ((should-be equal? '(with x = 2 do (x * 14)) (ll/pre-process '(with x = 2 do (x * 14))))))
              
              (it "Shoud pre-process with do exp with env variables correctly."
                  ((should-be equal? '(with x = ((pi + 14) / 2) do x) (ll/pre-process '(with x = (pi @ 14) do x)))))
              
              (it "Should pre-process nested with-do's correctly"
                  ((should-be equal? 
                              '(with x = 2 do
                                     (((with y = 15 do (y * x))
                                       +
                                       (with y = 77 do (y / x)))
                                      / 2))
                              (ll/pre-process '(with x = 2 do
                                                     ( (with y = 15 do (y * x))
                                                       @
                                                       (with y = 77 do (y / x)) ))))))
              
              (it "Should evaluate with-dos correctly"
                  ((should-be = 28 (ll/eval '(with x = 2 do (x * 14))))))
              
              (it "Should evaluate with-dos with env variables correctly"
                  ((should-be = 8.5707963 (ll/eval '(with x = (pi @ 14) do x)))))
              
              (it "Should evaluate with-dos with @ signs correctly"
                  ((should-be = 45.5 (ll/eval '(with x = 2 do
                                                       (with y = 10 do
                                                             ((15 + x) @ (y * 7.4))))))))
              
              
              )))
