#lang racket
(require "other.rkt")
(require "parser.rkt")
(require "runner.rkt")
(define env '((a 1) (b 2) (c 5)))
;(define sample-code '(call (function()(ask (bool > a b)(math - 1 2)(math - 1 2)))(a)))
;(define parsed-neo-code (neo-parser sample-code))
;(display parsed-neo-code)
;(display "\n")
;(run-neo-parsed-code parsed-neo-code env)

(define parsed-neo-code (neo-parser '(call (function(x) (local-vars ((a 3) (b 7) (c 3)) (math + a b))) (5))))


(run-neo-parsed-code parsed-neo-code env)

;(neo-parser '(local-vars((a 1)(b 2)(c 3))))
