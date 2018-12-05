# Manual

This Interpreter implements a subset of the [R5RS](http://www.schemers.org/Documents/Standards/R5RS/ "schemers.org: Documents: Standards: R5RS")

## Primitive expression types

### quote
#### syntax:  (quote datum)
~~~
(quote a)                               ;===>  a
'a                                      ;===>  a
'#(a b c)                               ;===>  #(a b c)
'()                                     ;===>  ()
'(+ 1 2)                                ;===>  (+ 1 2)
'(quote a)                              ;===>  'a
''a                                     ;===>  'a
~~~

### lambda
#### syntax:  (lambda formals body)
~~~
(lambda (x) (+ x x))                    ;===>  #<CLOSURE>
((lambda (x) (+ x x)) 4)                ;===>  8

(define reverse-subtract
  (lambda (x y) (- y x)))
(reverse-subtract 7 10)                 ;===>  3

(define add4
  (let ((x 4))
    (lambda (y) (+ x y))))
(add4 6)                                ;===>  10

((lambda x x)
  3 4 5 6)                              ;===>  (3 4 5 6)

((lambda (x y . z) z)
  3 4 5 6)                              ;===>  (5 6)
~~~

### if
#### syntax:  (if test consequent alternate)
#### syntax:  (if test consequent)
~~~
(if (> 3 2) 'yes 'no)                   ;===>  yes
(if (> 2 3) 'yes 'no)                   ;===>  no
(if (> 3 2)
    (- 3 2)
    (+ 3 2))                            ;===>  1
~~~

### set!
#### syntax:  (set! variable expression)
~~~
(define x 2)
(+ x 1)                                 ;===>  3
(set! x 4)                              ;===>  4  ; unspecified
(+ x 1)                                 ;===>  5
~~~

## Derived expression types

### cond
#### syntax:  (cond clause1 clause2 ...)
~~~
(cond ((> 3 2) 'greater)
      ((< 3 2) 'less))                  ;===>  greater

(cond ((> 3 3) 'greater)
      ((< 3 3) 'less)
      (else 'equal))                    ;===>  equal

(cond ((assv 'b '((a 1) (b 2))) => cadr)
      (else #f))                        ;===>  2
~~~

### case
#### syntax:  (case key clause1 clause2 ...)
~~~
(case (* 2 3)
  ((2 3 5 7) 'prime)
  ((1 4 6 8 9) 'composite))             ;===>  composite

(case (car '(c d))
  ((a) 'a)
  ((b) 'b))                             ;===>  ()

(case (car '(c d))
  ((a e i o u) 'vowel)
  ((w y) 'semivowel)
  (else 'consonant))                    ;===>  consonant
~~~

### and
#### syntax:  (and test1 ...)

~~~
(and (= 2 2) (> 2 1))                   ;===>  #t
(and (= 2 2) (< 2 1))                   ;===>  #f
(and 1 2 'c '(f g))                     ;===>  (f g)
(and)                                   ;===>  #t
~~~

### or
#### syntax:  (or test1 ...)
~~~
(or (= 2 2) (> 2 1))                    ;===>  #t
(or (= 2 2) (< 2 1))                    ;===>  #t
(or #f #f #f)                           ;===>  #f
(or (memq 'b '(a b c)) (/ 3 0))         ;===>  (b c)
~~~

### let
#### syntax:  (let bindings body)
~~~
(let ((x 2) (y 3))
  (* x y))                              ;===>  6

(let ((x 2) (y 3))
  (let ((x 7)
        (z (+ x y)))
    (* z x)))                           ;===>  35
~~~

### let*
#### syntax:  (let* bindings body)
~~~
(let ((x 2) (y 3))
  (let* ((x 7)
         (z (+ x y)))
    (* z x)))                           ;===>  70
~~~

### letrec
#### syntax:  (letrec bindings body)
~~~
(letrec ((even?
          (lambda (n)
            (if (zero? n)
                #t
                (odd? (- n 1)))))
         (odd?
          (lambda (n)
            (if (zero? n)
                #f
                (even? (- n 1))))))
  (even? 88))                           ;===>  #t
~~~

### begin
#### syntax:  (begin expression1 expression2 ...)
~~~
(define x 0)
(begin (set! x 5) (+ x 1))              ;===>  6
~~~

### do
#### syntax:  (do ((variable1 init1 step1) ...) (test expression ...) command ...)
~~~
(do ((vec (make-vector 5)) (i 0 (+ i 1)))
  ((= i 5) vec)
  (vector-set! vec i i))                ;===>  #(0 1 2 3 4)

(let ((x '(1 3 5 7 9)))
  (do ((x x (cdr x)) (sum 0 (+ sum (car x))))
    ((null? x) sum)))                   ;===>  25
~~~

### delay
#### syntax:  (delay expression)
~~~
(define d (delay (+ 1 2)))
(force d)                               ;===>  3
~~~

### quasiquote
#### syntax: (quasiquote qqtemplate)
~~~
`(a (+ 1 2) 4)                          ;===>  (a (+ 1 2) 4)
`(a ,(+ 1 2) 4)                         ;===>  (a 3 4)
`(a ,(+ 1 2) ,@'(4 5 6) b)              ;===>  (a 3 4 5 6 b)
~~~

## Macros

### let-syntax
#### syntax:  (let-syntax bindings body)
~~~
(let-syntax ((when (syntax-rules ()
                     ((when test stmt1 stmt2 ...)
                      (if test
                          (begin stmt1
                                 stmt2 ...))))))
  (let ((if #t))
    (when if (set! if 'now))
    if))                                ;===>  now

(let ((x 'outer))
  (let-syntax ((m (syntax-rules () ((m) x))))
    (let ((x 'inner))
      (m))))                            ;===>  outer
~~~

### letrec-syntax
#### syntax:  (letrec-syntax bindings body)
~~~
(letrec-syntax
  ((my-or (syntax-rules ()
            ((my-or) #f)
            ((my-or e) e)
            ((my-or e1 e2 ...)
             (let ((temp e1))
               (if temp
                   temp
                   (my-or e2 ...)))))))
  (let ((x #f)
        (y 7)
        (temp 8)
        (let odd?)
        (if even?))
    (my-or x
           (let temp)
           (if y)
           y)))                         ;===>  7
~~~

### syntax-rules
#### transformerspec  (syntax-rules literals syntaxrule ...)
#### literals  (identifier ...)
#### syntaxrule  (pattern template)

## Definitions

### define
#### syntax:  (define variable expression)
#### syntax:  (define (variable formals) body)
#### syntax:  (define (variable . formal) body)
~~~
(define pi 3.14)
pi                                      ;===>  3.14

(define first car)
(first '(1 2))                          ;===>  1

(define add1 (lambda (x) (+ x 1)))
(add1 4)                                ;===>  5

(define (add2 x) (+ x 2))
(add2 4)                                ;===>  6

(define (allsum . x) (apply + x))
(allsum 1 2 3 4)                        ;===>  10

(let ((x 5))
  (define foo (lambda (y) (bar x y)))
  (define bar (lambda (a b) (+ (* a b) a)))
  (foo (+ x 3)))                        ;===>  45
~~~

### define-syntax
#### syntax:  (define-syntax keyword transformerspec)
~~~
(define-syntax my-if
  (syntax-rules (then else)
    ((_ x then y else z) (if x y z))
    ((_ x then y) (if x y #f))
    ((_ x else y) (if x #f y))))

(my-if #t then 'OK else 'NG)            ;===>  OK
(my-if #f then 'OK else 'NG)            ;===>  NG
(my-if #t then 'OK)                     ;===>  OK
(my-if #f else 'NG)                     ;===>  NG
~~~

## Equivalence predicates

### eqv?
#### procedure:  (eqv? obj1 obj2)
~~~
(eqv? 'a 'a)                            ;===>  #t
(eqv? 'a 'b)                            ;===>  #f
(eqv? 2 2)                              ;===>  #t
(eqv? '() '())                          ;===>  #t
(eqv? 100000000 100000000)              ;===>  #t
(eqv? (cons 1 2) (cons 1 2))            ;===>  #f
(eqv? (lambda () 1) (lambda () 2))      ;===>  #f
(eqv? #f 'nil)                          ;===>  #f
(let ((p (lambda (x) x))) (eqv? p p))   ;===>  #t
(eqv? "" "")                            ;===>  #f  ; unspecified
(eqv? '#() '#())                        ;===>  #f  ; unspecified
(eqv? (lambda (x) x) (lambda (x) x))    ;===>  #f  ; unspecified
(eqv? (lambda (x) x) (lambda (y) y))    ;===>  #f  ; unspecified
~~~

### eq?
#### procedure:  (eq? obj1 obj2)
~~~
(eq? 'a 'a)                             ;===>  #t
(eq? '(a) '(a))                         ;===>  #f  ; unspecified
(eq? (list 'a) (list 'a))               ;===>  #f
(eq? "a" "a")                           ;===>  #f  ; unspecified
(eq? "" "")                             ;===>  #f  ; unspecified
(eq? '() '())                           ;===>  #t
(eq? 2 2)                               ;===>  #f  ; unspecified
(eq? #\A #\A)                           ;===>  #f  ; unspecified
(eq? car car)                           ;===>  #t
(let ((n (+ 2 3))) (eq? n n))           ;===>  #t  ; unspecified
(let ((x '(a))) (eq? x x))              ;===>  #t
(let ((x '#())) (eq? x x))              ;===>  #t
(let ((p (lambda (x) x))) (eq? p p))    ;===>  #t
~~~

### equal?
#### procedure:  (equal? obj1 obj2)
~~~
(equal? 'a 'a)                          ;===>  #t
(equal? '(a) '(a))                      ;===>  #t
(equal? '(a (b) c) '(a (b) c))          ;===>  #t
(equal? "abc" "abc")                    ;===>  #t
(equal? 2 2)                            ;===>  #t
(equal? (make-vector 5 'a)
        (make-vector 5 'a))             ;===>  #t
(equal? (lambda (x) x) (lambda (y) y))  ;===>  #f  ; unspecified
~~~

## Numbers

### number?
#### procedure:  (number? obj)
~~~
(number? 3)                             ;===>  #t
(number? 3.0)                           ;===>  #t
~~~

### complex?
#### not supported

### real?
#### procedure:  (real? obj)
~~~
(real? 3)                               ;===>  #t
(real? 1e-10)                           ;===>  #t
~~~

### rational?
#### not supported

### integer?
#### procedure:  (integer? obj)
~~~
(integer? 0)                            ;===>  #t
(integer? 0.0)                          ;===>  #t
(integer? 0.5)                          ;===>  #f
~~~

### exact?
#### procedure:  (exact->inexact z)
~~~
(exact? 3)                              ;===>  #t
(exact? 3.0)                            ;===>  #f
~~~

### inexact?
#### procedure:  (inexact? z)
~~~
(inexact? 3.0)                          ;===>  #t
(inexact? 3)                            ;===>  #f
~~~

### =
#### procedure:  (= z1 z2 z3 ...)
~~~
(= 3 3)                                 ;===>  #t
(= 3 4)                                 ;===>  #f
(= 3 3 3)                               ;===>  #t
~~~

### <
#### procedure:  (< x1 x2 x3 ...)
~~~
(< 3 3)                                 ;===>  #f
(< 3 4)                                 ;===>  #t
(< 3 4 5)                               ;===>  #t
~~~

### >
#### procedure:  (> x1 x2 x3 ...)
~~~
(> 3 3)                                 ;===>  #f
(> 3 4)                                 ;===>  #f
(> 3 2 1)                               ;===>  #t
~~~

### <=
#### procedure:  (<= x1 x2 x3 ...)
~~~
(<= 3 3)                                ;===>  #t
(<= 3 4)                                ;===>  #t
(<= 3 3 4)                              ;===>  #t
~~~

### >=
#### procedure:  (>= x1 x2 x3 ...)
~~~
(>= 3 3)                                ;===>  #t
(>= 3 4)                                ;===>  #f
(>= 3 3 2)                              ;===>  #t
~~~

### zero?
#### procedure:  (zero? z)
~~~
(zero? 0)                               ;===>  #t
(zero? 0.0)                             ;===>  #t
~~~

### positive?
#### procedure:  (positive? x)
~~~
(positive? 0)                           ;===>  #f
(positive? 1)                           ;===>  #t
~~~

### negative?
#### procedure:  (negative? x)
~~~
(negative? -1)                          ;===>  #t
(negative? 0)                           ;===>  #f
~~~

### odd?
#### procedure:  (odd? n)
~~~
(odd? 0)                                ;===>  #f
(odd? 1)                                ;===>  #t
~~~

### even?
#### procedure:  (even? n)
~~~
(even? 0)                               ;===>  #t
(even? 1)                               ;===>  #f
~~~

### max
#### procedure:  (max x1 x2 ...)
~~~
(max 3 4)                               ;===>  4    ; exact
(max 3.9 4)                             ;===>  4.0  ; inexact
~~~

### min
#### procedure:  (min x1 x2 ...)
~~~
(min 3 4)                               ;===>  3    ; exact
(min 3 3.1)                             ;===>  3.0  ; inexact
~~~

### +
#### procedure:  (+ z1 ...)
~~~
(+ 3 4)                                 ;===>  7
(+ 3)                                   ;===>  3
(+)                                     ;===>  0
~~~

### *
#### procedure:  (* z1 ...)
~~~
(* 4)                                   ;===>  4
(*)                                     ;===>  1
~~~

### -
#### procedure:  (- z1 z2 ...)
~~~
(- 3 4)                                 ;===>  -1
(- 3 4 5)                               ;===>  -6
(- 3)                                   ;===>  -3
~~~

### /
#### procedure:  (/ z1 z2 ...)
~~~
(/ 3 4 5)                               ;===>  0.15
(/ 3)                                   ;===>  0.3333333333
~~~

### abs
#### procedure:  (abs x)
~~~
(abs -7)                                ;===>  7
~~~

### quotient
#### procedure:  (quotient n1 n2)
~~~
(quotient 8 4)                          ;===>  2
(quotient 7 4)                          ;===>  1
(quotient -3 4)                         ;===>  0
~~~

### remainder
#### procedure:  (remainder n1 n2)
~~~
(remainder 8 4)                         ;===>  0
(remainder 7 4)                         ;===>  3
(remainder -3 4)                        ;===>  -3
~~~

### modulo
#### procedure:  (modulo n1 n2)
~~~
(modulo 8 4)                            ;===>  0
(modulo 7 4)                            ;===>  3
(modulo -3 4)                           ;===>  1
~~~

### gcd
#### procedure:  (gcd n1 ...)
~~~
(gcd 32 -36)                            ;===>  4
(gcd)                                   ;===>  0
~~~

### lcm
#### procedure:  (lcm n1 ...)
~~~
(lcm 32 -36)                            ;===>  288
(lcm 32.0 -36)                          ;===>  288.0  ; inexact
(lcm)                                   ;===>  1
~~~

### numerator
#### not supported

### denominator
#### not supported

### floor
#### procedure:  (floor x)
~~~
(floor -4.3)                            ;===>  -5.0
(floor 3.5)                             ;===>  3.0
~~~

### ceiling
#### procedure:  (ceiling x)
~~~
(ceiling -4.3)                          ;===>  -4.0
(ceiling 3.5)                           ;===>  4.0
~~~

### truncate
#### procedure:  (truncate x)
~~~
(truncate -4.3)                         ;===>  -4.0
(truncate 3.5)                          ;===>  3.0
~~~

### round
#### procedure:  (round x)
~~~
(round -4.3)                            ;===>  -4.0
(round 3.5)                             ;===>  4.0
(round 7)                               ;===>  7
~~~

### rationalize
#### not supported

### exp
#### procedure:  (exp z)
~~~
(exp 1)                                 ;===>  2.718281828
(exp 0)                                 ;===>  1
~~~

### log
#### procedure:  (log z)
~~~
(log 1)                                 ;===>  0.0
(log (exp 1))                           ;===>  1.0
~~~

### sin
#### procedure:  (sin z)
~~~
(sin 0)                                 ;===>  0.0
(sin 1.570796327)                       ;===>  1.0
~~~

### cos
#### procedure:  (cos z)
~~~
(cos 0)                                 ;===>  1.0
(cos 3.141592654)                       ;===>  -1.0
~~~

### tan
#### procedure:  (tan z)
~~~
(tan 0)                                 ;===>  0.0
(tan 0.7853981634)                      ;===>  1.0
~~~

### asin
#### procedure:  (asin z)
~~~
(asin 1)                                ;===>  1.570796327
(asin 0)                                ;===>  0.0
(asin -1)                               ;===>  -1.570796327
~~~

### acos
#### procedure:  (acos z)
~~~
(acos 1)                                ;===>  0.0
(acos 0)                                ;===>  1.570796327
(acos -1)                               ;===>  3.141592654
~~~

### atan
#### procedure:  (atan z)
#### procedure:  (atan y x)
~~~
(atan 1)                                ;===>  0.7853981634
(asin 0)                                ;===>  0.0
(atan -1)                               ;===>  -0.7853981634
~~~

### sqrt
#### procedure:  (sqrt z)
~~~
(sqrt 2)                                ;===>  1.414213562
(sqrt 3)                                ;===>  1.732050808
(sqrt 4)                                ;===>  2.0
~~~

### expt
#### procedure:  (expt z1 z2)
~~~
(expt 2 10)                             ;===>  1024
(expt 0 1)                              ;===>  0
(expt 0 0)                              ;===>  1
~~~

### make-rectangular
#### not supported

### make-polar
#### not supported

### real-part
#### not supported

### imag-part
#### not supported

### magnitude
#### not supported

### angle
#### not supported

### exact->inexact
#### procedure:  (exact->inexact z)
~~~
(exact->inexact 3)                      ;===>  3.0
~~~

### inexact->exact
#### procedure:  (inexact->exact z)
~~~
(inexact->exact 3.0)                    ;===>  3
~~~

### number->string
#### procedure:  (number->string z)
#### procedure:  (number->string z radix)
~~~
(number->string 15)                     ;===>  "15"
(number->string 15 2)                   ;===>  "1111"
(number->string 15 8)                   ;===>  "17"
(number->string 15 10)                  ;===>  "15"
(number->string 15 16)                  ;===>  "f"
~~~

### string->number
#### procedure:  (string->number string)
#### procedure:  (string->number string radix)
~~~
(string->number "100")                  ;===>  100
(string->number "100" 2)                ;===>  4
(string->number "100" 8)                ;===>  64
(string->number "100" 10)               ;===>  100
(string->number "100" 16)               ;===>  256
(string->number "1e2")                  ;===>  100.0
~~~

## Other data types

### not
#### procedure:  (not obj)
~~~
(not #t)                                ;===>  #f
(not 3)                                 ;===>  #f
(not (list 3))                          ;===>  #f
(not #f)                                ;===>  #t
(not '())                               ;===>  #f
(not (list))                            ;===>  #f
(not 'nil)                              ;===>  #f
~~~

### boolean?
#### procedure:  (boolean? z)
~~~
(boolean? #t)                           ;===>  #t
(boolean? #f)                           ;===>  #t
(boolean? 0)                            ;===>  #f
(boolean? '())                          ;===>  #f
~~~

### pair?
#### procedure:  (pair? obj)
~~~
(pair? '(a . b))                        ;===>  #t
(pair? '(a b c))                        ;===>  #t
(pair? '())                             ;===>  #f
(pair? '#(a b))                         ;===>  #f
~~~

### cons
#### procedure:  (cons obj1 obj2)
~~~
(cons 'a '())                           ;===>  (a)
(cons '(a) '(b c d))                    ;===>  ((a) b c d)
(cons "a" '(b c))                       ;===>  ("a" b c)
(cons 'a 3)                             ;===>  (a . 3)
(cons '(a b) 'c)                        ;===>  ((a b) . c)
~~~

### car
#### procedure:  (car pair)
~~~
(car '(1 . 2))                          ;===>  1
~~~

### cdr
#### procedure:  (cdr pair)
~~~
(cdr '(1 . 2))                          ;===>  2
~~~

### set-car!
#### procedure:  (set-car! pair obj)
~~~
(define p '(1 . 2))
(set-car! p 3)
p                                       ;===>  (3 . 2)
~~~

### set-cdr!
#### procedure:  (set-cdr! pair obj)
~~~
(define p '(1 . 2))
(set-cdr! p 3)
p                                       ;===>  (1 . 3)
~~~

### caar
### cdar
### cadr
### cddr
### caaar
### cdaar
### cadar
### cddar
### caadr
### cdadr
### caddr
### cdddr
### caaaar
### cdaaar
### cadaar
### cddaar
### caadar
### cdadar
### caddar
### cdddar
### caaadr
### cdaadr
### cadadr
### cddadr
### caaddr
### cdaddr
### cadddr
### cddddr
#### procedure:  (caar pair)
#### procedure:  (cdar pair)
#### procedure:  (cadr pair)
#### procedure:  (cddr pair)
#### procedure:  (caaar pair)
#### procedure:  (cdaar pair)
#### procedure:  (cadar pair)
#### procedure:  (cddar pair)
#### procedure:  (caadr pair)
#### procedure:  (cdadr pair)
#### procedure:  (caddr pair)
#### procedure:  (cdddr pair)
#### procedure:  (caaaar pair)
#### procedure:  (cdaaar pair)
#### procedure:  (cadaar pair)
#### procedure:  (cddaar pair)
#### procedure:  (caadar pair)
#### procedure:  (cdadar pair)
#### procedure:  (caddar pair)
#### procedure:  (cdddar pair)
#### procedure:  (caaadr pair)
#### procedure:  (cdaadr pair)
#### procedure:  (cadadr pair)
#### procedure:  (cddadr pair)
#### procedure:  (caaddr pair)
#### procedure:  (cdaddr pair)
#### procedure:  (cadddr pair)
#### procedure:  (cddddr pair)
~~~
(caar '((1 . 2) . 3))                   ;===>  1
(cdar '((1 . 2) . 3))                   ;===>  2
(cadr '(1 . (2 . 3)))                   ;===>  2
(cddr '(1 . (2 . 3)))                   ;===>  3
(caaar '(((1 . 2) . 3) . 4))            ;===>  1
(cdaar '(((1 . 2) . 3) . 4))            ;===>  2
(cadar '((1 . (2 . 3)) . 4))            ;===>  2
(cddar '((1 . (2 . 3)) . 4))            ;===>  3
(caadr '(1 . ((2 . 3) . 4)))            ;===>  2
(cdadr '(1 . ((2 . 3) . 4)))            ;===>  3
(caddr '(1 . (2 . (3 . 4))))            ;===>  3
(cdddr '(1 . (2 . (3 . 4))))            ;===>  4
(caaaar '((((1 . 2) . 3) . 4) . 5))     ;===>  1
(cdaaar '((((1 . 2) . 3) . 4) . 5))     ;===>  2
(cadaar '(((1 . (2 . 3)) . 4) . 5))     ;===>  2
(cddaar '(((1 . (2 . 3)) . 4) . 5))     ;===>  3
(caadar '((1 . ((2 . 3) . 4)) . 5))     ;===>  2
(cdadar '((1 . ((2 . 3) . 4)) . 5))     ;===>  3
(caddar '((1 . (2 . (3 . 4))) . 5))     ;===>  3
(cdddar '((1 . (2 . (3 . 4))) . 5))     ;===>  4
(caaadr '(1 . (((2 . 3) . 4) . 5)))     ;===>  2
(cdaadr '(1 . (((2 . 3) . 4) . 5)))     ;===>  3
(cadadr '(1 . ((2 . (3 . 4)) . 5)))     ;===>  3
(cddadr '(1 . ((2 . (3 . 4)) . 5)))     ;===>  4
(caaddr '(1 . (2 . ((3 . 4) . 5))))     ;===>  3
(cdaddr '(1 . (2 . ((3 . 4) . 5))))     ;===>  4
(cadddr '(1 . (2 . (3 . (4 . 5)))))     ;===>  4
(cddddr '(1 . (2 . (3 . (4 . 5)))))     ;===>  5
~~~

### null?
#### procedure:  (null? obj)
~~~
(null? '())                             ;===>  #t
(null? 0)                               ;===>  #f
~~~

### list?
#### procedure:  (list? obj)
~~~
(list? '(a b c))                        ;===>  #t
(list? '())                             ;===>  #t
(list? '(a . b))                        ;===>  #f
(let ((x (list 'a)))
  (set-cdr! x x) (list? x))             ;===>  #f
~~~

### list
#### procedure:  (list obj ...)
~~~
(list 'a (+ 3 4) 'c)                    ;===>  (a 7 c)
(list)                                  ;===>  ()
~~~

### length
#### procedure:  (length list)
~~~
(length '(a b c))                       ;===>  3
(length '(a (b) (c d e)))               ;===>  3
(length '())                            ;===>  0
~~~

### append
#### procedure:  (append list1 ...)
~~~
(append '(x) '(y))                      ;===>  (x y)
(append '(a) '(b c d))                  ;===>  (a b c d)
(append '(a (b)) '((c)))                ;===>  (a (b) (c))
(append '(a b) '(c . d))                ;===>  (a b c . d)
(append '() 'a)                         ;===>  a
~~~

### reverse
#### procedure:  (reverse list)
~~~
(reverse '(a b c))                      ;===>  (c b a)
(reverse '(a (b c) d (e (f))))          ;===>  ((e (f)) d (b c) a)
~~~

### list-tail
#### procedure:  (list-tail list k)
~~~
(list-tail '(a b c d) 0)                ;===>  (a b c d)
(list-tail '(a b c d) 1)                ;===>  (b c d)
(list-tail '(a b c d) 2)                ;===>  (c d)
(list-tail '(a b c d) 3)                ;===>  (d)
(list-tail '(a b c d)
  (inexact->exact (round 1.8)))         ;===>  (c d)
~~~

### list-ref
#### procedure:  (list-ref list k)
~~~
(list-ref '(a b c d) 0)                 ;===>  a
(list-ref '(a b c d) 1)                 ;===>  b
(list-ref '(a b c d) 2)                 ;===>  c
(list-ref '(a b c d) 3)                 ;===>  d
(list-ref '(a b c d)
  (inexact->exact (round 1.8)))         ;===>  c
~~~

### memq
#### procedure:  (memq obj list)
~~~
(memq 'a '(a b c))                      ;===>  (a b c)
(memq 'b '(a b c))                      ;===>  (b c)
(memq 'a '(b c d))                      ;===>  #f
(memq (list 'a) '(b (a) c))             ;===>  #f
(memq 101 '(100 101 102))               ;===>  #f  ; unspecified
~~~

### memv
#### procedure:  (memv obj list)
~~~
(memv 101 '(100 101 102))               ;===>  (101 102)
~~~

### member
#### procedure:  (member obj list)
~~~
(member (list 'a) '(b (a) c))           ;===>  ((a) c)
~~~

### assq
#### procedure:  (assq obj alist)
~~~
(assq 'a '((a 1) (b 2) (c 3)))          ;===>  (a 1)
(assq 'b '((a 1) (b 2) (c 3)))          ;===>  (b 2)
(assq 'd '((a 1) (b 2) (c 3)))          ;===>  #f
(assq (list 'a) '(((a)) ((b)) ((c))))   ;===>  #f
(assq 5 '((2 3) (5 7) (11 13)))         ;===>  #f
~~~

### assv
#### procedure:  (assv obj alist)
~~~
(assv 'a '((a 1) (b 2) (c 3)))          ;===>  (a 1)
(assv 'b '((a 1) (b 2) (c 3)))          ;===>  (b 2)
(assv 'd '((a 1) (b 2) (c 3)))          ;===>  #f
(assv (list 'a) '(((a)) ((b)) ((c))))   ;===>  #f
(assv 5 '((2 3) (5 7) (11 13)))         ;===>  (5 7)
~~~

### assoc
#### procedure:  (assoc obj alist)
~~~
(assoc 'a '((a 1) (b 2) (c 3)))         ;===>  (a 1)
(assoc 'b '((a 1) (b 2) (c 3)))         ;===>  (b 2)
(assoc 'd '((a 1) (b 2) (c 3)))         ;===>  #f
(assoc (list 'a) '(((a)) ((b)) ((c))))  ;===>  ((a))
(assoc 5 '((2 3) (5 7) (11 13)))        ;===>  (5 7)
~~~

### symbol?
#### procedure:  (substring string start end)
~~~
(symbol? 'foo)                          ;===>  #t
(symbol? (car '(a b)))                  ;===>  #t
(symbol? "bar")                         ;===>  #f
(symbol? 'nil)                          ;===>  #t
(symbol? '())                           ;===>  #f
(symbol? #f)                            ;===>  #f
~~~

### symbol->string
#### procedure:  (substring string start end)
~~~
(symbol->string 'abc)                   ;===>  "abc"
(symbol->string 'ABC)                   ;===>  "ABC"
~~~

### string->symbol
#### procedure:  (string->symbol string)
~~~
(string->symbol "ABC")                  ;===>  ABC
(string->symbol "abc")                  ;===>  abc
~~~

## Characters

### char?
#### procedure:  (char? obj)
~~~
(char? #\A)                             ;===>  #t
(char? "A")                             ;===>  #f
~~~

### char=?
#### procedure:  (char=? char1 char2)
~~~
(char=? #\A #\B)                        ;===>  #f
(char=? #\A #\a)                        ;===>  #f
~~~

### char<?
#### procedure:  (char<? char1 char2)
~~~
(char<? #\A #\B)                        ;===>  #t
(char<? #\A #\a)                        ;===>  #t
~~~

### char>?
#### procedure:  (char>? char1 char2)
~~~
(char>? #\A #\B)                        ;===>  #f
(char>? #\A #\a)                        ;===>  #f
~~~

### char<=?
#### procedure:  (char<=? char1 char2)
~~~
(char<=? #\A #\B)                       ;===>  #t
(char<=? #\A #\a)                       ;===>  #t
~~~

### char>=?
#### procedure:  (char>=? char1 char2)
~~~
(char>=? #\A #\B)                       ;===>  #f
(char>=? #\A #\a)                       ;===>  #f
~~~

### char-ci=?
#### procedure:  (char-ci=? char1 char2)
~~~
(char-ci=? #\A #\B)                     ;===>  #f
(char-ci=? #\A #\a)                     ;===>  #t
~~~

### char-ci<?
#### procedure:  (char-ci<? char1 char2)
~~~
(char-ci<? #\A #\B)                     ;===>  #t
(char-ci<? #\A #\a)                     ;===>  #f
~~~

### char-ci>?
#### procedure:  (char-ci>? char1 char2)
~~~
(char-ci>? #\A #\B)                     ;===>  #f
(char-ci>? #\A #\a)                     ;===>  #f
~~~

### char-ci<=?
#### procedure:  (char-ci<=? char1 char2)
~~~
(char-ci<=? #\A #\B)                    ;===>  #t
(char-ci<=? #\A #\a)                    ;===>  #t
~~~

### char-ci>=?
#### procedure:  (char-ci>=? char1 char2)
~~~
(char-ci>=? #\A #\B)                    ;===>  #f
(char-ci>=? #\A #\a)                    ;===>  #t
~~~

### char-alphabetic?
#### procedure:  (char-alphabetic? char)
~~~
(char-alphabetic? #\A)                  ;===>  #t
(char-alphabetic? #\z)                  ;===>  #t
(char-alphabetic? #\0)                  ;===>  #f
~~~

### char-numeric?
#### procedure:  (char-numeric? char)
~~~
(char-numeric? #\A)                     ;===>  #f
(char-numeric? #\z)                     ;===>  #f
(char-numeric? #\0)                     ;===>  #t
~~~

### char-whitespace?
#### procedure:  (char-whitespace? char)
~~~
(char-whitespace? #\A)                  ;===>  #f
(char-whitespace? #\z)                  ;===>  #f
(char-whitespace? #\space)              ;===>  #t
~~~

### char-upper-case?
#### procedure:  (char-upper-case? char)
~~~
(char-upper-case? #\A)                  ;===>  #t
(char-upper-case? #\z)                  ;===>  #f
(char-upper-case? #\0)                  ;===>  #f
~~~

### char-lower-case?
#### procedure:  (char-lower-case? char)
~~~
(char-lower-case? #\A)                  ;===>  #f
(char-lower-case? #\z)                  ;===>  #t
(char-lower-case? #\0)                  ;===>  #f
~~~

### char->integer
#### procedure:  (char->integer char)
~~~
(char->integer #\A)                     ;===>  65
(<= (char->integer #\a)
    (char->integer #\b))                ;===>  #t
~~~

### integer->char
#### procedure:  (integer->char n)
~~~
(integer->char 48)                      ;===>  #\0
(integer->char 65)                      ;===>  #\A
~~~

### char-upcase
#### procedure:  (char-upcase char)
~~~
(char-upcase #\A)                       ;===>  #\A
(char-upcase #\a)                       ;===>  #\A
~~~

### char-downcase
#### procedure:  (char-downcase char)
~~~
(char-downcase #\A)                     ;===>  #\a
(char-downcase #\a)                     ;===>  #\a
~~~

## Strings

### string?
#### procedure:  procedure:  (string? obj)
~~~
(string? "A")                           ;===>  #t
(string? #\A)                           ;===>  #f
~~~

### make-string
#### procedure:  (make-string k)
#### procedure:  (make-string k char)
~~~
(make-string 3)                         ;===>  "   "
(make-string 3 #\*)                     ;===>  "***"
~~~

### string
#### procedure:  (string char ...)
~~~
(string)                                ;===>  ""
(string #\A)                            ;===>  "A"
(string #\A #\B)                        ;===>  "AB"
~~~

### string-length
#### procedure:  (string-length string)
~~~
(string-length "")                      ;===>  0
(string-length "abc")                   ;===>  3
~~~

### string-ref
#### procedure:  (string-ref string k)
~~~
(string-ref "abc" 0)                    ;===>  #\a
(string-ref "abc" 2)                    ;===>  #\c
~~~

### string-set!
#### procedure:  (string-set! string k char)
~~~
(define s "abc")
(string-set! s 2 #\d)                   ;===>  "abd"  ; unspecified
s                                       ;===>  "abd"
~~~

### string=?
#### procedure:  (string=? string1 string2)
~~~
(string=? "A" "A")                      ;===>  #t
(string=? "A" "B")                      ;===>  #f
(string=? "A" "AB")                     ;===>  #f
~~~

### string-ci=?
#### procedure:  (string-ci=? string1 string2)
~~~
(string-ci=? "a" "A")                   ;===>  #t
(string-ci=? "a" "B")                   ;===>  #f
(string-ci=? "a" "AB")                  ;===>  #f
~~~

### string<?
#### procedure:  (string<? string1 string2)
~~~
(string<? "A" "A")                      ;===>  #f
(string<? "A" "B")                      ;===>  #t
(string<? "A" "AB")                     ;===>  #t
~~~

### string>?
#### procedure:  (string>? string1 string2)
~~~
(string>? "A" "A")                      ;===>  #f
(string>? "A" "B")                      ;===>  #f
(string>? "A" "AB")                     ;===>  #f
~~~

### string<=?
#### procedure:  (string<=? string1 string2)
~~~
(string<=? "A" "A")                     ;===>  #t
(string<=? "A" "B")                     ;===>  #t
(string<=? "A" "AB")                    ;===>  #t
~~~

### string>=?
#### procedure:  (string>=? string1 string2)
~~~
(string>=? "A" "A")                     ;===>  #t
(string>=? "A" "B")                     ;===>  #f
(string>=? "A" "AB")                    ;===>  #f
~~~

### string-ci<?
#### procedure:  (string-ci<? string1 string2)
~~~
(string-ci<? "a" "A")                   ;===>  #f
(string-ci<? "a" "B")                   ;===>  #t
(string-ci<? "a" "AB")                  ;===>  #t
~~~

### string-ci>?
#### procedure:  (string-ci>? string1 string2)
~~~
(string-ci>? "a" "A")                   ;===>  #f
(string-ci>? "a" "B")                   ;===>  #f
(string-ci>? "a" "AB")                  ;===>  #f
~~~

### string-ci<=?
#### procedure:  (string-ci<=? string1 string2)
~~~
(string-ci<=? "a" "A")                  ;===>  #t
(string-ci<=? "a" "B")                  ;===>  #t
(string-ci<=? "a" "AB")                 ;===>  #t
~~~

### string-ci>=?
#### procedure:  (string-ci>=? string1 string2)
~~~
(string-ci>=? "a" "A")                  ;===>  #t
(string-ci>=? "a" "B")                  ;===>  #f
(string-ci>=? "a" "AB")                 ;===>  #f
~~~

### substring
#### procedure:  (substring string start end)
~~~
(substring "abc" 0 1)                   ;===>  "a"
(substring "abc" 1 2)                   ;===>  "b"
(substring "abc" 1 3)                   ;===>  "bc"
~~~

### string-append
#### procedure:  (string-append string ...)
~~~
(string-append "a")                     ;===>  "a"
(string-append "a" "b" "c")             ;===>  "abc"
~~~

### string->list
#### procedure:  (string->list string)
~~~
(string->list "")                       ;===>  ()
(string->list "A")                      ;===>  (#\A)
(string->list "AB")                     ;===>  (#\A #\B)
~~~

### list->string
#### procedure:  (list->string list)
~~~
(list->string '(#\a #\b #\c))           ;===>  "abc"
~~~

### string-copy
#### procedure:  (string-copy string)
~~~
(string-copy "abc")                     ;===>  "abc"
~~~

### string-fill!
#### procedure:  (string-fill! string char)
~~~
(define s "abc")
(string-fill! s #\d)                    ;===>  "ddd"  ; unspecified
s                                       ;===>  "ddd"
~~~

## Vectors

### vector?
#### procedure:  (vector? obj)
~~~
(vector? '#(a b c))                     ;===>  #t
(vector? '(a b c))                      ;===>  #f
~~~

### make-vector
#### procedure:  (make-vector k)
#### procedure:  (make-vector k fill)
~~~
(make-vector 3)                         ;===>  #(() () ())
(make-vector 3 'a)                      ;===>  #(a a a)
~~~

### vector
#### procedure:  (vector obj ...)
~~~
(vector 'a 'b 'c)                       ;===>  #(a b c)
~~~

### vector-length
#### procedure:  (vector-length vector)
~~~
(vector-length '#(a b c))               ;===>  3
~~~

### vector-ref
#### procedure:  (vector-ref vector k)
~~~
(vector-ref '#(a b c) 0)                ;===>  a
(vector-ref '#(a b c) 2)                ;===>  c
~~~

### vector-set!
#### procedure:  (vector-set! vector k obj)
~~~
(define v '#(a b c))
(vector-set! v 2 'd)
v                                       ;===>  #(a b d)
~~~

### vector->list
#### procedure:  (vector->list vector)
~~~
(vector->list '#(a b c))                ;===>  (a b c)
~~~

### list->vector
#### procedure:  (list->vector list)
~~~
(list->vector '(a b))                   ;===>  #(a b)
~~~

### vector-fill!
#### procedure:  (vector-fill! vector fill)
~~~
(define v '#(a b c))
(vector-fill! v 'd)                     ;===>  #(d d d)  ; unspecified
v                                       ;===>  #(d d d)
~~~

## Control features

### procedure?
#### procedure:  (procedure? obj)
~~~
(procedure? car)                        ;===>  #t
(procedure? 'car)                       ;===>  #f
(procedure? (lambda (x) (* x x)))       ;===>  #t
(procedure? '(lambda (x) (* x x)))      ;===>  #f
(call-with-current-continuation procedure?) ;===>  #t
~~~

### apply
#### procedure:  (apply proc arg1 ... args)
~~~
(apply + (list 3 4))                    ;===>  7
(apply + 3 (list 4))                    ;===>  7
~~~

### map
#### procedure:  (map proc list1 list2 ...)
~~~
(map cadr '((a b) (d e) (g h)))         ;===>  (b e h)
(map (lambda (n) (expt n n)) '(1 2 3 4 5)) ;===>  (1 4 27 256 3125)
(map + '(1 2 3) '(4 5 6))               ;===>  (5 7 9)
(let ((count 0))
  (map (lambda (ignored) (set! count (+ count 1)) count)
       '(a b)))                         ;===>  (1 2) or (2 1)
~~~

### for-each
#### procedure:  (for-each proc list1 list2 ...)
~~~
(let ((v (make-vector 5)))
  (for-each
    (lambda (i) (vector-set! v i (* i i)))
    '(0 1 2 3 4))
  v)                                    ;===>  #(0 1 4 9 16)
~~~

### force
#### procedure:  (force promise)
~~~
(force (delay (+ 1 2)))                 ;===>  3
(let ((p (delay (+ 1 2))))
  (list (force p) (force p)))           ;===>  (3 3)

(define a-stream
  (letrec ((next
            (lambda (n)
              (cons n (delay (next (+ n 1)))))))
    (next 0)))
(define head car)
(define tail
  (lambda (stream) (force (cdr stream))))

(head (tail (tail a-stream)))           ;===>  2

(define count 0)
(define p
  (delay (begin (set! count (+ count 1))
                (if (> count x)
                    count
                    (force p)))))
(define x 5)
p                                       ;===>  #<PROMISE>
(force p)                               ;===>  6
p                                       ;===>  #<PROMISE (FORCED)>
(begin (set! x 10) (force p))           ;===>  6
~~~

### call-with-current-continuation
#### procedure:  (call-with-current-continuation proc)
~~~
(call-with-current-continuation
  (lambda (exit)
    (for-each (lambda (x)
                (if (negative? x)
                    (exit x)))
              '(54 0 37 -3 245 19))
    #t))                                ;===>  -3

(define list-length
  (lambda (obj)
    (call-with-current-continuation
      (lambda (return)
        (letrec ((r
                  (lambda (obj)
                    (cond ((null? obj) 0)
                          ((pair? obj)
                           (+ (r (cdr obj)) 1))
                          (else (return #f))))))
          (r obj))))))

(list-length '(1 2 3 4))                ;===>  4

(list-length '(a b . c))                ;===>  #f
~~~

### values
#### procedure:  (values obj ...)

### call-with-values
#### procedure:  (call-with-values producer consumer)
~~~
(call-with-values
  (lambda () (values 4 5))
  (lambda (a b) b))                     ;===>  5

(call-with-values * -)                  ;===>  -1
~~~

### dynamic-wind
#### procedure:  (dynamic-wind before thunk after)
~~~
(let ((path '())
      (c #f))
  (let ((add (lambda (s)
               (set! path (cons s path)))))
    (dynamic-wind
      (lambda () (add 'connect))
      (lambda ()
        (add (call-with-current-continuation
               (lambda (c0)
                 (set! c c0)
                 'talk1))))
      (lambda () (add 'disconnect)))
    (if (< (length path) 4)
        (c 'talk2)
        (reverse path))))               ;===>  (connect talk1 disconnect connect talk2 disconnect)
~~~

## Eval

### eval
#### procedure:  (eval expression)
~~~
(eval '(* 7 3))                         ;===>  21
(let ((f (eval '(lambda (f x) (f x x)))))
  (f + 10))                             ;===>  20
~~~

### scheme-report-environment
#### not supported

### null-environment
#### not supported

### interaction-environment
#### procedure:  (interaction-environment)
~~~
(interaction-environment)
~~~

## Ports

### port?
#### procedure:  (port? obj)

### call-with-input-file
#### procedure:  (call-with-input-file string proc)
~~~
(call-with-input-file "hello.txt"
  (lambda (p)
    (let loop ((c (read-char p)))
      (if (eof-object? c)
        (close-input-port p)
        (begin (display c)
          (loop (read-char p)))))))
~~~

### call-with-output-file
#### procedure:  (call-with-output-file string proc)
~~~
(call-with-output-file "hello.txt"
  (lambda (p)
    (display "Hello world!" p)
    (newline p)
    (close-output-port p)))
~~~

### input-port?
#### procedure:  (input-port? obj)
~~~
(input-port? (current-input-port))      ;===>  #t
(input-port? (current-output-port))     ;===>  #f
~~~

### output-port?
#### procedure:  (output-port? obj)
~~~
(output-port? (current-input-port))     ;===>  #f
(output-port? (current-output-port))    ;===>  #t
~~~

### current-input-port
#### procedure:  (current-input-port)
~~~
(define p (current-input-port))
(let loop ((c (read-char p)))
  (if (eof-object? c)
    (close-input-port p)
    (begin (display c)
      (loop (read-char p)))))
~~~

### current-output-port
#### procedure:  (current-output-port)
~~~
(define p (current-output-port))
(display "Hello world!" p)
~~~

### with-input-from-file
#### procedure:  (with-input-from-file string thunk)

### with-output-to-file
#### procedure:  (with-output-to-file string thunk)

### open-input-file
#### procedure:  (open-input-file filename)
~~~
(define p (open-input-file "hello.txt"))
(let loop ((c (read-char p)))
  (if (eof-object? c)
    (close-input-port p)
    (begin (display c)
      (loop (read-char p)))))
~~~

### open-output-file
#### procedure:  (open-output-file filename)
~~~
(define p (open-output-file "hello.txt"))
(display "Hello world!" p)
(close-output-port p)
~~~

### close-input-port
#### procedure:  (close-input-port port)

### close-output-port
#### procedure:  (close-output-port port)

## Input

### read
#### procedure:  (read)
#### procedure:  (read port)

### read-char
#### procedure:  (read-char)
#### procedure:  (read-char port)

### peek-char?
#### procedure:  (peek-char)
#### procedure:  (peek-char port)

### eof-object?
#### procedure:  (eof-object? obj)

### char-ready?
#### procedure:  (char-ready?)
#### procedure:  (char-ready? port)
~~~
(char-ready?)                           ;===>  #t or #f
(char-ready? (current-input-port))      ;===>  #t or #f
~~~

## Output

### write
#### procedure:  (write obj)
#### procedure:  (write obj port)

### display
#### procedure:  (display obj)
#### procedure:  (display obj port)
~~~
(display "Hello world!\n")              ;===>  #t
~~~

### newline
#### procedure:  (newline)
#### procedure:  (newline port)
~~~
(newline)                               ;===>  #t
~~~

### write-char
#### procedure:  (write-char char)
#### procedure:  (write-char char port)
~~~
(write-char #\a)                        ;===>  #t
~~~

## System interface

### load
#### procedure:  (load filename)
~~~
(load "init.scm")
~~~

### transcript-on
#### not supported

### transcript-off
#### not supported
