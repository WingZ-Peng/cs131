; #lang racket

(define (is-keyword x)
  (member x '(quote lambda let if)))

(define (get-let-vars x)
  (map car (car (cdr x))))

(define (compare-expr x y)
  (cond 
    ;;; base case – sides identical
    ;; if the expressions are equal, just return one (doesn't matter which)
    [(equal? x y) x]
    ;;; base case – sides differ
    ;; special case: "TCP [represents] a subexpression that is #t in TCP's 
    ;; version and #f in UDP's version, and (not TCP)[represents] the reverse
    ;; situation"
    [(and (boolean? x) (boolean? y))
      (if x 'TCP '(not TCP))]
     ;; if not both lists (type mismatch), don't recurse
    [(or (not (and (list? x) (list? y)))
     ;; if lists of differing lengths, don't recurse
     (not (= (length x) (length y)))
     ;; if one expression is quoted, don't recurse
     (or (equal? (car x) 'quote) (equal? (car y) 'quote))
     ;; if both expressions are lambdas and their arguments are not equal, don't recurse
     (and (equal? (car x) 'lambda) (equal? (car y) 'lambda) (not (equal? (car (cdr x)) (car (cdr y)))))
     ;; if both expressions are `let` and the variable names are different, don't recurse
     (and (equal? (car x) 'let) (equal? (car y) 'let) (not (equal? (get-let-vars x) (get-let-vars y))))
     ;; if one expression begins with a special keyword that the other doesn't, don't recurse
     (and (or (is-keyword (car x)) (is-keyword (car y))) (not (equal? (car x) (car y)))))
      (list 'if 'TCP x y)]
    ;;; recursive case
    ;; compare-expr on the head and tail of x and y
    [else (cons (compare-expr (car x) (car y)) (compare-expr (cdr x) (cdr y)))]))

(define (test-compare-expr x y)
  (and (equal? (eval x) (eval (list 'let '((TCP #t)) (compare-expr x y))))
       (equal? (eval y) (eval (list 'let '((TCP #f)) (compare-expr x y))))))

;; see corresponding lines in `test-y` for how the x and y sides differ
(define test-x
  ; (0) list function
  '(list 
    ; (1) lambdas with different arguments
    ((lambda (a b c) (+ a b c)) 1 2 3)
    ; (2) lambdas with same arguments and different argument values and bodies
    ((lambda (a b c) (* a b c)) 1 2 3)
    ; (3) quoted expressions using the `quote` keyword
    (quote (a b c))
    ; (4) quoted expressions (lists) using quote mark (') literal
    '(d e f)
    ; (5) cons with differing first argument
    (cons 'a '(b c d))
    ; (6) cons with differing second argument
    (cons 'a '(b c d))
    ; (7) if with differing first expression
    (if (equal? '(a b) '(a b)) 'a 'b)
    ; (8) if with differing second expression
    (if (equal? '(a b) '(a b)) 'a 'b)
    ; (9) if with differing third expression
    (if (equal? '(a b) '(a b)) 'a 'b)
    ; (10) conditionals: both true, x true and y false, x false and y true, both false
    #t #t #f #f
    ; (11) arithmetic expressions
    (+ 1 2 (/ 3 4) (* 4 5 (- 6 7) 8))
    ; (12) let with same variables but different assignment values
    (let ((a 'bob) (b 'alice) (c 'sue)) (list a b c))
    ; (13) let with different variables that are referenced in body
    (let ((a 'bob) (b 'alice) (c 'sue)) (list a b c))
    ; (14) let with different variables that are NOT referenced in body
    (let ((a 'bob) (b 'alice) (c 'sue)) (+ 1 2 3))
    ; (15) incompatible keyword expressions
    (if #t 'zoe 'ed)
    ; (16) keyword expression vs. non-keyword expression
    (quote (+ 1 2 3))
    ; (17) quote keyword vs. quote literal (should behave identically)
    (quote (+ 1 2 3))
    ; (18) nesting - let, lambda, equal?, list
    (let ((a ((lambda (b c) (if (equal? b c) 1 2)) 'bob 'alice))) (list 1 a))))

(define test-y
  '(list ; 0
    ((lambda (a b d) (+ a b d)) 1 2 3) ; 1
    ((lambda (a b c) (* a c b)) 1 2 4) ; 2
    (quote (a b d)) ; 3
    '(z e f) ; 4
    (cons 'z '(b c d)) ; 5
    (cons 'a '(b z d)) ; 6
    (if (equal? '(a b) '(a c)) 'a 'b) ; 7
    (if (equal? '(a b) '(a b)) 'z 'b) ; 8
    (if (equal? '(a b) '(a b)) 'a 'z) ; 9
    #t #f #t #f ; 10
    (+ 1 9 (/ 3 4) (* 8 5 (- 1 7) 3)) ; 11
    (let ((a 'bob) (b 'joe) (c 'lauren)) (list a b c)) ; 12
    (let ((a 'bob) (z 'alice) (c 'sue)) (list a z c)) ; 13
    (let ((a 'bob) (z 'alice) (c 'sue)) (+ 1 2 3)) ; 14
    ((lambda (a b) (list a b)) 'zoe 'ed) ; 15
    (+ 1 2 3) ; 16
    '(+ 1 2 3) ; 17
    (let ((a ((lambda (b c) (if (equal? b c) 1 1)) 'bob 'lauren))) (list 1 a)))) ; 18

;;; debug
;; http://www.greghendershott.com/2014/11/racket-workflow.html
;; (require racket/trace)
;; (trace compare-expr)

;;; tests
;; define an assert syntax rule for easier testing
;; adapted from http://rosemary.umw.edu/~finlayson/class/fall13/cpsc401/notes/10-racket.html
#|
(define-syntax-rule (assert output expected)
  (when (not (equal? output expected))
    (display "Assertion `") (display #'c) (displayln "' failed.")
    (display "  Result: ") (displayln output)
    (display "Expected: ") (displayln expected)))
    ;(raise 'AssertionError)))

;; test cases from spec
(assert (compare-expr 12 12) 12)
(assert (compare-expr 12 20) '(if TCP 12 20))
(assert (compare-expr #t #t) #t)
(assert (compare-expr #f #f) #f)
(assert (compare-expr #t #f) 'TCP)
(assert (compare-expr #f #t) '(not TCP))
(assert (compare-expr 'a '(cons a b)) '(if TCP a (cons a b)))
(assert (compare-expr '(cons a b) '(cons a b)) '(cons a b))
(assert (compare-expr '(cons a b) '(cons a c)) '(cons a (if TCP b c)))
(assert (compare-expr '(cons (cons a b) (cons b c))
                      '(cons (cons a c) (cons a c))) 
        '(cons (cons a (if TCP b c)) (cons (if TCP b a) c)))
(assert (compare-expr '(cons a b) '(list a b)) '((if TCP cons list) a b))
(assert (compare-expr '(list) '(list a)) '(if TCP (list) (list a)))
(assert (compare-expr ''(a b) ''(a c)) '(if TCP '(a b) '(a c)))
(assert (compare-expr '(quote (a b)) '(quote (a c))) '(if TCP '(a b) '(a c)))
(assert (compare-expr '(quoth (a b)) '(quoth (a c))) '(quoth (a (if TCP b c))))
(assert (compare-expr '(if x y z) '(if x z z)) '(if x (if TCP y z) z))
(assert (compare-expr '(if x y z) '(g x y z)) '(if TCP (if x y z) (g x y z)))
(assert (compare-expr '(let ((a 1)) (f a)) '(let ((a 2)) (g a))) '(let ((a (if TCP 1 2))) ((if TCP f g) a)))
(assert (compare-expr '(+ #f (let ((a 1) (b 2)) (f a b)))
                      '(+ #t (let ((a 1) (c 2)) (f a c)))) 
        '(+
          (not TCP)
          (if TCP
              (let ((a 1) (b 2)) (f a b))
              (let ((a 1) (c 2)) (f a c)))))
(assert (compare-expr '((lambda (a) (f a)) 1) '((lambda (a) (g a)) 2)) '((lambda (a) ((if TCP f g) a)) (if TCP 1 2)))
(assert (compare-expr '((lambda (a b) (f a b)) 1 2)
                      '((lambda (a b) (f b a)) 1 2))
        '((lambda (a b) (f (if TCP a b) (if TCP b a))) 1 2))
(assert (compare-expr '((lambda (a b) (f a b)) 1 2)
                      '((lambda (a c) (f c a)) 1 2))
        '((if TCP (lambda (a b) (f a b)) (lambda (a c) (f c a))) 1 2))
|#