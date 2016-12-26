#lang racket

(define (is-keyword x)
  (member x '(quote lambda let if)))

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
     (and (equal? (car x) 'let) (equal? (car y) 'let) (not (equal? (map car (car (cdr x))) (map car (car (cdr y))))))
     ; (and (equal? (car x) 'let) (equal? (car y) 'let) (> (length x) 2) (> (length y) 2))
      ; the variable assignments in y that are not in x
     ; (> (length (filter (lambda (var) (member var (cdr (cdr y)))) (filter (lambda (var) (not (member var (map car (car (cdr x)))))) (map car (car (cdr y)))))) 0))
     ;; if one expression begins with a special keyword that the other doesn't, don't recurse
     (and (or (is-keyword (car x)) (is-keyword (car y))) (not (equal? (car x) (car y)))))
      (list 'if 'TCP x y)]
    ;;; recursive case
    ;; compare-expr on the head and tail of x and y
    [else (cons (compare-expr (car x) (car y)) (compare-expr (cdr x) (cdr y)))]))

;;; debug
;; http://www.greghendershott.com/2014/11/racket-workflow.html
;; (require racket/trace)
;; (trace compare-expr)

;;; tests
;; define an assert syntax rule for easier testing
;; adapted from http://rosemary.umw.edu/~finlayson/class/fall13/cpsc401/notes/10-racket.html
(define-syntax-rule (assert output expected)
  (when (not (equal? output expected))
    (display "Assertion `")
    (display #'c)
    (displayln "' failed.")
    (display "  Result: ")
    (displayln output)
    (display "Expected: ")
  (displayln expected)))
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