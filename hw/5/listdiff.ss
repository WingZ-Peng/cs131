;;; implementation

;; Return #t if obj is a listdiff, #f otherwise.
(define (listdiff? x)
  (cond
    [(or (not (pair? x)) (not (pair? (car x)))) #f]
    ; N.B. eq? compares object references, *not* values
    ; http://stackoverflow.com/a/17719745
    [(eq? (car x) (cdr x)) #t]
    [else (listdiff? (cons (cdr (car x)) (cdr x)))]))

;; Return a newly allocated listdiff of its arguments.
(define (listdiff x . args)
  ; because we're comparing object references (eq? instead of equal?), we need
  ; to instantiate a variable to use both in the car and the cdr parts of the
  ; pair so the memory locations are identical, not just the values
  (let ((tail (list x)))
    (cons (append (cons x args) tail) tail)))

;; Return #t if obj is an empty listdiff, #f otherwise.
(define (null-ld? x)
  (and (listdiff? x) (eq? (car x) (cdr x))))

;; Return a listdiff whose first element is obj and whose remaining elements 
;; are listdiff. (Unlike cons, the last argument cannot be an arbitrary 
;; object; it must be a listdiff.)
(define (cons-ld x d)
  (cons (cons x (car d)) (cdr d)))

;; Return the first element of listdiff. It is an error if listdiff has no 
;; elements. ("It is an error" means the implementation can do anything it 
;; likes when this happens, and we won't test this case when grading.)
(define (car-ld d)
  ; return an ERROR atom for testing purposes
  (if (or (not (listdiff? d)) (null-ld? d)) 'ERROR (car (car d))))

;; Return a listdiff containing all but the first element of listdiff. It is 
;; an error if listdiff has no elements.
(define (cdr-ld d)
  (if (or (not (listdiff? d)) (null-ld? d)) 'ERROR (cons (cdr (car d)) (cdr d))))

;; helper function for length-ld, below
;; identical to listdiff? except it keeps and increments a count for each
;; recursive call
(define (length-ld-helper x count)
  (cond
    [(or (not (pair? x)) (not (pair? (car x)))) 'ERROR]
    ; N.B. eq? compares object references, *not* values
    ; http://stackoverflow.com/a/17719745
    [(eq? (car x) (cdr x)) count]
    [else (length-ld-helper (cons (cdr (car x)) (cdr x)) (+ 1 count))]))

;; Return the length of listdiff.
(define (length-ld d)
  (length-ld-helper d 0))


;; Return a listdiff consisting of the elements of the first listdiff followed 
;; by the elements of the other listdiffs. The resulting listdiff is always 
;; newly allocated, except that it shares structure with the last argument. 
;; (Unlike append, the last argument cannot be an arbitrary object; it must be 
;; a listdiff.)
(define (append-ld d . args)
  (cond 
    [(> (length args) 1) (append-ld (cons (append (listdiff->list d) (car (car args))) (cdr (car args))) (car (cdr args)))]
    [else (cons (append (listdiff->list d) (car (car args))) (cdr (car args)))]))
    ;[else d]))
    ;[else (car (car args))]))
    ;[else (cons (append (listdiff->list d) (car (car args))) (cdr (car args)))]))

;; alistdiff must be a listdiff whose members are all pairs. Find the first 
;; pair in alistdiff whose car field is eq? to obj, and return that pair; if 
;; there is no such pair, return #f.
(define (assq-ld x d)
  (cond
    [(or (null? d) (not (pair? (car (car d))))) #f]
    [(eq? (car (car (car d))) x) (car (car d))]
    [else (assq-ld x (cons (cdr (car d)) (cdr d)))]))


;; Return a listdiff that represents the same elements as list.
(define (list->listdiff l)  
  (append (list (append l (list (car l)))) (list (car l))))


;; helper function for listdiff->list, below
(define (listdiff->list-helper d lst)
  (cond
    [(not (pair? d)) 'ERROR]
    [(eq? (car d) (cdr d)) lst]
    [else (listdiff->list-helper (cons (cdr (car d)) (cdr d)) (append lst (list (car (car d)))))]))

;; Return a list that represents the same elements as listdiff.
(define (listdiff->list d)
  (listdiff->list-helper d '()))


;; helper function for expr-returning, below
(define (expr-returning-helper prefix suffix)
  (quasiquote (let ((prefix (quote (unquote prefix))) (suffix (quote (unquote suffix)))) (cons (append prefix suffix) suffix))))

;; Return a Scheme expression that, when evaluated, will return a copy of 
;; listdiff, that is, a listdiff that has the same top-level data structure as 
;; listdiff. Your implementation can assume that the argument listdiff 
;; contains only booleans, characters, numbers, and symbols.
(define (expr-returning d)
  (let ((split (length (listdiff->list d))))
    (expr-returning-helper (take (car d) split) (list-tail (car d) split))))


;;; debugging
(require racket/trace)
;(trace assq-ld)

;;; tests
;; define an assert syntax rule for easier testing
;; adapted from:
;; http://rosemary.umw.edu/~finlayson/class/fall13/cpsc401/notes/10-racket.html
(define-syntax-rule (assert output expected)
  (when (not (equal? output expected))
    (display "Assertion `") (display #'c) (displayln "' failed.")
    (display "  Result: ") (displayln output)
    (display "Expected: ") (displayln expected)))

(define ils (append '(a e i o u) 'y))
(define d1 (cons ils (cdr (cdr ils))))
(define d2 (cons ils ils))
(define d3 (cons ils (append '(a e i o u) 'y)))
(define d4 (cons '() ils))
(define d5 0)
(define d6 (listdiff ils d1 37))
(define d7 (append-ld d1 d2 d6))
(define e1 (expr-returning d1))

(assert (listdiff? d1) #t)
(assert (listdiff? d2) #t)
(assert (listdiff? d3) #f)
(assert (listdiff? d4) #f)
(assert (listdiff? d5) #f)
(assert (listdiff? d6) #t)
(assert (listdiff? d7) #t)

(assert (null-ld? d1) #f)
(assert (null-ld? d2) #t)
(assert (null-ld? d3) #f)
(assert (null-ld? d6) #f)

(assert (car-ld d1) 'a)
(assert (car-ld d2) 'ERROR)
(assert (car-ld d3) 'ERROR)
(assert (car-ld d6) '(a e i o u . y))

(assert (length-ld d1) 2)
(assert (length-ld d2) 0)
(assert (length-ld d3) 'ERROR)
(assert (length-ld d6) 3)
(assert (length-ld d7) 5)

(define kv1 (cons d1 'a))
(define kv2 (cons d2 'b))
(define kv3 (cons d3 'c))
(define kv4 (cons d1 'd))
(define d8 (listdiff kv1 kv2 kv3 kv4))

(assert (eq? (assq-ld d1 d8) kv1) #t)
(assert (eq? (assq-ld d2 d8) kv2) #t)
(assert (eq? (assq-ld d1 d8) kv4) #f)

(assert (eq? (car-ld d6) ils) #t)
(assert (eq? (car-ld (cdr-ld d6)) d1) #t)
(assert (eqv? (car-ld (cdr-ld (cdr-ld d6))) 37) #t)
(assert (equal? (listdiff->list d6)
        (list ils d1 37)) #t)
(assert (eq? (list-tail (car d6) 3) (cdr d6)) #t)

(assert (listdiff->list (eval e1)) '(a e))
(assert (equal? (listdiff->list (eval e1))
        (listdiff->list d1)) #t)