#lang racket
(require rackunit)

(define (atom? x)
  (and (not (pair? x)) (not (null? x))))


(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define new-entry build)

(define lookup-in-entry-help
  (lambda (name keys values entry-f)
    (cond
      [(null? keys) (entry-f name)]
      [(equal? name (first keys)) (first values)]
      [else (lookup-in-entry-help name
                                  (cdr keys )
                                  (cdr values)
                                  entry-f
                                  )])))

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
                          (first entry)
                          (second entry)
                          entry-f)))
(check-equal? (lookup-in-entry 'entrée
                               (new-entry '(appetizer entrée beverage)
                                           '(paté boeuf vin))
                 (lambda (x) "Could not find name"))
              'boeuf)

(define extend-table cons)

; Takes an entry and a table and creates a new table
; putting the new entry in front of the old table
(define lookup-in-table
  (lambda (name table table-f)
    (cond
      [(null? table) (table-f name)]
      [else (lookup-in-entry name
                             (car table)
                             (lambda (x)
                               (lookup-in-table name
                                                (cdr table)
                                                table-f)))])))
(check-equal? (lookup-in-table 'dessert
                               '(((entrée dessert)
                                  (spaghetti spumoni))
                                 ((appetizer entrée beverage)
                                  (food tastes good))
                                 ) (lambda (x) "Error")) 'spumoni)

; ----- types -----

; const type
(define *const
  (lambda (e table)
    (cond
      [(number? e) e]
      [(eq? e #t) #t]
      [(eq? e #f) #f]
      [else (build 'primitive e)])))

(check-equal? (*const 2 '()) 2)
(check-equal? (*const #t '()) #t)
(check-equal? (*const #f '()) #f)
(check-equal? (*const 'car '()) '(primitive car))
(check-equal? (*const 'cdr '()) '(primitive cdr))


(define text-of second)
; quote type
(define *quote
  (lambda (e table)
    (text-of e)))
(check-equal? (*quote '(quote 2) '()) '2)
(check-equal? (*quote '(quote x) '()) 'x)
(check-equal? (*quote '(quote cond) '()) 'cond)


(define initial-table
  (lambda (name)
    (car '())))

; identifier type
(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))
(check-equal? (*identifier 'A-CONST
                           '(((x y A-CONST)
                              (2 y 10))))
              10)


; (cons 'non-primitive (cons table e))
; lambda type
(define *lambda
  (lambda (e table)
    (build 'non-primitive (cons table (cdr e)))))
(check-equal? (*lambda '(lambda (x) (cons x y))
                      '(((y z)
                         ((8) 9)))
                       )
              '(non-primitive
                 ((((y z) ((8) 9)))
                  (x)
                  (cons x y))))





; ----------

; get the type of the expression and return its function
(define atom-to-action
  (lambda (e)
    (cond
      [(number? e) *const]
      [(eq? e #f) *const]
      [(eq? e #t) *const]
      [(eq? e 'cons) *const]
      [(eq? e 'car) *const]
      [(eq? e 'cdr) *const]
      [(eq? e 'null?) *const]
      [(eq? e 'eq?) *const]
      [(eq? e 'atom?) *const]
      [(eq? e 'zero?) *const]
      [(eq? e 'add1) *const]
      [(eq? e 'sub1) *const]
      [(eq? e 'number?) *const]
      [else *identifier]
      )))


; get the type of the expression and return its function
(define list-to-action
  (lambda (e)
    (cond
      [(atom? (car e))
       (cond
         [(eq? (car e) 'quote) *quote]
         [(eq? (car e) 'lambda) *lambda]
         [(eq? (car e) 'cond) *cond]
         [else *application]
         )]
      [else *application])))


; get the type of the expression and return its function
(define expression-to-action
  (lambda (e)
    (cond
      [(atom? e) (atom-to-action e)]
      [else (list-to-action e)])))

(check-equal? (expression-to-action 2) *const)
(check-equal? (expression-to-action #t) *const)
(check-equal? (expression-to-action #f) *const)
(check-equal? (expression-to-action 'sub1) *const)


; First it gets the type of the expression and return a function according to
; that type.
; Then apply the function to the expression and its table
(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

; eval
(define value
  (lambda (e)
    (meaning e '())))
(check-equal? (value 2) 2)
(check-equal? (value '(quote 2)) '2)


; non primitive

(define table-of first)
(define formals-of second)
(define body-of third)
(define cond-lines-of cdr)
(define question-of first)
(define answer-of second)

(define else?
  (lambda (x)
    (cond
      [(atom? x) (eq? x 'else)]
      [else #f])))


; eval cond
(define evcon
  (lambda (lines table)
    (cond
      [(else? (question-of (first lines)))
       (meaning (answer-of (first lines)) table)]
      [(meaning (question-of (first lines)) table)
       (meaning (answer-of (first lines)) table)]
      [else (evcon (cdr lines) table)])))


; cond type
(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))
(check-equal? (*cond '(cond
                        (coffee klatsch)
                        (else party))
                        '(
                          ((coffee ) (#t))
                          ((klatsch party) (5 (6)))
                          )) 5)

; eval a list of args
(define evlis
  (lambda (args table)
    (cond
      [(null? args) '()]
      [else (cons (meaning (car args) table)
                  (evlis (cdr args) table))])))


(define functions-of car)
(define arguments-of cdr)
(define *application
  (lambda (e table)
    (apply
      (meaning (functions-of e) table)
      (evlis (arguments-of e) table))))


(define primitive?
  (lambda (l)
    (eq? (car l) 'primitive)))


(define non-primitive?
  (lambda (l)
    (eq? (car l) 'non-primitive)))


(define :atom?
  (lambda (x)
    (cond
      [(atom? x) #t]
      [(null? x) #f]
      [(eq? (car x) 'primitive) #t]
      [(eq? (car x) 'non-primitive) #t]
      [else #f]
      )))


(define apply-primitive
  (lambda (name vals)
    (cond
      [(eq? name 'cons)
       (cons (first vals) (second vals))]
      [(eq? name 'car)
       (car (first vals))]
      [(eq? name 'cdr)
       (cdr (first vals))]
      [(eq? name 'null?)
       (null? (first vals))]
      [(eq? name 'eq?)
       (eq? (first vals) (second vals))]
      [(eq? name 'atom?)
       (:atom? (first vals))]
      [(eq? name 'zero?)
       (zero? (first vals))]
      [(eq? name 'add1)
       (add1 (first vals))]
      [(eq? name 'sub1)
       (sub1 (first vals))]
      [(eq? name 'number?)
       (number? (first vals))])))


; apply a non-primitive function to a val
(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-table
               (new-entry
                 (formals-of closure)
                 vals)
               (table-of closure)))))


; apply a function to val
(define apply
  (lambda (fun vals)
    (cond
      [(primitive? fun) (apply-primitive (second fun) vals)]
      [(non-primitive? fun)
       (apply-closure
         (second fun) vals)])))

(check-equal? (apply '(primitive sub1) '(1)) 0)
(check-equal? (apply '(primitive cons) '(1 ())) '(1))



(check-equal? (value 'car) '(primitive car))
(check-equal? (value 'cdr) '(primitive cdr))
(check-equal? (value 'cons) '(primitive cons))
(check-equal? (value 'null?) '(primitive null?))
(check-equal? (value 'null?) '(primitive null?))
(check-equal? (value 'eq?) '(primitive eq?))
(check-equal? (value #t) #t)
(check-equal? (value #f) #f)
(check-equal? (value '((lambda (x) 2) 2)) 2)
(check-equal? (value '((lambda (x) (add1 x)) 2)) 3)
(check-equal? (value '((lambda (x y) (cons x y)) 1  (quote ()))) '(1))
