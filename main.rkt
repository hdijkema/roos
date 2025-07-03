#lang racket/base

(require racket/syntax)
(provide roos ->)

(define-syntax ->
  (syntax-rules ()
    ((_ obj method)
       (obj 'method))
    ((_ obj method arg ...)
     (obj 'method arg ...))))

(define-syntax roos-def
  (syntax-rules ()
    ((_ h ((a) expr ...))
     (begin
       (define (a) expr ...)
       (hash-set! h 'a a)))
    ((_ h ((a b) expr ...))
     (begin
       (define (a b) expr ...)
       (hash-set! h 'a a)))
    ((_ h ((a b ...) expr ...))
     (begin
       (define (a b ...) expr ...)
       (hash-set! h 'a a)))
    ((_ h ((a . f) expr ...))
     (begin
       (define (a . f) expr ...)
       (hash-set! h 'a a)))
    ((_ h ((a b . f) expr ...))
     (begin
       (define (a b . f) expr ...)
       (hash-set! h 'a a)))
    ((_ h ((a b ... . f) expr ...))
     (begin
       (define (a b ... . f) expr ...)
       (hash-set! h 'a a)))
    ((_ h (a b))
     (begin 
       (define a b)
       (hash-set! h 'a (lambda () a))
       (hash-set! h (string->symbol (format "~a!" 'a)) (lambda (v) (set! a v)))))
    ))

(define-syntax roos-supers
  (syntax-rules ()
    ((_ self supers ())
     (hash-set! self '@supers@ (list)))
    ((_ self supers ((a)))
     (hash-set! self '@supers@ (list (a))))
    ((_ self supers ((a b ...)))
     (hash-set! self '@supers@ (list (a b ...))))
    ((_ self supers (x y ...))
     (hash-set! self '@supers@ (list x y ...)))
    ))

(define-syntax roos-err
  (syntax-rules ()
    ((_ name msg)
       (error (format "~a: ~a" name msg)))))

(define (roos-call hash supers f-name args)
  (let ((f (hash-ref hash f-name '@roos-undefined@)))
    (if (eq? f '@roos-undefined@)
        (if (null? supers)
            (roos-err f-name "Method or Attribute not defined")
            (roos-call ((car supers) '@roos@) (cdr supers) f-name args))
        (apply f args))))

(define-syntax roos-supers-def
  (syntax-rules ()
    ((_ supers @roos@ @super-starter@ invokes)
     (define supers (lambda (f-name . args)
                      (roos-call @super-starter@ (hash-ref @roos@ '@supers@) f-name args))))))

(define-syntax roos-body
  (syntax-rules ()
    ((_ self (supers . super-invokes) b ...)
     (begin
       (define @roos@ (make-hash))
       (define @super-starter@ (make-hash))
       (define self (lambda (f-name . args)
                      (roos-call @roos@ (hash-ref @roos@ '@supers@) f-name args)))
       (roos-supers-def supers @roos@ @super-starter@ super-invokes)
       (hash-set! @roos@ '@roos@ (lambda () @roos@))
       (hash-set! @roos@ '@set-self@ (lambda (derived-self)
                                       (set! self derived-self)
                                       (for-each (lambda (super)
                                                   (super '@set-self@ self))
                                                 (hash-ref @roos@ '@supers@))))
       (roos-supers @roos@ supers super-invokes)
       (roos-def @roos@ b)
       ...
       (self '@set-self@ self)
       self))))

(define-syntax roos
  (syntax-rules ()
    ((_ (a ...) self (supers ...) b ...)
     (begin
       (define (a ...)
         (roos-body self (supers ...) b ...))))
    ((_ (a ... . b) self (supers ...) c ...)
     (begin
       (define (a ... . b)
         (roos-body self (supers ...) c ...))))
    ))



(module+ test
  (require rackunit)

  (test-case
   "Simple ROOS declaration and usage"
   (roos (t1) this (supers) (a 10))
   (let ((obj (t1)))
     (check-true (= (-> obj a) 10))
     (-> obj a! 12)
     (check-true (= (-> obj a) 12))))
  
  (test-case
   "ROOS declaration with supers"
   (roos (a x) this (supers)
         (y (+ x 4))
         ((g a) (* a (-> this y))))
   (roos (b) this (supers (a 2))
         (y 55)
        ((v . a)
          (if (null? a)
              (-> supers y)
              (begin
                (-> supers y! (car a))
                (-> supers y)))))
   (let ((bb (b)))
     (check-true (= (-> bb y) 55))
     (check-true (= (-> bb g 2) 110))
     (check-true (= (-> bb v) 6))
     (check-true (= (-> bb v 10) 10))
     (check-true (= (-> bb g 3) 165))
     (-> bb y! 10)
     (check-true (= (-> bb g 2) 20))))
  
  )

