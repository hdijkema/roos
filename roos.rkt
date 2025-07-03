#lang racket/base

(require racket/syntax)
  (provide roos ->)

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
      ((_ self ())
       (hash-set! self 'super (list)))
      ((_ self (a))
       (hash-set! self 'super (list (a))))
      ((_ self (a b ...))
       (hash-set! self 'super (list (a b ...))))
      ((_ ((x) (y) ...))
       (hash-set! self 'super (list (roos-supers x) (roos-supers y) ...)))
      ))

  (define-syntax roos
    (syntax-rules ()
      ((_ (a ...) self supers b ...)
       (begin
         (define (a ...)
           (begin
             (define self (make-hash))
             (roos-supers self supers)
             (roos-def self b)
             ...
             self))))
      ((_ (a ... . b) self supers c ...)
       (begin
         (define (a ... . b)
           (begin
             (define self (make-hash))
             (roos-supers self supers)
             (roos-def self c)
             ...
             self))))
      ))

  (define (find-super obj method)
    (let ((supers (hash-ref obj 'super '())))
      (letrec ((f (lambda (supers)
                    (if (null? supers)
                        (error (format "Method ~a not found" method))
                        (let ((s (car supers)))
                          (let ((m (hash-ref s method '%roos-nil%)))
                            (if (eq? m '%roos-nil%)
                                (f (cdr supers))
                                m)))))))
        (f supers))))

  (define-syntax ->
    (syntax-rules ()
      ((_ obj method)
       (let ((f (hash-ref obj 'method '%roos-nil%)))
         (if (eq? f '%roos-nil%)
             (let ((f* (find-super obj 'method)))
               (f*))
             (f))))
      ((_ obj method arg ...)
       (let ((f (hash-ref obj 'method '%roos-nil%)))
         (if (eq? f '%roos-nil%)
             (let ((f* (find-super obj 'method)))
               (f* arg ...))
             (f arg ...))))))

  )
