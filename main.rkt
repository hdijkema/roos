#lang racket

(require racket/syntax)
(require uuid)

(provide def-roos
         -*
         roos-new
         ->
         ->>
         roos-object?
         roos-obj?
         roos-class
         roos-class?
         roos-classname
         with-roos-obj
         roos-id
         roos-id!
         roos-drop!
         roos-storage!
         roos-members
         )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Persistance helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define @storage@ (make-hasheq))

(define (cache-get id symbol)
  (let ((h (hash-ref @storage@ id)))
    (hash-ref h symbol)))

(define (cache-set! id symbol val)
  (let ((h (hash-ref @storage@ id (make-hasheq))))
    (hash-set! h symbol val)
    (hash-set! @storage@ id h)))

(define (cache-del! id)
  (hash-remove! @storage@ id))

(define (roos-storage! fn-cache-get fn-cache-set! fn-cache-del!)
  (set! cache-get fn-cache-get)
  (set! cache-set! fn-cache-set!)
  (set! cache-del! fn-cache-del!))

(define (new-id)
  (string->symbol (string-append "roos-" (uuid-string))))

(define (roos-id obj)
  (->> obj @roos-id@))

(define (roos-id! obj id)
  (-> obj @set-roos-id@ id))

(define (roos-drop! obj)
  (let ((id (roos-id obj)))
    (cache-del! id)))


  (define (roos-class? cl)
    (if (pair? cl)
        (eq? (car cl) 'roos-class)
        #f))

  (define (roos-object? obj)
    (if (pair? obj)
        (eq? (car obj) 'roos-object)
        #f))

  (define roos-obj? roos-object?)

  (define (roos-classname cl-obj)
    (if (roos-object? cl-obj)
        (-> cl-obj @roos-classname@)
        (if (roos-class? cl-obj)
            ((cdr cl-obj) '@roos-classname@)
            (error "This is not a roos object or class"))))

  (define (roos-class cl-obj)
    (if (roos-object? cl-obj)
        (cons 'roos-class (-> cl-obj @roos-class@))
        (if (roos-class? cl-obj)
            cl-obj
            (error "This is not a roos object or class"))))

  (define (roos-members cl-obj)
    (if (roos-object? cl-obj)
        (-> cl-obj @roos-members@)
        (if (roos-class? cl-obj)
            ((cdr cl-obj) '@roos-members@)
            (error "This is not a roos object or class"))))

  (define (-* . args)
    (if (null? args)
        (error "This is not a roos class")
        (let ((cl (car args)))
          (if (roos-class? cl)
              (begin
                ;(display cl)(display (cdr args))(newline)
                (apply (cdr cl) (cdr args)))
              (error "This is not a roos class")))))

  (define roos-new -*)

  ;(define-syntax roos-new
  ;  (syntax-rules ()
  ;    ((_ cl ...)
  ;     (-* cl ...))))

  ;(define-syntax -*
  ;  (syntax-rules ()
  ;    ((_ cl)
  ;     (if (roos-class? cl)
  ;         ((cdr cl))
  ;         (error "This is not a roos class")))
  ;    ((_ cl a ...)
  ;     (if (roos-class? cl)
  ;         ((cdr cl) a ...)
  ;         (error "This is not a roos class")))))


  (define-syntax ->
    (syntax-rules ()
      ((_ obj method)
       (if (roos-object? obj)
           ((cdr obj) 'method)
           (error "This is not a roos object")))
      ((_ obj method arg ...)
       (if (roos-object? obj)
           (begin
             ((cdr obj) 'method arg ...))
           (error "This is not a roos object")))))

  (define-syntax ->>
    (syntax-rules ()
      ((_ obj method)
       (if (roos-object? obj)
           (-> (cdr obj) @get-method@ 'method)
           (error "This is not a roos object")))))
       

  (define-syntax with-roos-obj
    (syntax-rules ()
      ((_ obj (m1 ...)
          expr ...)
       (if (roos-object? obj)
           (let ((m1 (->> obj m1))
                 ...)
             expr
             ...)
           (error (format "~a: not a roos object" obj))))))

  
  (define-syntax @@roos-def
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
      ((_ h (p a b))
       (begin
         (cache-set! (hash-ref h '@roos-id@) 'a b)
         (if (not (eq? 'p 'persist)) (error (format "Keyword 'persist' expected for attribute ~a" 'a)) #t)
         (hash-set! h 'a (lambda () (cache-get (hash-ref h '@roos-id@) 'a)))
         (hash-set! h (string->symbol (format "~a!" 'a)) (lambda (v) (cache-set! (hash-ref h '@roos-id@) 'a v)))))
      ))


  (define-syntax roos-supers
    (syntax-rules ()
      ((_ self supers ())
       (hash-set! self '@supers@ (list)))
      ;((_ self supers ((a)))
      ; (hash-set! self '@supers@ (list (a))))
      ;((_ self supers ((a b ...)))
      ; (hash-set! self '@supers@ (list (a b ...))))
      ((_ self supers (x y ...))
       (hash-set! self '@supers@ (list x y ...)))
      ))

  (define-syntax roos-err
    (syntax-rules ()
      ((_ name msg)
       (error (format "~a: ~a" name msg)))))

  (define (roos-find hash supers f-name)
    ;(display hash)(display supers)(newline)
    (let ((f (hash-ref hash f-name '@roos-undefined@)))
      (if (eq? f '@roos-undefined@)
          (if (null? supers)
              (roos-err f-name "Method or Attribute not defined")
              (roos-find (-> (car supers) @roos@) (cdr supers) f-name))
          f)))

  (define (roos-call hash supers f-name args)
    (apply (roos-find hash supers f-name) args))

  (define-syntax roos-supers-def
    (syntax-rules ()
      ((_ supers @roos@ @super-starter@)
       (define supers (cons 'roos-object (lambda (f-name . args)
                                           (roos-call @super-starter@ (hash-ref @roos@ '@supers@) f-name args)))))))

  (define-syntax init-roos-body
    (syntax-rules ()
      ((_ @roos@ class classname self supers)
       (begin
         (hash-set! @roos@ '@roos-id@ (new-id))
         (hash-set! @roos@ '@set-roos-id@ (lambda (id)
                                            (hash-set! @roos@ '@roos-id@ id)))
         (define last-call #f)
         (define last-func #f)
         (define @supers-hash@ (make-hasheq))
         (define self (cons 'roos-object
                            (lambda (f-name . args)
                              (if (eq? last-call f-name)
                                  (apply last-func args)
                                  (let ((f (hash-ref @roos@ f-name #f)))
                                    (if f
                                        (begin
                                          (set! last-call f-name)
                                          (set! last-func f)
                                          (apply f args))
                                        (roos-call @roos@ (hash-ref @roos@ '@supers@) f-name args)))))
         ))
         (define supers (cons 'roos-object
                            (lambda (f-name . args)
                              (roos-call @supers-hash@ (hash-ref @roos@ '@supers@) f-name args))))
         (hash-set! @roos@ '@roos@ (lambda () @roos@))
         ;(hash-set! @roos@ '@supers@ '())
         (hash-set! @roos@ '@set-supers@ (lambda (s) (hash-set! @roos@ '@supers@ s)))
         (hash-set! @roos@ '@set-self@ (lambda (derived-self)
                                         (set! self derived-self)
                                         (for-each (lambda (super)
                                                     (-> super @set-self@ self))
                                                   (hash-ref @roos@ '@supers@))))
         (hash-set! @roos@ '@roos-classname@ (lambda () classname))
         (hash-set! @roos@ '@roos-class@ (lambda () class))
         (hash-set! @roos@ '@roos-object?@ (lambda () #t))
         (hash-set! @roos@ '@get-method@ (lambda (f-name)
                                           (roos-find @roos@ (hash-ref @roos@ '@supers@) f-name)))))
      ))

  (define-syntax roos-body
    (syntax-rules (s)
      ((_ class classname self (supers) b ...)
       (begin
         (define @roos@ (make-hasheq))
         (hash-set! @roos@ '@supers@ '())
         (init-roos-body @roos@ class classname self supers)
         (@@roos-def @roos@ b)
         ...
         (-> self @set-self@ self)
         self))
      ((_ class classname self (supers super-invokes ...) b ...)
       (let* ((s (list super-invokes ...))
             (obj 
              (begin
                (let ((@roos@ (make-hasheq)))
                  (hash-set! @roos@ '@supers@ s)
                  (init-roos-body @roos@ class classname self supers)
                  (@@roos-def @roos@ b)
                  ...
                  (-> self @set-self@ self)
                  self))))
         ;(display "Setting supers to ")(display (list super-invokes ...))(newline)
         ;(-> obj @set-supers@ (list super-invokes ...))
         obj))
      ))

  (define-syntax @roos-classname
    (syntax-rules ()
      ((_ a . b)
       'a)))

  (define-syntax @roos-class
    (syntax-rules ()
      ((_ a . b)
       a)))

  (define-syntax @@roos-level2
    (syntax-rules ()
      ((_ (a ...) self (supers ...) b ...)
       (begin
         (define (a ...)
           (roos-body (@roos-class a ...) (@roos-classname a ...) self (supers ...) b ...))))
      ((_ (a ... . b) self (supers ...) c ...)
       (begin
         (define (a ... . b)
           (roos-body (@roos-class a ... . b) (@roos-classname a ... . b) self (supers ...) c ...))))
      ))


  (define-syntax @roos-caller
    (syntax-rules ()
      ((_ a . b)
       a)))

  (define-syntax @@roos-top
    (syntax-rules ()
      ((_ (a . bb) (at ...) self (supers ...) b ...)
       (define a 
         (cons 'roos-class
               (lambda arg
                 (@@roos-level2 (at ...) self (supers ...) b ...)
                 (if (null? arg)
                     (a)
                     (if (eq? (car arg) '@roos-classname@)
                         (@roos-classname at ...)
                         (apply (@roos-caller at ...) arg)))))))
      ((_ (a . bb) (at ... . atb) self (supers ...) b ...)
       (define a
         (cons 'roos-class
               (lambda arg
                 (@@roos-level2 (at ... . atb) self (supers ...) b ...)
                 (if (null? arg)
                     (a)
                     (if (eq? (car arg) '@roos-classname@)
                         (@roos-classname at ... . atb)
                         (apply (@roos-caller at ... . atb) arg)))))))
      ))
           

  (define-syntax def-roos
    (syntax-rules ()
      ((_ (a ...) self (supers ...) b ...)
       (@@roos-top (a ...) (a ...) self (supers ...) b ...))
      ((_ (a ... . b) self (supers ...) c ...)
       (@@roos-top (a ... . b) (a ... . b) self (supers ...) c ...))
      ))
                 
(module+ test
  (require rackunit)

  (test-case
   "Simple ROOS declaration and usage"
   (def-roos (t1) this (supers) (a 10))
   (let ((obj (-* t1)))
     (check-true (= (-> obj a) 10))
     (-> obj a! 12)
     (check-true (= (-> obj a) 12)))
   )
  
  (test-case
   "ROOS declaration with supers"
   (def-roos (a x) this (supers)
         (y (+ x 4))
         ((g a) (* a (-> this y))))
   (def-roos (b) this (supers (roos-new a 2))
         (y 55)
        ((v . a)
         (if (null? a)
             (-> supers y)
             (begin
               (-> supers y! (car a))
               (-> supers y)))))
  (let ((bb (roos-new b)))
     (check-true (= (-> bb y) 55))
     (check-true (= (-> bb g 2) 110))
     (check-true (= (-> bb v) 6))
     (check-true (= (-> bb v 10) 10))
     (check-true (= (-> bb g 3) 165))
     (-> bb y! 10)
     (check-true (= (-> bb g 2) 20)))
   )
  
  )

