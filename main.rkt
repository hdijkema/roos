#lang racket

(require racket/syntax)
(require racket/set)
(require uuid)
(require (for-syntax racket/base))
(require finalizer)

(provide def-roos
         -!
         roos-new
         ->
         ->>
         
         roos-class?
         roos-object?
         roos-obj?

         roos-class
         roos-classname

         roos-help
         
         with-roos-obj
         
         roos-id
         roos-id!
         roos-storage!
         roos-storage-stop-deleting!
         )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Persistence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define @@storage@@ (make-hasheq))

(define (@@cache-get obj var default)
  (let ((cl-hash (hash-ref @@storage@@ (roos-classname obj) (make-hasheq))))
    (let ((id (-> obj roos-id)))
      (let ((obj-hash (hash-ref cl-hash id (make-hasheq))))
        (hash-ref obj-hash var default)))))

(define (@@cache-set! obj var val)
  (let ((cl-hash (hash-ref @@storage@@ (roos-classname obj) #f)))
    (unless cl-hash
      (set! cl-hash (make-hasheq))
      (hash-set! @@storage@@ (roos-classname obj) cl-hash))
    (let ((id (-> obj roos-id)))
      (let ((obj-hash (hash-ref cl-hash id #f)))
        (unless obj-hash
          (set! obj-hash (make-hasheq))
          (hash-set! cl-hash id obj-hash))
        (hash-set! obj-hash var val)))))

(define @@stop-deleting@@ #f)

(define (@@cache-delete! obj)
  (unless @@stop-deleting@@
    (let ((cl-hash (hash-ref @@storage@@ (roos-classname obj) (make-hasheq))))
      (hash-remove! cl-hash (-> obj roos-id)))))

(define (@@cache-stop-deleting yn)
  (set! @@stop-deleting@@ yn))

(define (roos-storage! getter setter deleter stop-deleting)
  (set! @@cache-get getter)
  (set! @@cache-set! setter)
  (set! @@cache-delete! deleter)
  (set! @@cache-stop-deleting stop-deleting)
  )

(define (roos-storage-stop-deleting! yn)
  (@@cache-stop-deleting yn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Class definition syntax
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct roos-class* (closure name members super-inits decl))
(struct roos-object* (this supers classname the-class
                           set-caller (caller #:mutable)))

(define (@@new-id)
  (string->symbol (string-append "roos-" (uuid-string))))

(define-syntax @@mk-supers
  (syntax-rules ()
    ((_ supers)
     (define supers (list)))
    ((_ supers s1 ...)
     (define supers (list s1 ...)))
    ))

(define-syntax @@mk-lit
  (syntax-rules ()
    ((_ (f ...))
     '(f ...))
    ((_ (f . a))
     '(f . a))
    ))
        

(define-syntax @@mk-name
  (syntax-rules ()
    ((_ (f . args))
     'f)
    ((_ f . args)
     'f)
    ))

(define-syntax @@mk-proc
  (syntax-rules ()
    ((_ (f . args))
     f)
    ((_ f . args)
     f)
    ))

(define-syntax @@mk-super-infos
  (syntax-rules ()
    ((_ supers)
     '())
    ((_ supers s1 ...)
     (list 's1 ...))
    ))

(define-syntax (@@mk-mem-info* stx)
  (syntax-case stx ()
    ((@@mk-mem-info* persist doc a b)
     #'(list 'a 'a (string-append "<persist> " doc)))
    ((@@mk-mem-info* doc a b)
     (if (eq? (syntax->datum #'doc) 'persist)
         #'(list 'a 'a "<persist>")
         #'(list 'a 'a doc)
     ))
    ))

(define-syntax @@mk-member-info*
  (syntax-rules ()
    ((_ ((f) expr ...))
     (list (@@mk-name f) (@@mk-lit (f)) ""))
    ((_ ((f . b) expr ...))
     (list (@@mk-name f . b) (@@mk-lit (f . b)) ""))
    ((_ ((f a ...) expr ...))
     (list (@@mk-name f a ...) (@@mk-lit (f a ...)) ""))
    ((_ ((f a ... . b) expr ...))
     (list (@@mk-name f a ... . b) (@@mk-lit (f a ... . b)) ""))
    ((_ (doc ((f) expr ...)))
     (list (@@mk-name f) (@@mk-lit (f)) doc))
    ((_ (doc ((f . a) expr ...)))
     (list (@@mk-name f . a) (@@mk-lit (f . a)) doc))
    ((_ (doc ((f a ...) expr ...)))
     (list (@@mk-name f a ...) (@@mk-lit (f a ...)) doc))
    ((_ (doc ((f a ... . b) expr ...)))
     (list (@@mk-name f a ... . b) (@@mk-lit (f a ... . b)) doc))
    ((_ (persist doc (a b)))
     (@@mk-mem-info* persist doc a b))
    ((_ (doc (a b)))
     (@@mk-mem-info* doc a b))
    ((_ (a b))
     (list 'a 'a ""))
    ))

(define-syntax (@@mk-member-info stx)
  (syntax-case stx ()
    ((_ (keyw))
     (let ((kw (syntax->datum #'keyw)))
       (if (or (eq? kw 'init)
               (eq? kw 'finalize))
           #'(list 'keyw 'keyw (format "has empty '~a'" 'keyw))
           #'(@@mk-member-info* keyw))))
    ((_ (keyw b1 ...))
     (let ((kw (syntax->datum #'keyw)))
       (if (or (eq? kw 'init)
               (eq? kw 'finalize))
           #'(list 'keyw 'keyw (format "has defined '~a'" 'keyw))
           #'(@@mk-member-info* (keyw b1 ...)))))
    ((_ x)
     #'(@@mk-member-info* x))
    ))

(define-syntax @@mk-member-infos
  (syntax-rules ()
    ((_ b1 ...)
     (list (@@mk-member-info b1) ...))
    ))

(define-syntax @@mk-super-init
  (syntax-rules ()
    ((_ (roos-new cl ...))
     (if (roos-class? (@@mk-proc cl ...))
         (@@mk-proc cl ...)
         roos-new))
    ((_ (f ...))
     (@@mk-proc f ...))
    ((_ f)
     f)
    ))

(define-syntax @@mk-super-inits
  (syntax-rules ()
    ((_ supers)
     (list))
    ((_ supers s1 ...)
     (list (@@mk-super-init s1) ...))
    ))

(define-syntax @@mk-persist-def
  (syntax-rules ()
    ((_ this supers (persist doc (a b)))
     (begin
       (hash-set! this 'a (lambda () (@@cache-get this 'a b)))
       (hash-set! this (string->symbol (format "~a!" 'a))
                  (lambda (v) (@@cache-set! this 'a v) v))))
    ((_ this supers (persist (a b)))
     (begin
       (hash-set! this 'a (lambda () (@@cache-get this 'a b)))
       (hash-set! this (string->symbol (format "~a!" 'a))
                  (lambda (v) (@@cache-set! this 'a v) v))))
    ))

(define-syntax @@mk-var-def
  (syntax-rules ()
    ((_ this supers (doc (a b)))
     (begin
       (define a b)
       (hash-set! this 'a (lambda () a))
       (hash-set! this (string->symbol (format "~a!" 'a))
                  (lambda (v) (set! a v) a))))
    ((_ this supers (a b))
     (begin
       (define a b)
       (hash-set! this 'a (lambda () a))
       (hash-set! this (string->symbol (format "~a!" 'a))
                  (lambda (v) (set! a v) a))))
    ))

(define-for-syntax (@@guard-persist-doc pd stx)
  (unless (or (eq? pd 'persist) (string? pd))
    (let ((ind (if (symbol? pd) pd (string->symbol (format "~a" pd)))))
      (raise-syntax-error #f
                          "keyword 'persist' or documentation expected"
                          (cadddr (syntax->datum stx))))))

(define-for-syntax (@@guard-persist p stx)
  (unless (eq? p 'persist)
    (let ((ind (if (symbol? p) p (string->symbol (format "~a" p)))))
      (raise-syntax-error #f "keyword 'persist' expected"
                          (cadddr (syntax->datum stx))))))

(define-for-syntax (@@guard-doc d stx)
  (unless (string? d)
    (raise-syntax-error #f "documentation expected"
                        (cadddr (syntax->datum stx)))))

(define-for-syntax (@@guard-identifier i stx)
  (unless (and (identifier? i) (not (symbol? i)))
    (raise-syntax-error #f "identifier expected"
                        (cadddr (syntax->datum stx)))))

(define-for-syntax (@@guard-func-identifier f-def stx)
  (let ((i (car (syntax-e f-def))))
    (unless (and (identifier? i) (not (symbol? i)))
      (raise-syntax-error #f "identifier expected"
                          (cadddr (syntax->datum stx))))))


(define-syntax (@@mk-persist stx)
  (syntax-case stx ()
    [(@@mk-persist this supers (p doc (a b)))
     (let ((pp (syntax->datum #'p))
           (dd (syntax->datum #'doc)))
       (@@guard-persist pp stx)
       (@@guard-doc dd stx)
       (if (eq? pp 'persist)
         #'(begin
             (hash-set! this 'a (lambda () (@@cache-get this 'a b)))
             (hash-set! this (string->symbol (format "~a!" 'a))
                        (lambda (v) (@@cache-set! this 'a v) v)))
         #'(begin
             (define a b)
             (hash-set! this 'a (lambda () a))
             (hash-set! this (string->symbol (format "~a!" 'a))
                        (lambda (v) (set! a v) v)))
         ))]
    [(@@mk-persist this supers (p (a b)))
     (let ((pp (syntax->datum #'p)))
       (@@guard-persist-doc pp stx)
       (if (eq? pp 'persist)
         #'(begin
             (hash-set! this 'a (lambda () (@@cache-get this 'a b)))
             (hash-set! this (string->symbol (format "~a!" 'a))
                        (lambda (v) (@@cache-set! this 'a v) v)))
         #'(begin
             (define a b)
             (hash-set! this 'a (lambda () a))
             (hash-set! this (string->symbol (format "~a!" 'a))
                        (lambda (v) (set! a v) v)))
         ))]
    [(@@mk-persist this supers (a b))
     (begin
       (@@guard-identifier #'a stx)
       #'(begin
           (define a b)
           (hash-set! this 'a (lambda () a))
           (hash-set! this (string->symbol (format "~a!" 'a))
                      (lambda (v) (set! a v) v))))
       ]
    ))

(define-syntax (@@mk-doc-method stx)
  (syntax-case stx ()
    [(@@mk-method this supers (doc ((f ...) expr ...)))
     (let ((dd (syntax->datum #'doc))
           (ff #'(f ...)))
       ;(display "form1:")(display (syntax->datum ff))(newline)
       (@@guard-doc dd stx)
       (@@guard-func-identifier ff stx)
       #'(begin
           (define (f ...) expr ...)
           (hash-set! this (@@mk-name f ...) (@@mk-proc f ...))
           ))]
    [(@@mk-method this supers (doc ((f ... . b) expr ...)))
     (let ((dd (syntax->datum #'doc)))
       ;(display "form4:")(display (syntax->datum #'(f ...)))(newline)
       (@@guard-doc dd stx)
       (@@guard-func-identifier #'(f ...) stx)
       #'(begin
           (define (f ... . b) expr ...)
           (hash-set! this (@@mk-name f ...) (@@mk-proc f ...))
           ))]
    ))

(define-syntax (@@mk-method stx)
  (syntax-case stx ()
    [(@@mk-method this supers ((f ...) expr ...))
     (begin
       ;(display "form2:")(display (syntax->datum #'(f ...)))(newline)
       (@@guard-func-identifier #'(f ...) stx)
       #'(begin
           (define (f ...) expr ...)
           (hash-set! this (@@mk-name f ...) (@@mk-proc f ...))
           ))]
    [(@@mk-method this supers ((f ... . b) expr ...))
     (begin
       ;(display "form3:")(display (syntax->datum #'(f ...)))(newline)
       (@@guard-func-identifier #'(f ...) stx)
       #'(begin
           (define (f ... . b) expr ...)
           (hash-set! this (@@mk-name f ...) (@@mk-proc f ...))
           ))]
    ))

(define-syntax @@mk-body*
  (syntax-rules ()
    ((_ this supers (doc ((f) expr ...)))
     (@@mk-doc-method this supers (doc ((f) expr ...))))
    ((_ this supers (doc ((f . b) expr ...)))
     (@@mk-doc-method this supers (doc ((f . b) expr ...))))
    ((_ this supers (doc ((f a ... . b) expr ...)))
     (@@mk-doc-method this supers (doc ((f a ... . b) expr ...))))
    ((_ this supers ((f) expr ...))
     (@@mk-method this supers ((f) expr ...)))
    ((_ this supers ((f a ...) expr ...))
     (@@mk-method this supers ((f a ...) expr ...)))
    ((_ this supers ((f a ... . b) expr ...)) 
     (@@mk-method this supers ((f a ... . b) expr ...)))
    ((_ this supers ((f . b) expr ...))
     (@@mk-method this supers ((f . b) expr ...)))
    ((_ this supers (doc ((f a ...) expr ...)))
     (@@mk-doc-method this supers (doc ((f a ...) expr ...))))
    ((_ this supers (persist doc (a b)))
     (@@mk-persist this supers (persist doc (a b))))
    ((_ this supers (doc (a b)))
     (@@mk-persist this supers (doc (a b))))
    ((_ this supers (a b))
     (@@mk-persist this supers (a b)))
    ))

(define-syntax (@@mk-keyw stx)
  (syntax-case stx ()
    ((_ this keyw)
     (begin
       (printf "mk-keyw: ~a" (syntax->datum #'keyw))
       #'(hash-set! this 'init (lambda () #f))))
    ((_ this keyw body ...)
     (begin
       (printf "mk-keyw: ~a" (syntax->datum #'keyw))
       #'(hash-set! this 'init (lambda () body ...))))
    ))

(define-syntax (@@mk-body stx)
  (syntax-case stx ()
    ((_ this supers (keyw))
     (let ((kw (syntax->datum #'keyw)))
       (if (or (eq? kw 'init)
               (eq? kw 'finalize))
           #'(@@mk-keyw this keyw)
           #'(@@mk-body* this supers (keyw)))))
    ((_ this supers (keyw a ...))
     (let ((kw (syntax->datum #'keyw)))
       (if (or (eq? kw 'init)
               (eq? kw 'finalize))
           #'(@@mk-keyw this keyw a ...)
           #'(@@mk-body* this supers (keyw a ...)))))
    ((_ this supers any)
     #'(@@mk-body* this supers any))
    ))

(define-syntax @@mk-bodies
  (syntax-rules ()
    ((_ this supers (b1 ...))
     (begin
       (@@mk-body this supers b1)
       ...))))

(define-syntax @@mk-result
  (syntax-rules ()
    ((_ (cl . args) val)
     (set! cl val))))

(define (@@find-func f objs)
  (if (null? objs)
      #f
      (let ((h (roos-object*-this (car objs))))
        (if (hash-ref h f #f)
            (hash-ref h f)
            (let ((ff (@@find-func f (roos-object*-supers (car objs)))))
              (if ff
                  ff
                  (@@find-func f (cdr objs))))))))


(define-syntax (@@guard-this stx)
  (syntax-case stx ()
    ((_ this)
     (begin
       (unless (eq? (syntax->datum #'this) 'this)
         (raise-syntax-error #f "Keyword 'this' expected" (cadr (syntax->datum stx))))
       #'#t
     ))))

(define-syntax (@@guard-supers stx)
  (syntax-case stx ()
    ((_ supers)
     (begin
       (unless (eq? (syntax->datum #'supers) 'supers)
         (raise-syntax-error #f "Keyword 'supers' expected" (cadr (syntax->datum stx))))
       #'#t
       ))))


(define-syntax @@check-keywords
  (syntax-rules ()
    ((_ this supers . args)
     (begin
       (@@guard-this this)
       (@@guard-supers supers)))))

(define (@@has-persist syntax count)
  ;(display "has-persist ")(display count)(display " ")(display syntax)(newline)
  (if (null? syntax)
      #f
      (let ((b (car syntax)))
        (if (list? b)
            (if (null? b)
                (@@has-persist (cdr syntax) (+ count 1))
                (if (eq? (car b) 'persist)
                    #t
                    (@@has-persist (cdr syntax) (+ count 1))))
            (@@has-persist (cdr syntax) (+ count 1))))))


(define-syntax @@finalize
  (syntax-rules ()
    ((_ this (body ...))
     (let ((has-persist (@@has-persist '(body ...) 0)))
       (if has-persist
           (let ((our-finalizer (hash-ref (roos-object*-this this) 'finalize)))
             (when (not our-finalizer)
               (set! our-finalizer (lambda () #t)))
             (hash-set! (roos-object*-this this) 'finalize 'finalizer-registered)
             (register-finalizer this
                                 (lambda (obj)
                                   ; First call our own finalizer
                                   (our-finalizer)
                                   ; Next delete from storage
                                   (@@cache-delete! obj))))
           (let ((f (hash-ref (roos-object*-this this) 'finalize)))
             (unless (eq? f #f)
               (register-finalizer this (lambda (obj) (f)))))))
     )
    ))


(define-syntax def-roos-body
  (syntax-rules ()
    ((_ cl-decl this (supers ...)
        body
        ...)
     (begin
       (@@check-keywords this supers ...)
       (define this (make-hasheq))

       (hash-set! this 'finalize #f)
         
       (@@mk-body this (supers ...) (roos-id (@@new-id)))

       (@@mk-supers supers ...)
       (@@mk-bodies this (supers ...) (body ...))

       (define (@set-caller@ c)
         (set-roos-object*-caller! this c)
         (for-each (lambda (s)
                     ((roos-object*-set-caller s) c))
                   (roos-object*-supers this)))

       (define (@caller@ f . args)
         (let ((ff (@@find-func f (list this))))
           (if ff
               (apply ff args)
               (error (format "~a: ~a - no such member." (roos-object*-classname this) f)))))

       (set! this (roos-object*
                   this
                   (@@mk-proc supers ...)
                   (@@mk-name cl-decl)
                   (@@mk-proc cl-decl)
                   @set-caller@
                   @caller@))
       (@set-caller@ @caller@)

       (@@finalize this (body ...))

       (unless (eq? (hash-ref (roos-object*-this this) 'init #f) #f)
         ((hash-ref (roos-object*-this this) 'init)))

       this
       )
     )
    ))

(define-syntax def-roos
  (syntax-rules ()
    ((_ (cl) this (supers ...) body ...)
     (begin
       (define (cl)
         (def-roos-body cl this (supers ...) body ...))
       (@@mk-result (cl)
                    (roos-class* (@@mk-proc cl)
                                 (@@mk-name cl)
                                 (@@mk-member-infos body ...)
                                 (@@mk-super-infos supers ...)
                                 'cl))))
    ((_ (cl . a) this (supers ...) body ...)
     (begin
       (define (cl . a)
         (def-roos-body (cl . a) this (supers ...) body ...))
       (@@mk-result (cl . a)
                    (roos-class* (@@mk-proc cl)
                                 (@@mk-name cl)
                                 (@@mk-member-infos body ...)
                                 (@@mk-super-infos supers ...)
                                 '(cl . a)))))
    ((_ (cl a ...) this (supers ...) body ...)
     (begin
       (define (cl a ...)
         (def-roos-body (cl a ...) this (supers ...) body ...))
       (@@mk-result (cl a ...)
                    (roos-class* (@@mk-proc cl)
                                 (@@mk-name cl)
                                 (@@mk-member-infos body ...)
                                 (@@mk-super-infos supers ...)
                                 (cl a ...)))))
    ((_ (cl a ... . b) this (supers ...) body ...)
     (begin
       (define (cl a ... . b)
         (def-roos-body (cl a ... . b) this (supers ...) body ...))
       (@@mk-result (cl a ... . b)
                    (roos-class* (@@mk-proc cl)
                                 (@@mk-name cl)
                                 (@@mk-member-infos body ...)
                                 (@@mk-super-infos supers ...)
                                 (cl a ... . b)))))
    ((_ cl this supers body ...)
     (error (string-append
             "Wrong roos definition\n"
             "Define roos classes as follows:\n\n"
             "(def-roos (cl ...) this (supers ...)\n"
             "   (attr value)\n"
             "   (persist (attr value))\n"
             "   (\"documentation\" (attr value))\n"
             "   (persist \"documentation\" (attr value))\n"
             "\n"
             "   ((method ...) body ...)\n"
             "   (\"documentation\" ((method ...) body ...))\n"
             "\n"
             "   (init expr ...)      ; optional initializer\n"
             "   (finalize expr ...)  ; optional finalizer\n"
             ")\n")))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Class Instantiation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax @-*
  (syntax-rules ()
    ((_ cl . args)
     (roos-class*-closure cl))))

(define-syntax -!
  (syntax-rules ()
    ((_ cl)
     (if (roos-class*? cl)
         ((@-* cl))
         (error "Not a roos class")))
    ((_ cl a ...)
     (if (roos-class*? cl)
         ((@-* cl) a ...)
         (error "Not a roos class")))
    ))

(define-syntax roos-new
  (syntax-rules ()
    ((_ cl ...)
     (-! cl ...))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calling methods / attributes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (@@is-supers*? objs)
  (if (null? objs)
      #t
      (if (roos-object*? (car objs))
          (@@is-supers*? (cdr objs))
          #f)))

(define (@@is-supers? objs)
  (if (list? objs)
      (if (null? objs)
          #f
          (@@is-supers*? objs))
      #f))

(define-syntax ->>
  (syntax-rules ()
    ((_ obj f)
     (if (roos-object*? obj)
         (let ((f (@@find-func 'f (list obj))))
           (if f
               f
               (if (@@is-supers? obj)
                   (let ((f (@@find-func 'f obj)))
                     (if f
                         f
                         (error (format "~a: ~a - no such member"
                                        (roos-object*-classname obj)
                                        'f
                                        ))))
                   (error "Not a list of roos objects (supers)"))))
         (error "Not a roos object")))))

(define-syntax with-roos-obj
  (syntax-rules ()
    ((_ obj (m1 ...) body ...)
     (let* ((m1 (->> obj m1))
           ...)
       body
       ...))))

(define-syntax @@mk-call
  (syntax-rules ()
    ((_ g (f))
     (g))
    ((_ g (f a ...))
     (g a ...))
    ))

(define (@@find-> obj name)
  (if (roos-object*? obj)
      (let ((f (@@find-func name (list obj))))
        (if f
            f
            (error (format "~a: ~a - no such member"
                           (roos-object*-classname obj)
                           name))))
      (if (@@is-supers? obj)
          (let ((f (@@find-func name obj)))
            (if f
                f
                (error (format "~a: ~a - no such member"
                               (roos-object*-classname obj)
                               name))))
          (error "Not a roos object of roos supers"))))

(define-syntax @->
  (syntax-rules ()
    ((_ caller f)
     (caller 'f))
    ((_ caller f a ...)
     (caller 'f a ...))
    ))

(define-syntax ->
  (syntax-rules ()
    ((_ obj f ...)
     (if (roos-object*? obj)
         (@-> (roos-object*-caller obj) f ...)
         (let ((g (@@find-> obj (@@mk-name f ...))))
           (@@mk-call g (f ...)))))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Introspection / Predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define roos-obj? roos-object*?)
(define roos-object? roos-object*?)
(define roos-class? roos-class*?)

(define (roos-id obj)
  (-> obj roos-id))

(define (roos-id! obj id)
  (-> obj roos-id! id))

(define (roos-class cl-obj)
  (if (roos-object*? cl-obj)
      (roos-object*-the-class cl-obj)
      (if (roos-class*? cl-obj)
          cl-obj
          (error "Not a roos object or class"))))

(define-syntax roos-classname
  (syntax-rules ()
    ((_ cl-obj)
     (if (roos-object*? cl-obj)
         (roos-object*-classname cl-obj)
         (if (roos-class*? cl-obj)
             'cl-obj
             (error "Not a roos object or class"))))
    ))

(define (@@get-members-cl cl funcs)
  (let ((filter-f (if (null? funcs)
                      (lambda x #t)
                      (let ((s (list->seteq funcs)))
                        (lambda (e) (set-member? s (car e)))))))
    (filter filter-f (roos-class*-members cl))))

(define (@@get-super-members-obj s-objs s)
  (if (null? s-objs)
      '()
      (let ((s-obj (car s-objs)))
        (let ((members (roos-class*-members (roos-object*-the-class s-obj))))
          (let ((s-obj-members (filter (lambda (e)
                                         (not (set-member? s (car e))))
                                       members)))
            (for-each (lambda (m)
                        (set-add! s (car m))) s-obj-members)
            (append s-obj-members
                    (@@get-super-members-obj (roos-object*-supers s-obj) s)
                    (@@get-super-members-obj (cdr s-objs) s))
            )))
      ))

(define (@@get-members-obj obj . funcs)
  (let ((filter-f (if (null? funcs)
                      (lambda x #t)
                      (let ((s (list->seteq funcs)))
                        (lambda (e) (set-member? s (car e)))))))
    (let ((cl (roos-object*-the-class obj)))
      (let* ((m (roos-class*-members cl))
             (s (list->mutable-seteq (map (lambda (e) (car e)) m))))
        (let ((supers (@@get-super-members-obj (roos-object*-supers obj) s)))
          (filter filter-f (append m supers)))))))

(define (@@travel-obj-hierarchy f obj)
  (letrec ((g (lambda (depth obj)
                (f depth obj)
                (for-each (lambda (o)
                            (g (+ depth 1) o))
                          (roos-object*-supers obj)))))
    (g 0 obj)))

(define-syntax @@mk-hlp-mems
  (syntax-rules ()
    ((_ cl-obj)
     '())
    ((_ cl-obj f ...)
     (list 'f ...))))

(define (roos-help* cl-obj . symbols)
  (let* ((is-class (roos-class*? cl-obj))
         (is-obj (roos-object*? cl-obj))
         (funcs symbols)
         (no-funcs (null? funcs)))
    (let ((cln (if is-class
                   (roos-class*-name cl-obj)
                   (roos-object*-classname cl-obj)))
          (cl (if is-class
                  cl-obj
                  (roos-object*-the-class cl-obj)))
          )
      (when no-funcs
        (begin
          (printf "Roos class: ~a\n" cln)
          
          (let ((re #px"^[(](.*)[)]$"))
            (printf "  Instantiation: (-! ~a)\n"
                    (cadr (regexp-match re
                                        (format "~a" (roos-class*-decl cl))))))
                                    
          (let ((inits (roos-class*-super-inits cl)))
            (unless (null? inits)
              (printf "  Supers initializators:~a\n" (apply string-append
                                                            (map (lambda (x) (format " ~a" x))
                                                                 inits)))))
          (when is-obj
            (begin
              (printf "  Object class hierarchy:\n")
              (@@travel-obj-hierarchy
               (lambda (depth obj)
                 (printf "    ~a~a~a~a\n"
                         (make-string depth #\space)
                         (if (= depth 0) "" #\|)
                         (make-string (* depth 3) #\-)
                         (roos-object*-classname obj)))
               cl-obj)))
         
          (printf "  ~a members:\n" (if is-class "Class" "Object"))))
         
      (if is-class
          (let ((ind (if no-funcs "    " "")))
            (for-each (lambda (m)
                        (printf "~a~a~a\n"
                                ind
                                (cadr m)
                                (if (string=? (caddr m) "")
                                    ""
                                    (format ": ~a" (caddr m)))))
                      (@@get-members-cl cl-obj funcs)))
          (let ((s (mutable-seteq)))
            (@@travel-obj-hierarchy
             (lambda (_depth obj)
               (let ((cl (roos-object*-the-class obj))
                     (cln (roos-object*-classname obj))
                     (ind (if no-funcs "    " ""))
                     (depth (if no-funcs _depth 0)))
                 (let ((members
                        (filter (lambda (m)
                                  (not (set-member? s (car m))))
                                (@@get-members-cl cl funcs))))
                   (for-each (lambda (m)
                               (set-add! s (car m))) members)
                   (unless (null? members)
                     (let ((starter (format "~a~a~a~a"
                                            (make-string depth #\space)
                                            (if (= depth 0) "" "|")
                                            (make-string (* depth 3) #\-)
                                            cln)))
                       (printf "~a~a - ~a~a\n"
                               ind
                               starter
                               (cadr (car members))
                               (if (string=? (caddr (car members)) "")
                                   ""
                                   (format ": ~a" (caddr (car members)))))
                       (let ((indent (make-string ( * depth 3) #\space))
                             (line (make-string
                                    (string-length (symbol->string cln))
                                    #\-)))
                         (for-each (lambda (m)
                                     (if no-funcs
                                         (printf "~a~a |~a ~a~a\n"
                                                 ind
                                                 indent
                                                 line
                                                 (cadr m)
                                                 (if (string=? (caddr m) "")
                                                     ""
                                                     (format ": ~a" (caddr m))))
                                         (printf "~a~a - ~a~a\n"
                                                 ind
                                                 starter
                                                 (cadr m)
                                                 (if (string=? (caddr m) "")
                                                     ""
                                                     (format ": ~a" (caddr m))))
                                         ))
                                   (cdr members))))))))
             cl-obj)

            (let ((cr ""))
              (for-each (lambda (func)
                          (unless (set-member? s func)
                            (begin
                              (printf "~a~a is not a member\n" cr func)
                              (set! cr ""))))
                        funcs)
              )
            )) ; let s
      )))


(define-syntax roos-help
  (syntax-rules ()
    ((_ cl-obj)
     (roos-help* cl-obj))
    ((_ cl-obj a ...)
     (roos-help* cl-obj 'a ...))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Testing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (require rackunit)

  (define test-nr 0)

  (define-syntax tst
    (syntax-rules ()
      ((_ tst)
       (begin
         (set! test-nr (+ test-nr 1))
         (printf "test ~a. ~a: " test-nr 'tst)
         (let ((chk (check-true tst "failed")))
           (printf "~a\n" (if chk "OK" "not oke"))
           chk)))
      ((_ tst msg)
       (begin
         (set! test-nr (+ test-nr 1))
         (printf "test ~a. ~a: " test-nr msg)
         (let ((chk (check-true tst "failed")))
           (printf "~a\n" (if chk "OK" "not oke"))
           chk)))
      ))

  (test-case
   "Simple ROOS declaration and usage"
   (def-roos (t1) this (supers) (a 10))
   (let ((obj (-! t1)))
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
     ;("The v function gets and sets the y member of the super object of class a"
      ((v . a)
       (if (null? a)
           (-> supers y)
           (begin
             (-> supers y! (car a))
             (-> supers y))))
     ;)
     )
   (let ((bb (roos-new b)))
     (tst (= (-> bb y) 55))
     (tst (= (-> bb g 2) 110))
     (tst (= (-> bb v) 6))
     (tst (= (-> bb v 10) 10))
     (tst (= (-> bb g 3) 165))
     (tst (= (-> bb y! 10) 10))
     (tst (= (-> bb g 2) 20)))
   )

  (test-case
   "ROOS al declaration variants"
   (def-roos (decl) this (supers)
     (a1 1)
     (persist (a2 2))
     ("a3 doc" (a3 3))
     (persist "a4 doc" (a4 4))
     ((f1) (+ 2 3) (* 3 3))
     ("f2-doc" ((f2) (* 3 3) (+ 2 2)))
     ((f3 a b) (* a b))
     ((f4 a . b) (cons a b))
     ("f5-doc" ((f5 a b) (* a b)))
     ("f6-doc" ((f6 a) (* a a a)))
     ("f7-doc" ((f7 a . b) (cons a (cons (length b) (cons a b)))))
     ("f8-doc" ((f8 . b) (append b b)))
     ((f9 . c) (append c c))
     ((f10 a b . d)
      (cons (list a b) d))
     ("f11-doc" ((f11 a b c d e . h) (cons (* a b c d e) h)))
     )
   (let ((o (-! decl)))
     (tst (= (-> o a1) 1))
     (tst (begin (-> o a1! 33) (= (-> o a1) 33)))
     (tst (symbol? (-> o roos-id)))
     (tst (begin (printf "roos-id: ~a " (-> o roos-id)) #t) "Displaying roos id of object")
     (tst (eq?  (-> o roos-id! 'my-id) 'my-id))
     (tst (begin (printf "roos-id: ~a " (-> o roos-id)) #t) "Displaying roos id of object after set")
     (-> o a2! 99)
     (tst (= (-> o a2) 99))
     (tst (begin (printf "storage: ~a " @@storage@@) #t))
     (tst (= (hash-ref (hash-ref (hash-ref @@storage@@ 'decl) 'my-id ) 'a2) 99))
     (let ((o1 (-! decl)))
       (tst (eq?  (-> o1 roos-id! 'my-id) 'my-id) "Don't do this at home: giving new object same id as existing object")
       (tst (begin (printf "o1 -> a2: ~a " (-> o1 a2)) #t) "a2 = o1 -> a2")
       (tst (= (-> o1 a2) (-> o a2)))
       (tst (equal? (-> o1 a2! "hoi") "hoi"))
       (tst (string=? (-> o a2) "hoi"))
       )
     (collect-garbage)
     (tst (= (-> o a2) 2) "After collection of o1, o will be deleted from storage")
     )
   (def-roos (t1) this (supers)
     (x 8))
   (def-roos (t2 a) this (supers)
     (x a))
   (tst (= (-> (-! t2 5) x) 5))
   (def-roos (t3 . a) this (supers)
     ((f y) (map (lambda (x) (* x y)) a)))
   (tst (equal? (-> (-! t3 4 5 6) f 2) '(8 10 12)))
   (def-roos (t4 a b c . d) this (supers (-! t3 a b c))
     ((g y) (cons (map (lambda (x) (+ x y))
                       (-> supers f y)) (map (lambda (x) (* x y)) d)))
     )
   (tst (let ((r (-> (-! t4 2 3 4 12 13) g 2))
               (R '((6 8 10) 24 26)))
           (equal? r R)))
                       
   )
  )