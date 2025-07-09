#lang racket

(require (rename-in racket/class [send old-send] [new old-new]))
(require (for-syntax (rename-in roos [-> old->])))
(require (rename-in roos [-> old->]))

(provide -> send new)

(define-syntax send
  (syntax-rules ()
    ((_ obj method)
     (if (roos-object? obj)
         (-> obj method)
         (old-send obj method)))
    ((_ obj method a ...)
     (if (roos-object? obj)
         (-> obj method a ...)
         (old-send obj method a ...)))
    ))



(define-syntax ->
  (syntax-rules ()
    ((_ obj method)
     (if (roos-object? obj)
         (old-> obj method)
         (old-send obj method)))
    ((_ obj method a ...)
     (if (roos-object? obj)
         (old-> obj method a ...)
         (old-send obj method a ...)))
    ))

(define-syntax new*
  (syntax-rules (v)
    ((_ cl (x y) ...)
     (old-new cl (x y) ...))
    ((_ cl x ...)
     (old-new cl (v x) ...))
    ))

(define-syntax new
  (syntax-rules ()
    ((_ cl)
     (if (roos-class? cl)
         (roos-new cl)
         (old-new cl)))
    ((_ cl a ...)
     (if (roos-class? cl)
         (roos-new cl a ...)
         (new* cl a ...)))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Testing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (require rackunit)

  (define (t% x)
    (class object%
      (init-field (y* x))
      (define/public (y) y*)
      (define/public (y! x) (set! y* x))
      (define/public (f a) (* (send this y) a))
      (super-new)
      ))

  (def-roos (t x) this (supers)
    (y x)
    ((f a) (* (-> this y) a))
    )
  
  (check-true
   (let ((cl (t% 5)))
     (let ((o (new cl)))
       (= (send o f 2) 10))))

  (check-true
   (let ((cl (t% 6)))
     (let ((o (new cl)))
       (= (-> o f 3) 18))))

  (check-true
   (let ((o (new t 8)))
     (= (-> o f 4) 32)))

  (check-true
   (= (send (new t 4) f 2) 8))
)

