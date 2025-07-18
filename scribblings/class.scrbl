#lang scribble/manual

@(require
   scribble/example
   @(for-label roos/class))

@(define myeval
   (make-base-eval '(require roos/class)))


@title[#:tag "top"]{Interoperability Macros for roos and racket/class}

@author[@author+email["Hans Dijkema" "hans@dijkewijk.nl"]]

@defmodule[roos/class]

This module provides a compatibility layer between the @seclink["roos"  #:doc '(lib "roos/scribblings/roos.scrbl") ]{@racketmodname[roos]} object system and the standard @racketmodname[racket/class] system. It exports the macros @racket[send], @racket[->], and @racket[new], which automatically dispatch to the appropriate implementation based on the type of the given object or class.

@section{Macros}

@defidform[send]{(send obj method arg ...)
A generic message-send macro that works for both Roos objects and standard Racket class objects.  
If @racket[obj] is a Roos object (@racket[roos-object?]), it uses the Roos dispatch (@racket[->]).  
Otherwise, it falls back to the original @racket[send] from @racket[racket/class].}

@examples[
#:eval (make-base-eval '(require roos/class))
(def-roos (t x) this (supers)
  (y x)
  ((f a) (* a x)))
(define o (new t 5))
(send o f 2) ; → 10
]

@defidform[->]{(-> obj method arg ...)
An alternative method dispatch macro. The syntax omits the explicit @racket[send] and may feel more
familiar to users of other programming languages that use concise or operator-based method invocation.
This macro checks whether @racket[obj] is a Roos object or a standard Racket object and dispatches accordingly.}

@examples[
#:eval (make-base-eval '(require roos/class))
(def-roos (t x) this (supers)
  (y x)
  ((f a) (* a x)))
(define o (new t 5))
(-> o f 3) ; → 15
]

@subsection{Comparison with other languages}

In other languages, method invocation often uses compact notation:

@itemlist[
  @item{Perl: @tt{$obj->method(args)}}
  @item{Python / Ruby / Java / JavaScript: @tt{obj.method(args)}}
  @item{C++: @tt{obj->method(args)} or @tt{obj.method(args)}}
]

The @racket[->] macro serves a similar role within the s-expression syntax of Racket.

@defidform[new]{(new class arg ...)
Creates a new object. If @racket[class] is a Roos class (@racket[roos-class?]), then @racket[roos-new] is used.  
Otherwise, the standard @racket[new] from @racket[racket/class] is used, supporting initialization arguments such as @racket[(init-field val)].}

@#reader scribble/comment-reader 
[racketblock 
  (require roos/class) ; instead of racket/class
          
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
  
  (displayln
   (let ((cl (t% 5)))
     (let ((o (new cl)))
       (= (send o f 2) 10))))

  (displayln
   (let ((cl (t% 6)))
     (let ((o (new cl)))
       (= (-> o f 3) 18))))

  (displayln
   (let ((o (new t 8)))
     (= (-> o f 4) 32)))

  (displayln
   (= (send (new t 4) f 2) 8))
]

@section{Implementation Notes}

@itemlist[
  @item{The original Racket @racket[send] and @racket[->] are renamed to @racket[old-send] and @racket[old->] internally.}
  @item{The Roos-aware macros detect the object or class type and route to the correct implementation.}
  @item{@racket[new*] is a helper macro that transforms arguments into @racket[(v x)] form when needed.}
]

@section{Testing}

The module includes an internal test suite using RackUnit.  
It validates consistent behavior of @racket[send], @racket[->], and @racket[new] across both Racket classes and Roos classes.


@; End of documentation
