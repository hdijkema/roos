#lang scribble/manual

@(require
   scribble/example
   @(for-label roos))

@(define myeval
   (make-base-eval '(require roos)))

@title[#:tag "roos"]{roos}

@author[@author+email["Hans Dijkema" "hans@dijkewijk.nl"]]


@defmodule[roos]
ROOS as a simple OO framework that can be used to create objects have methods and attributes.
It has a simple form of inheritance. All methods and attributes are "virtual", i.e.
if called from a base class, and re-declared in a derived roos "class", the derived attribute or
method will be used.

@defform[(def-roos (class-name ...) this supers
               (attribute-i value-i)
               ...
               ((method-j ...) expr ...)
               )]
Defines a class with name @code{class-name}. @code{this} refers to the instantiated object of class @code{class-name}, @code{supers} refers to the possible instantiated super classes of @code{class-name}. @code{attribute-i} defines an attribute. It  will create a getter, named @code{attribute-i}, and a setter, named @code{attribute-i!}. @code{method-j} defines a method.

@defform[(-> obj name ...)]
Calls a method or getter/setter of obj.

@defform[(roos-class? var)]
Returns @code{#t}, if var is a defined roos class; @code{#f}, otherwise.

@defform[(roos-object? var)]
Returns @code{#t}, if var is a variable instantiated by a roos class; @code{#f}, otherwise.

@defform[(roos-classname var)]
Returns the name (as symbol) of the defined roos class, or of the class of a roos object, if var is an instantiated class; @code{#f}, otherwise.

@defform[(roos-class var)]
Returns the defined roos class of an instantiated roos class if @code{roos-object?} returns @code{#t}; @code{#f}, otherwise


@examples[(require roos)
          (def-roos (a x) this (supers)
                (y ( + x 4))
                ((g a) (* a (-> this y))))
          
          (def-roos (b1) this (supers (-* a 6))
                ((v . a) (if (null? a)
                             (-> supers y)
                             (begin
                               (-> supers y! (car a))
                               (-> supers y))))
                (y 55))
          
          (def-roos (b2) this (supers (-* a 5))
                ((v2) (-> supers y))
                ((v2*) (-> this y)))
          
          (def-roos (c) this (supers (-* b1) (-* b2))
                ((zy) (-> supers y))
                ((z1) (-> supers v))
                ((z2) (-> supers v2))
                (y -1))
          
          (define-syntax :
            (syntax-rules ()
              ((_ c d ...)
               c)))
          
          (define bb (-* b1))
          
          (: (-> bb g 2)      "(-> bb g 2) Will return the value of (* 2 y of class b1)")
          (: (-> bb y! 7)     "(-> bb y! 7) Will set y in class b1 to 7")
          (: (-> bb g 6)      "(-> bb g 6) Will return 42")
          (: (-> bb v)        "(-> bb v) Will return the value of y in class a")
          (: (-> bb v 42)     "(-> bb v 42) Will set the value of y in class a to 42")
          (: (-> bb y)        "(-> bb y) Will return the value of y in class b1, i.e. 7")
          (: (-> bb v)        "(-> bb v) Will return the value of y in class a, i.e. 42")

          (define cc (c))
          (: (-> cc zy)       "(-> cc zy) Will return the value of y in super class b1")
          (: (-> cc y! 88)    "(-> cc y! 88) Will set the value of y in class c")
          (: (-> cc zy)       "(-> cc zy) Will return the value of y in super class b1")
          (: (-> cc z1)       "(-> cc z1) Will return the value of y in the super class of b1, which will be (+ 4 6) = 10")
          (: (-> cc z2)       "(-> cc z2) Will return this value of y in the super class of b2, which will be (+ 4 5) = 9")
          (: (-> cc v2*)      "(-> cc v2*) Will return the value of y in class c")
          
          ]
                            
