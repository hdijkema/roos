#lang scribble/manual

@(require
   scribble/example
   @(for-label racket roos))

@(define myeval
   (make-base-eval '(require roos)))

@title[#:tag "roos"]{roos}

@author[@author+email["Hans Dijkema" "hans@dijkewijk.nl"]]


@defmodule[roos]
ROOS as a simple OO framework that can be used to create objects have methods and attributes.
It has a simple form of inheritance. All methods and attributes are "virtual", i.e.
if called from a base class, and re-declared in a derived roos "class", the derived attribute or
method will be used.

@defform[(roos (class-name ...) this supers
               (attribute-i value-i)
               ...
               ((method-j ...) expr ...)
               )]
Defines a class with name @code{class-name}. @code{this} refers to the instantiated object of class @{class-name}, @code{supers} refers to the possible instantiated super classes of @code{class-name}. @code{attribute-i} defines an attribute. It  will create a getter, named @code{attribute-i}, and a setter, named @code{attribute-i!}. @code{method-j} defines a method.

@defform[(-> obj name ...)]
Calls a method or getter/setter of obj.

@examples[#:eval eval
          (roos (a x) this (supers)
                (y ( + x 4))
                ((g a) (* a (-> this y))))
          (roos (b) this (supers (a))
                ((v . a) (if (null? a)
                             (-> supers y)
                             (begin
                               (-> supers y! (car a))
                               (-> supers y))))
                (y 55))
          (define bb (b))
          (-> bb g 2)]
                            
