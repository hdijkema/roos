#lang scribble/manual

@(require
   scribble/example
   @(for-label roos/gui-class))

@(define myeval
   (make-base-eval '(require roos/class)))


@title[#:tag "top"]{Interoperability Macros for roos and racket/gui}

@author[@author+email["Hans Dijkema" "hans@dijkewijk.nl"]]

@defmodule[roos/gui-class]

This module provides a compatibility layer between the @seclink["roos"  #:doc '(lib "roos/scribblings/roos.scrbl") ]{@racketmodname[roos]} object system and the standard @racketmodname[racket/class] system. It exports the macros @racket[send], @racket[->], and @racket[new], which automatically dispatch to the appropriate implementation based on the type of the given object or class.

This one is specifically for racket/gui. The provided macros are the same as for @seclink["roos/class" #:doc '(lib "roos/scribblings/class.scrbl")].

@; End of documentation
