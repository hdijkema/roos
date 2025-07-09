#lang info

(define pkg-authors '(hnmdijkema))
(define version "0.8.2")
(define license 'Apache-2.0)
(define collection "roos")
(define pkg-desc "A Simple (perl like) OO system for racket")

(define scribblings
  '(
    ("scribblings/roos.scrbl" () (library) "roos")
    ("scribblings/class.scrbl" () (interop) "roos-class")
    )
  )

(define deps
  '("base"))

(define build-deps
  '("racket-doc"
    "rackunit-lib"
    "scribble-lib"))
