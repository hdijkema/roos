#lang info

(define pkg-authors '(hnmdijkema))
(define version "0.8.1")
(define license 'Apache-2.0)
(define collection "roos")
(define pkg-desc "An OO Framework for Racket")

(define scribblings
  '(
    ("scribblings/roos.scrbl" (multi-page) (library) "roos")
    ("scribblings/class.scrbl" (multi-page) (library) "roos/class")
    )
  )

(define deps
  '("base"))

(define build-deps
  '("racket-doc"
    "rackunit-lib"
    "scribble-lib"))
