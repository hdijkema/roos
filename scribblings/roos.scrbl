#lang scribble/manual

@(require
   scribble/example
   @(for-label roos))

@(define myeval
   (make-base-eval '(require roos)))

@title[#:tag "roos"]{roos}

@author[@author+email["Hans Dijkema" "hans@dijkewijk.nl"]]

@defmodule[roos]
ROOS is a lightweight object-oriented framework for Racket.
It supports class definitions with attributes and methods, multiple inheritance, introspection,
and persistent fields through a user-extensible storage mechanism. All methods and attributes
are virtual, and may be overridden in subclasses.

@section{Class Definition Syntax}

@defform[(def-roos (class-name ...) this (supers ...) body ...)]{
Defines a ROOS class.

@racket[this] is bound to the object under construction.
@racket[supers] refers to instantiated superclass objects.

Each body entry may be:

@itemlist[
 @item{Standard attribute: @racket[(attr val)] — creates getter @racket[attr] and setter @racket[attr!].}
 @item{Persistent attribute: @racket[(persist "Doc" (attr val))] — also stored/restored via storage backend.}
 @item{Documented attribute: @racket[("Doc" (attr val))] — adds inline documentation to attribute.}
 @item{Method: @racket[((method args ...) expr ...)] — defines a public method.}
 @item{Documented method: @racket[("Doc" ((method args ...) expr ...))] — with documentation.}
 @item{Reserved method: @racket[init] and @racket[finalize] are automatically called at creation and finalization.}
]

Methods and fields are always virtual. Superclass definitions are resolved based on declared order. Multiple inheritance is supported and left-to-right linearized.

@racket[def-roos] supports default values, optional documentation, and user-defined persistence.
}

@subsection{Object and Method Use}

@itemlist[
 @item{@racket[(-> obj field)] — call getter for field.}
 @item{@racket[(-> obj field! val)] — set field.}
 @item{@racket[(-> obj method args ...)] — invoke method.}
 @item{@racket[(->> obj name)] — retrieve method/field procedure.}
 @item{@racket[(roos-object? x)] — is it a ROOS object?}
 @item{@racket[(roos-class? x)] — is it a ROOS class definition?}
 @item{@racket[(roos-classname obj)] — symbolic class name.}
 @item{@racket[(roos-class obj)] — class definition.}
 @item{@racket[(roos-id obj)] — unique object ID.}
 @item{@racket[(roos-id! obj id)] — set object's ID (used in persistence).}
]

@subsection{Persistence and Storage Backend}

ROOS lets you persist selected attributes by tagging them with @racket[persist]. Persistence is handled by user-provided backends through:

@racketblock[
(roos-storage! getter setter deleter stop-deleting!)
]

Each function takes a ROOS object and field name:

@itemlist[
 @item{@racket[getter obj field default] — a function that returns stored value or default.}
 @item{@racket[setter obj field val] — a function that stores value.}
 @item{@racket[deleter obj] — a function that removes an object, i.e. all persistent fields for that (unless @racket[stop-deleting] is @racket[#t]).]}
 @item{@racket[stop-deleting! #t] — disables or enables deletion for current session.}
]

See the full SQLite example in the next section.

@subsection{Address Book Example with Persistent Vector of Person IDs}

This example builds an address book with persistent reference to persons, using ROOS' object ID mechanism.

@racketblock[
(def-roos (person) this (supers)
  (persist "Name" (name ""))
  (persist "Phone" (phone "")))

(def-roos (addressbook) this (supers)
  (persist "Person IDs" (ids (vector)))
  (persons (vector))

  ((init)
   (let ((restored (vector-map
                    (lambda (id)
                      (let ((p (-! person)))
                        (roos-id! p id)
                        p))
                    (-> this ids))))
     (set! persons restored)))

  ((add-person p)
   (let ((new-persons (vector-append persons (vector p))))
     (set! persons new-persons)
     (-> this ids! (vector-map roos-id new-persons))))

  ((insert-person-at i p)
   (let* ((before (subvector persons 0 i))
          (after (subvector persons i (vector-length persons)))
          (new-persons (vector-append before (vector p) after)))
     (set! persons new-persons)
     (-> this ids! (vector-map roos-id new-persons))))

  ((remove-person-at i)
   (let* ((before (subvector persons 0 i))
          (after (subvector persons (add1 i) (vector-length persons)))
          (new-persons (vector-append before after)))
     (set! persons new-persons)
     (-> this ids! (vector-map roos-id new-persons))))

  ((all-names)
   (vector->list (vector-map (lambda (p) (-> p name)) persons))))

;; Create sample data
(define ab (-! addressbook))
(roos-id! ab 'addressbook-id)

(define alice (-! person))
(-> alice name! "Alice")
(-> alice phone! "123")
(-> ab add-person alice)

(define bob (-! person))
(-> bob name! "Bob")
(-> bob phone! "456")
(-> ab add-person bob)

(-> ab all-names) ; => '("Alice" "Bob")

;; Reopen addressbook later from persistent storage
(define ab2 (-! addressbook))
(roos-id! ab2 'addressbook-id)
(-> ab2 all-names) ; => '("Alice" "Bob")
]

@bold{Note:} call @racket[(roos-storage-stop-deleting! #t)] before shutdown to prevent finalizers from purging storage content.

@subsection{Cyclic References and Garbage Collection}

ROOS objects can reference each other freely, including circular (cyclic) references.
For example, a doubly-linked list:

@racketblock[
(def-roos (node) this (supers)
  (persist "Value" (val 0))
  (next #f)
  (prev #f))

(define a (-! node))
(-> a val! 1)

(define b (-! node))
(-> b val! 2)

(-> a next! b)
(-> b prev! a)
]

To avoid resource leaks when such cyclic structures are finalized, make sure that any cleanup (e.g. persistence flush) is done in @racket[finalize] methods. Racket's garbage collector can collect cyclic references if there are no external references left.

If persistent fields depend on each other cyclically (e.g. mutual IDs), you may want to:

@itemlist[
 @item{Assign fixed IDs at creation time.}
 @item{Defer construction of cyclic pointers until after all involved objects exist.}
 @item{Use @racket[init] to resolve and wire up these references after restoring from persistent state.}
]

Cyclic references are supported and safe as long as your finalization logic handles them properly.
