#lang scribble/manual

@(require
   scribble/example
   @(for-label roos))

@(define myeval
   (make-base-eval '(require roos)))

@title[#:tag "roos"]{Roos OO System}

@author[@author+email["Hans Dijkema" "hans@dijkewijk.nl"]]

@defmodule[roos]
ROOS is simple (perl like) a lightweight object-oriented framework for racket. 
It supports class definitions with attributes and methods, multiple inheritance, introspection,
and persistent fields through a user-extensible storage mechanism. All methods and attributes
are virtual, and may be overridden in subclasses.

See also 
@seclink["top" #:doc '(lib "roos/scribblings/class.scrbl")]{roos/class},
the interoperability macros for roos and racket/class.

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
 @item{Optional: you may define @racket[(init expr ...)] to run code immediately after object construction.}
 @item{Optional: you may define @racket[(finalize expr ...)] to run cleanup logic when object is collected.}
]

Methods and fields are always virtual. Superclass definitions are resolved based on declared order. Multiple inheritance is supported and left-to-right linearized.

@racket[def-roos] supports default values, optional documentation, and user-defined persistence.
}

@section{Object and Method Use}

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

@subsection{Provided procedures}

@defproc[(-> [obj any/c] [name symbol?] ...) any/c]{
Invoke a getter, setter, or method on ROOS object @racket[obj] using name and arguments.
}

@defproc[(->> [obj any/c] [name symbol?]) procedure?]{
Return the method or field procedure named @racket[name] from object @racket[obj].
Useful for higher-order usage.
}

@defproc[(roos-class? [x any/c]) boolean?]{
Returns @racket[#t] if @racket[x] is a ROOS class definition.
}

@defproc[(roos-object? [x any/c]) boolean?]{
Returns @racket[#t] if @racket[x] is an instance of a ROOS object.
}

@defproc[(roos-obj? [x any/c]) boolean?]{
Alias for @racket[roos-object?].
}

@defproc[(roos-class [obj any/c]) any/c]{
Returns the ROOS class of which @racket[obj] is an instance.
}

@defproc[(roos-classname [obj any/c]) symbol?]{
Returns the symbolic class name of @racket[obj].
}

@defproc[(roos-id [obj any/c]) symbol?]{
Returns the persistent unique identifier for @racket[obj].
}

@defproc[(roos-id! [obj any/c] [id symbol?]) void?]{
Sets the persistent unique identifier of @racket[obj] to @racket[id].
}

@defproc[(roos-new [class any/c] [args any/c] ...) any/c]{
Constructs a new ROOS object of the given @racket[class], optionally with arguments.
If the class defines @racket[init], that method is invoked automatically.
}

@defproc[(-! [class any/c] [args any/c] ...) any/c]{
Convenient shorthand for @racket[roos-new]. Also invokes @racket[init] if present.
}

@defproc[(roos-help) void?]{
Prints help information for all known ROOS classes, including attributes and documented methods.
}

@defproc[(with-roos-obj [x any/c]) any/c]{
Convenience wrapper to evaluate expressions in the context of a ROOS object.
}

@defproc[(roos-storage! [getter procedure?] [setter procedure?] [deleter procedure?] [stop-deleting! procedure?]) void?]{
Registers the storage backend for persistent attributes. This must be set before any persistence takes place.
}

@defproc[(roos-storage-stop-deleting! [flag boolean?]) void?]{
Globally disables or enables deletion via the registered deleter procedure. Useful when shutting down.
}

@section{Persistence and Storage Backend}

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

@subsection{Example of persistence backend for roos}

Below is an example SQLite backend implementation that stores fields in a table:

@#reader scribble/comment-reader 
[racketblock 
(require db)
(require racket/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conversion of field values
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (value->string s-expr)
  (let ((o (open-output-string)))
    (write s-expr o)
    (get-output-string o)))

(define (string->value str)
  (let ((o (open-input-string str)))
    (read o)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Database storage backend
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define conn (sqlite3-connect #:database "roos.db" #:mode 'create))
(query-exec conn "CREATE TABLE IF NOT EXISTS store (class TEXT, id TEXT, field TEXT, val TEXT)")

(define stop-deleting? #f)
(define (stop-deleting-fn flag) (set! stop-deleting? flag))

(define (getter obj field default)
  (let ((class (symbol-string (roos-classname obj)))
        (id (symbol->string (roos-id obj)))
        (field (symbol->string field)))
  (let ((count (query-value conn
                            "SELECT count(*) FROM store WHERE class=? AND id=? AND field=?"
                            class id field)))
    (if (= count 0)
        default
        (let ((row (query-row conn
                              "SELECT val FROM store WHERE class=? AND id=? AND field=?"
                              class id field)))
          (if row
              (string->value (vector-ref row 0))
              default))))))

(define (setter obj field val)
  (let ((class (symbol->string (roos-classname obj)))
        (id    (symbol->string (roos-id obj)))
        (fld   (symbol->string field))
        (vstr  (value->string val)))
    (query-exec conn "DELETE FROM store WHERE class=? AND id=? AND field=?" class id fld)
    (query-exec conn "INSERT INTO store (class, id, field, val) VALUES (?, ?, ?, ?)" class id fld vstr)))

(define (deleter obj)
  (unless stop-deleting?
    (let ((class (symbol->string (roos-classname obj)))
          (id    (symbol->string (roos-id obj))))
      (query-exec conn "DELETE FROM store WHERE class=? AND id=?" class id))))

(roos-storage! getter setter deleter stop-deleting-fn)

(plumber-add-flush! (current-plumber)
                    (lambda (x)
                      (printf "Collecting garbage to cleanup the storage for variables that have been cleared\n")
                      (collect-garbage)))             
]


@subsection{Address Book Example with Persistent Vector of Person IDs}

This example builds an address book with persistent reference to persons, using ROOS' object ID mechanism.

@#reader scribble/comment-reader 
[racketblock
(require racket/vector)

;; person class
(def-roos (person) this (supers)
  (persist (name ""))
  (persist (tel "")))

;; book class
(def-roos (book) this (supers)
  (persist (ids (list)))
  (persist (name ""))
  ("" (persons (make-vector 0)))

  ((add p)
   (set! persons (vector-extend persons (+ (vector-length persons) 1) p))
   (-> this ids! (vector->list (vector-map (lambda (o) (-> o roos-id)) persons))))

  ((remove i)
   (set! persons (vector-append (vector-take persons i) (vector-drop persons (+ i 1))))
   (-> this ids! (vector->list
                    (vector-map (lambda (o) (-> o roos-id)) persons))))

  ((for-each f) 
   (letrec ((g (lambda (i n)
                 (when (< i n)
                   (f (vector-ref persons i))
                   (g (+ i 1) n)))))
     (g 0 (vector-length persons))))

  (init (begin
            (-> this roos-id! 'book)
            (let ((ps (map (lambda (id)
                             (let ((p (roos-new person)))
                               (-> p roos-id! id)
                               p))
                           (-> this ids))))
              (set! persons (list->vector ps)))))
  )
   
;; Create sample data
(define b (-! book))

(define (adder n t)
  (let ((p (-! person)))
    (-> p name! n)
    (-> p tel! t)
    (-> b add p)))

(adder "Alice" "123")
(adder "Bob" "456")
(adder "Jos" "982")
(adder "Rebecca" "363")

(-> b (for-each (lambda (p) (displayln (-> p name)))))

;; Reopen addressbook later from persistent storage
(define a (-! book))
(-> b (for-each (lambda (p) (displayln (-> p name)))))
  
]

@bold{Note:} call @racket[(roos-storage-stop-deleting! #t)] before shutdown to prevent finalizers from purging storage content.

@section{Cyclic References and Garbage Collection}

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
