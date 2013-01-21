;;; This file is part of Integer Sequences, a library for recreational
;;; number theory in MIT Scheme.
;;; Copyright 2007-2009 Alexey Radul.
;;;
;;; Integer Sequences is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Affero General Public License
;;; as published by the Free Software Foundation; either version 3 of
;;; the License, or (at your option) any later version.
;;; 
;;; This code is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU Affero General Public
;;; License along with Integer Sequences; if not, see
;;; <http://www.gnu.org/licenses/>.

(declare (usual-integrations))

;;;; Mechanisms for operating on various views of integer sequences.

;;; The two common ways that integer sequences are defined are by
;;; giving a predicate to test membership in the sequence (and letting
;;; the sequence be monotonically increasing) or by giving a mechanism
;;; to compute the nth element of the sequence (a recurrence can be
;;; seen as a special case of such a mechanism).  One wants, however,
;;; to be able to carry out various operations on sequences,
;;; regardless of how they are defined, including generating
;;; individual elements by index, constructing a (possibly bounded)
;;; stream of elements, testing whether a number is an element, and,
;;; if it is, finding out its index.

;;; These operations can all be mechanically constructed from any of
;;; the means of defining the sequence, and certain assumptions about
;;; the sequence.  A summary of the operations, relationships, and
;;; means of generating operations from other operations are laid out
;;; in the accompanying figure numbers-meta.fig.  The code below
;;; implements these transformations, assuming that all sequences are
;;; from Z+ --> Z+, infinite, and strictly monotonically increasing.

;;; This file culminates in the definition of the integer-sequence
;;; macro which constructs any missing sequence operations and defines
;;; them in the environment according to the naming convention given
;;; in the README file.  In addition, integer-sequence defines the
;;; sequence meta object

;;; Operation      Name                Function
;;; ...            ...                 ...
;;; meta-object    foo-seq             the meta object, below

;;; TODO Completeness: Incorporate a theory of recurrences?

;;; TODO Performance: Here's what the counter->inverter fiddle looks
;;; like:
;; (define (counter->inverter counter)
;;   (lambda (n)
;;     (+ (counter 0 n) ; Exclusive of n
;;        (let ((n-count (counter n (+ n 1)))) ; Exclusive of n+1
;;          (if (= n-count 0)
;;              1/2 ; n is not not an element
;;              n-count)))))
;;; test it for correctness and performance and fit it in.  It could be
;;; useful for going from tester->ranger->counter->inverter instead
;;; of tester->streamer->generator->inverter

;;; TODO Performance: collapse appropriate compositions of arrows
;;; (notably the-foos->foo->foo-root and foo-root->foo->streams could
;;; shave off a log factor); also, I can avoid allocating the promises
;;; that the streams would generate by doing loop fusion.

;;; TODO Possible, as yet unimplemented extensions: non-decreasing
;;; sequences, finite sequences, negative numbers?, and non-monotonic
;;; sequences?

;;; Finite sequences could be represented by (foo k) returning a
;;; distinguished (exact!) +inty object for over-large k.  (foo-root
;;; +infty) should return the maximum legal index plus 1/2.  The
;;; streams would need to be careful to terminate at the end of the
;;; sequence.

;;; Negative numbers introduce non-monotonicities (square, fibonacci)

;;;; Integer Inverses

;;; As a reminder from the README, the definition of an integer
;;; inverse is
;;;
;;;   Define an _integer inverse_ of a monotonic function f: Z+ --> Z+
;;;   to be any function g: Z+ --> Q+ such that, for each n, either
;;;
;;;   - g(n) is an integer and f(g(n)) = n, or
;;;   - g(n) is not an integer and f(floor(g(n))) < n < f(ceiling(g(n))),
;;;     where we formally take f(0) = 0 to cover the case where n < f(1).
;;;
;;; The two procedures below implement two ways to derive such a g
;;; given an f (which is presumed, but not tested, to be monotonic).
(define (invert-by-counting f)
  (lambda (x)
    (let loop ((i 1))
      (let ((f-of-i (f i)))
	(cond ((= f-of-i x) i)
	      ((> f-of-i x) (- i 1/2))
	      (else (loop (+ i 1))))))))

;; TODO How, if at all, do I want this to deal with negative numbers?
;; e.g. (fibonacci? -21) (which is arguably true)
(define (invert-by-binary-search f)
  (lambda (x)
    (let loop ((lower 0)
	       (upper #f))
      ;; Invariant: (f lower) < x <= (f upper)
      (let ((mid (if (number? upper)
		     (floor (/ (+ lower upper) 2))
		     (max (* 2 lower) (+ lower 1)))))
	(if (= lower mid)
	    (if (= x (f upper))
		upper		      ; which is (+ lower 1)
		(- upper 1/2))
	    (if (<= x (f mid))
		(loop lower mid)
		(loop mid upper)))))))

;;; Also, in some cases, more efficient methods are possible, relying,
;;; for example, on Newton's Method.  Unfortunately, it is not clear
;;; how to determine automatically which cases those are.

;;;; The Integers

;;; The sequence of integers is basic enough that we need it
;;; explicitly.

(define (integer n) n)

(define (integer-root n) n)

;;; integer? is already in the system and already does what I want

(define (count-integers lower upper)
  (- upper lower))

(define (the-integers)
  (integers-from 1))

(define (integers-from low)
  (stream-unfold low increment))

(define (integers-down-from high)
  (stream-unfold high decrement (lambda (n) (<= n 0))))

;;; Ranges are inclusive of where you start and exclusive of where you
;;; stop.
(define (integers-between low high)
  (stream-unfold low increment (lambda (n) (>= n high))))

(define (integers-between-down low high)
  (stream-unfold high decrement (lambda (n) (<= n low))))

;;;; The Single Steps
 
;;; These are the single arrows in the diagram
;;; numbers-meta-implemented.png (derived from numbers-meta.fig).  The
;;; box in the diagram singles out operations that are so similar that
;;; interactions with the set of them are collapsed into single arrows
;;; in the diagram; here they appear as separate functions.

(define (generator->inverter generator)
  (invert-by-binary-search generator))

(define (inverter->tester inverter)
  (lambda (number)
    (integer? (inverter number))))

(define (tester->up-ranger tester)
  (lambda (lower upper)
    (stream-filter tester (integers-between lower upper))))

(define (tester->down-ranger tester)
  (lambda (lower upper)
    (stream-filter tester (integers-between-down lower upper))))

(define (tester->up-streamer tester)
  (lambda (lower)
    (stream-filter tester (integers-from lower))))

(define (down-ranger->down-streamer down-ranger)
  (lambda (upper)
    (down-ranger 0 upper)))

(define (up-streamer->streamer up-streamer)
  (lambda ()
    (up-streamer 1)))

(define (streamer->generator streamer)
  (lambda (n)
    (stream-car (stream-drop (streamer) (- n 1)))))

(define (generator->streamer generator)
  (lambda ()
    (stream-map generator (the-integers))))

(define (streamer->up-streamer streamer)
  (lambda (lower)
    (stream-drop-while (lambda (n) (< n lower))
		       (streamer))))

(define (up-streamer->up-ranger up-streamer)
  (lambda (lower upper)
    (stream-take-while (lambda (n) (< n upper))
		       (up-streamer lower))))

(define (up-ranger->down-ranger up-ranger)
  (lambda (lower upper)
    ;; Adding 1 to fix the inclusivity
    (stream-reverse (up-ranger (+ lower 1) (+ upper 1)))))

(define (up-ranger->tester up-ranger)
  (lambda (number)
    (stream-pair? (up-ranger number (+ number 1)))))

(define (down-ranger->tester down-ranger)
  (lambda (number)
    (stream-pair? (down-ranger (- number 1) number))))

(define (generator+inverter->up-streamer generator inverter)
  (lambda (low)
    (stream-map generator (integers-from (ceiling (inverter low))))))

(define (generator+inverter->up-ranger generator inverter)
  (lambda (lower upper)
    (stream-map generator (integers-between (ceiling (inverter lower))
					    (ceiling (inverter upper))))))

(define (generator+inverter->down-ranger generator inverter)
  (lambda (lower upper)
    (stream-map generator (integers-between-down (floor (inverter lower))
						 (floor (inverter upper))))))

(define (up-ranger->counter up-ranger)
  (lambda (lower upper)
    (stream-count (lambda (x) #t) (up-ranger lower upper))))

(define (inverter->counter inverter)
  ;; There are (- (ceiling (inverter upper)) 1) of them that are
  ;; strictly less than upper; I want to subtract from them the
  ;; number that are strictly less than lower.
  (lambda (lower upper)
    (- (ceiling (inverter upper))
       (ceiling (inverter lower)))))

;;;; Meta Objects

;;; The meta objects keep track of the implementations of the
;;; operations for each individual sequence.  They store the
;;; procedures that perform those operations, and allow access
;;; to each possible operation for each sequence.

;;; The other major purpose of meta objects is to facilitate a
;;; sensible automatic construction of derived operations from the
;;; available ones.  Since in principle any operation can be derived
;;; from any reasonably definitional operation by any of several
;;; routes, at greatly varying cost in performance, maintaining
;;; metadata and trying the derivations in a sensible order is
;;; valuable.  N.B.: Not all of the implemented transformations are
;;; tried by this automatic system; the ones that are are summarized
;;; in numbers-meta.png (derived from numbers-meta.fig).

(define-structure
  (seq keyword-constructor)
  generator
  inverter
  tester
  counter
  streamer
  up-streamer
  down-streamer
  up-ranger
  down-ranger)

(define (construct-seq . args)
  (complete-seq! (apply make-seq args)))

;; I would rather that this structure definition were internal to
;; complete-seq! below, but MIT Scheme didn't like that idea.
(define-structure
  transform
  source
  target
  action)

(define (complete-seq! seq)
  "Completes (destructively) a sequence metaobject by filling in all
empty slots with closures derived, by some path through the
numbers-meta diagram, from the available operations already in the
object.  This is done by repeatedly trying the various operations, in
a relatively sensible order, until they produce no more change."
  ;; Heh.  A propagator network would do a much better job of this
  ;; (especially if I taught it how to prefer one procedure over
  ;; another by runtime).
  (define source transform-source)
  (define target transform-target)
  (define action transform-action)
  (define-syntax transform-of
    (syntax-rules ()
      ((_ symbol)
       (parse-transform 'symbol symbol))))
  (define (parse-transform symbol procedure)
    (let* ((the-name (symbol->string symbol))
	   (index-of-> (string-search-forward "->" the-name)))
      (make-transform
       (string->symbol (string-head the-name index-of->))
       (string->symbol (string-tail the-name (+ index-of-> 2)))
       procedure)))
  (define-syntax transforms
    (syntax-rules ()
      ((_ form ...)
       (list (transform-of form) ...))))
  (define the-transforms
    ;; In order of increasing cost
    (transforms
     inverter->tester
     inverter->counter
     generator->streamer

     down-ranger->down-streamer
     up-streamer->streamer
     up-streamer->up-ranger
     up-ranger->tester
     down-ranger->tester

     ;; TODO Binary transforms go here

     generator->inverter

     ;; TODO Order of these four
     streamer->up-streamer
     tester->up-ranger
     tester->down-ranger
     tester->up-streamer

     streamer->generator
     ))

  (define (seq-get field-symbol)
    ((record-accessor rtd:seq field-symbol) seq))
  (define (seq-set! field-symbol value)
    ((record-modifier rtd:seq field-symbol) seq value))
  (define (applicable? transform)
    (and (procedure? (seq-get (source transform)))
	 (not (procedure? (seq-get (target transform))))))
  (define (do-it! transform)
    (seq-set! (target transform)
	      ((action transform)
	       (seq-get (source transform)))))

  (let loop ((transforms-to-try the-transforms))
    (if (not (null? transforms-to-try))
	(if (applicable? (car transforms-to-try))
	    (begin (do-it! (car transforms-to-try))
		   (loop the-transforms))
	    (loop (cdr transforms-to-try)))
	seq)))

(define integer-seq
  (construct-seq
   'generator integer 'inverter integer-root 'tester integer? 'counter count-integers
   'streamer the-integers 'up-streamer integers-from 'down-streamer integers-down-from
   'up-ranger integers-between 'down-ranger integers-between-down))

;;;; Integer Sequences

;;; This macro implements the naming convention for the sequence
;;; operations and defines the appropriately named meta-object and
;;; functions.  For example, writing
;;; 
;;; (define (square n)
;;;   (* n n))
;;; (integer-sequence square generator)
;;; 
;;; will define appropriate functions square-root, square?,
;;; count-squares, the-squares, squares-from, squares-from-down,
;;; squares-between, and squares-between-down, and define square-seq
;;; to be a metaobject for the square sequence.
;;; 
;;; The first argument of the macro call is the name of the sequence
;;; to be defined; the subsequent arguments are the names of the
;;; avaiable operations, which the expansion will appropriately 
;;; pluck from the environment and use.  So, for another example,
;;; one might
;;; 
;;; (define (prime? n)
;;;   ...)
;;; (integer-sequence prime tester)  ; Not (integer-sequence prime? ...)
;;; 
;;; for the prime numbers.

(define-syntax integer-sequence
  (sc-macro-transformer
   (lambda (form use-env)
     (define naming-convention
       `((generator     . ,(lambda (foo) foo))
	 (inverter      . ,(lambda (foo) (symbol foo '-root)))
	 (tester        . ,(lambda (foo) (symbol foo '?)))
	 (counter       . ,(lambda (foo) (symbol 'count- foo 's)))
	 (streamer      . ,(lambda (foo) (symbol 'the- foo 's)))
	 (up-streamer   . ,(lambda (foo) (symbol foo 's-from)))
	 (down-streamer . ,(lambda (foo) (symbol foo 's-down-from)))
	 (up-ranger     . ,(lambda (foo) (symbol foo 's-between)))
	 (down-ranger   . ,(lambda (foo) (symbol foo 's-between-down)))))
     (define base-name (cadr form))
     (define available-operations (cddr form))
     (define seq-object-name (symbol base-name '-seq))
     (define seq-construction-args
       (apply append (map (lambda (operation)
			    (list `(quote ,operation)
				  ((cdr (assq operation naming-convention))
				   base-name)))
			  available-operations)))
     (define definee-namers
       (filter (lambda (clause)
		 (not (member (car clause) available-operations)))
	       naming-convention))
     `(begin
	(define ,seq-object-name
	  (construct-seq ,@seq-construction-args))
	,@(map (lambda (operation-type operation-namer)
		 `(define ,(operation-namer base-name)
		    ((record-accessor rtd:seq ',operation-type)
		     ,seq-object-name)))
	       (map car definee-namers)
	       (map cdr definee-namers))))))
