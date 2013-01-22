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

;;;; Some integer sequences

;;; These are definitions for a bunch of integer sequences and the
;;; helpers needed to compute them; see numbers-meta.scm for a
;;; description for the sequence facilities and, in particular, the
;;; integer-sequence macro that defines them.

;;; You will notice in this file that some procedures are defined
;;; twice.  If so, the first is meant to be read definitionally and
;;; the second implementationally: one defines the meaning of the
;;; name, whereas the other provides a decent mechanism of computing
;;; whatever the first defines.  The latter will, of course, be the
;;; one executed, because its definition clobbers the former in the
;;; Scheme image.

;;;; Basics

(define (increment n)
  (+ n 1))

(define (decrement n)
  (- n 1))

(define (sum numbers)
  (apply + numbers))

(define (product numbers)
  (apply * numbers))

(define (even k) (* 2 k))
(define (even-root n) (/ n 2))
; The system even? is exactly what I want
(integer-sequence even generator inverter tester)

(define (odd k) (- (* 2 k) 1))
(define (odd-root n) (/ (+ n 1) 2))
; The system odd? is exactly what I want
(integer-sequence odd generator inverter tester)

;;;; Combinatorics

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(define (factorial n)
  (let loop ((n n)
	     (accum 1))
    (if (= n 0)
	accum
	(loop (- n 1) (* accum n)))))
(integer-sequence factorial generator)

(define (choose k n)
  "Choose k out of n"
  (/ (factorial n)
     (* (factorial k)
	(factorial (- n k)))))

(define (choose k n)
  "Choose k out of n"
  ;; This version comprises three performance transformations.  First,
  ;; canceling the larger factorial in the denominator saves a bunch
  ;; of multiplying.  Second, interleaving multiplications with
  ;; divisions keeps computations out of bignums more.  Third,
  ;; counting the denominator up (as opposed to down) avoids rational
  ;; arithmetic (because the intermediate answer is always some
  ;; binomial coefficient).
  (if (> k (/ n 2))
      (choose (- n k) n)
      (let loop ((answer 1)
                 (next-factor (+ (- n k) 1))
                 (next-divisor 1))
        (if (> next-divisor k)
            answer
            (loop (/ (* answer next-factor) next-divisor)
                  (+ next-factor 1)
                  (+ next-divisor 1))))))

(define (catalan k)
  (/ (choose k (* 2 k)) (+ k 1)))
(integer-sequence catalan generator)

(define (distribute num-objects num-buckets)
  "Distribute exactly n identical objects among k buckets."
  ;; Consider a sequence of n+k-1 slots, exactly k-1 of which must be
  ;; bucket separators, and the remaining n must be the objects.
  ;; Distributions of the objects into the buckets are in one-to-one
  ;; correspondence with such sequences.  There are (choose k-1 out of
  ;; n+k-1) such sequences.
  (choose (- num-buckets 1) (+ num-objects (- num-buckets 1))))

;;;; Fibonacci

(define (fibonacci n)
  (cond ((> n 0) (fibonacci+ n))
	((= n 0) 0)
	((< n 0) (* -1
		    (fibonacci+ (- n))
		    (expt -1 (- n))))))
(integer-sequence fibonacci generator)

(define (fibonacci+ n)
  (let loop ((a 1) (b 1) (count 1))
    (if (<= n count)
	a
	(loop b (+ a b) (+ count 1)))))

;; This version, using faster matrix exponentiation is about 6.5 times
;; faster than fibonacci+, but I'm not sure that justifies the extra
;; complexity.
(define (fibonacci+by-matrix n)
  (define (mat-* mat1 mat2)
    (let ((a1 (car mat1))
	  (b1 (cadr mat1))
	  (c1 (caddr mat1))
	  (d1 (cadddr mat1))
	  (a2 (car mat2))
	  (b2 (cadr mat2))
	  (c2 (caddr mat2))
	  (d2 (cadddr mat2)))
      (list (+ (* a1 a2) (* b1 c2))
	    (+ (* a1 b2) (* b1 d2))
	    (+ (* c1 a2) (* d1 c2))
	    (+ (* c1 b2) (* d1 d2)))))
  (define (mat-expt mat n)
    (cond ((= 0 n) (list 1 0 0 1))
	  ((= 1 n) mat)
	  ((even? n)
	   (let ((ans (mat-expt mat (/ n 2))))
	     (mat-* ans ans)))
	  ((odd? n)
	   (mat-* (mat-expt mat (- n 1))
		  mat))))
  (cadr (mat-expt (list 0 1 1 1) n)))

;;;; Factorizations

(define (divides? d n)
  (= 0 (remainder n d)))

(define (smallest-divisor number #!optional start-from)
  (define (helper number start-from)
    (cond ((divides? start-from number)
	   start-from)
	  ((> (* start-from start-from) number)
	   number)
	  (else
	   (helper number (+ start-from 1)))))
  (if (default-object? start-from)
      (set! start-from 2))
  (helper number start-from))

(define (prime? number)
  (and (> number 1)
       (= number (smallest-divisor number))))
(integer-sequence prime tester)

(define (composite? number)
  (and (not (= 1 number))
       (not (prime? number))))
(integer-sequence composite tester)

(define (semiprime? number)
  (= 2 (length (prime-factors number))))
(integer-sequence semiprime tester)

(define (twin-prime? number)
  (and (prime? number)
       (or (prime? (+ number 2))
           (prime? (- number 2)))))
(integer-sequence twin-prime tester)

(define (square-free? number)
  (let ((factors (prime-factors number)))
    (= (length factors) (length (delete-duplicates factors)))))
(integer-sequence square-free tester)

(define (powerful? number)
  (define (all-at-least-twice lst)
    (if (null? lst)
        #t
        (count (cdr lst) (car lst) 1)))
  (define (count lst item ct)
    (if (and (pair? lst) (= (car lst) item))
        (count (cdr lst) item (+ ct 1))
        (if (>= ct 2)
            (all-at-least-twice lst)
            #f)))
  (all-at-least-twice (prime-factors number)))
(integer-sequence powerful tester)

(define (mersenne k)
  (- (expt 2 (prime k)) 1))
(integer-sequence mersenne generator)

(define (primorial number)
  (product (stream-take->list (the-primes) number)))
(integer-sequence primorial generator)

(define (compositorial number)
  (product (stream-take->list (the-composites) number)))
(integer-sequence compositorial generator)

;;;; Divisors

(define (prime-factors number)
  (define (helper number min-divisor)
    (if (<= number 1)
	'()
	(let ((div (smallest-divisor number min-divisor)))
	  (cons div (helper (/ number div) div)))))
  (helper number 2))

(define (all-combinations lst #!optional same-combination)
  (if (default-object? same-combination)
      (set! same-combination eq?))
  (if (null? lst)
      '(())
      (let ((subcombinations (all-combinations (cdr lst))))
        (delete-duplicates
         (append subcombinations
                 (map (lambda (l) (cons (car lst) l)) subcombinations))
         same-combination))))

(define (divisors n)
  (sort (map product (all-combinations (prime-factors n) equal?)) <))

(define (proper-divisors n)
  (filter (lambda (d) (not (= n d)))
	  (divisors n)))

(define (sigma number)
  (sum (proper-divisors number)))

(define (perfect? number)
  (= number (sigma number)))
(integer-sequence perfect tester)

(define (abundant? number)
  (< number (sigma number)))
(integer-sequence abundant tester)

(define (deficient? number)
  (> number (sigma number)))
(integer-sequence deficient tester)

(define (amicable? number)
  (and (> number 1)
       (let ((sum (sigma number)))
	 (and (not (= number sum))
	      (= number (sigma sum))))))
(integer-sequence amicable tester)

(define (aspiring? number)
  (and (not (perfect? number))
       (let loop ((number number)
		  (next (sigma number))
		  (count 0))
	 (cond ((= 1 number)
		#f)
	       ((= number next)
		#t)
	       ;; TODO Is there a better way?
	       ((or (> count 100) (> next 100000))
		#f)
	       (else
		(loop next (sigma next) (+ count 1)))))))
(integer-sequence aspiring tester)

;; As it is, both of these are too slow to be really useful.
#|
 (define (untouchable? number)
   (not (member number (map sigma (iota (square number))))))

 (define (untouchable? number)
   ;; This version short-circuits the sum-of-divisors computations
   (and (> number 1)
        (let ((bound (square number)))
          (let loop ((try 1))
            (cond ((= number (sigma try)) #f)
                  ((>= try bound) #t)
                  (else (loop (+ try 1))))))))
 (integer-sequence untouchable tester)

 (define (weird? number)
   (and (abundant? number)
        (not (member number (map sum (all-combinations (proper-divisors number))) =))))
 (integer-sequence weird tester)
|#

;;;; Figurate numbers

(define (square number)
  (* number number))
(integer-sequence square generator)

(define (cube number)
  (* number number number))
(integer-sequence cube generator)

(define (triangle n)
  (/ (* n (+ n 1)) 2))
(integer-sequence triangle generator)

(define (pentagon k)
  (* k (- (* 3 k) 1) 1/2))
(integer-sequence pentagon generator)

(define (hexagon k)
  (* k (- (* 2 k) 1)))
(integer-sequence hexagon generator)

(define (heptagon k)
  (* k (- (* 5 k) 3) 1/2))
(integer-sequence heptagon generator)

(define (octagon k)
  (* k (- (* 3 k) 2)))
(integer-sequence octagon generator)

(define (nonagon k)
  (* k (- (* 7 k) 5) 1/2))
(integer-sequence nonagon generator)

(define (decagon k)
  (* k (- (* 4 k) 3)))
(integer-sequence decagon generator)

(define (tetrahedron k)
  (* 1/6 k (+ k 1) (+ k 2)))
(integer-sequence tetrahedron generator)

(define (pronic n)
  (* n (+ 1 n)))
(integer-sequence pronic generator)

;;;; Digits

(define (number->digits number #!optional base)
  (define (number->reverse-digits number)
    (if (< number base)
	(list number)
	(cons (remainder number base)
	      (number->reverse-digits (quotient number base)))))
  (if (default-object? base) (set! base 10))
  (reverse (number->reverse-digits number)))

(define digits number->digits)

(define (binary-digits n)
  (digits n 2))

(define number->bits binary-digits)

(define (digits->number digits #!optional base)
  (if (default-object? base)
      (set! base 10))
  (let loop ((digits digits)
	     (total 0))
    (if (null? digits)
	total
	(loop (cdr digits)
	      (+ (car digits) (* base total))))))

(define (automorphic? number)
  (define (is-suffix-of? short long)
    (equal? short (drop long (- (length long) (length short)))))
  (is-suffix-of? (digits number) (digits (square number))))
(integer-sequence automorphic tester)

(define (pandigital? number)
  (= 10 (length (delete-duplicates (number->digits number)))))
(integer-sequence pandigital tester)

(define (bitcount number)
  (length (filter (lambda (bit) (= bit 1)) (number->bits number))))

(define (evil? number)
  (= 0 (remainder (bitcount number) 2)))
(integer-sequence evil tester)

(define (odious? number)
  (= 1 (remainder (bitcount number) 2)))
(integer-sequence odious tester)

(define (multidigit? number)
  (> (length (number->digits number)) 1))

(define (multidigit? number)
  (>= number 10))
(integer-sequence multidigit tester)

(define (apocalyptic-power? number)
  (substring? "666" (number->string (expt 2 number))))
(integer-sequence apocalyptic-power tester)

(define happy-number?
  ;; The trick to computing happy numbers reasonably is to notice that
  ;; after a certain bound, the sum of squares of digits operation
  ;; cannot increase the number.  This means that all cycles involve
  ;; reasonably small integers, and detection thereof can be cached.
  (let* ((bound 200) ; (step k) < (step 199) < 199
         (cache (make-vector bound)))
    (define (step n)
      (sum (map square (digits n))))
    (define (state n)
      (let ((cached (vector-ref cache n)))
        (cond ((or (eq? 'happy cached)
                   (eq? 'sad cached))
               cached)
              ((eq? 'pending cached)
               (vector-set! cache n 'sad)
               'sad)
              ((= 1 n)
               (vector-set! cache n 'happy)
               'happy)
              (else
               (vector-set! cache n 'pending)
               (let ((answer (state (step n))))
                 (vector-set! cache n answer)
                 answer)))))
    (lambda (number)
      (if (>= number bound)
          (happy-number? (step number))
          (eq? 'happy (state number))))))
(integer-sequence happy-number tester)

(define (repunit? number)
  (every (lambda (x) (= x 1)) (number->digits number)))

(define (repunit n)
  (/ (- (expt 10 n) 1) 9))
(integer-sequence repunit generator)

(define (repdigit? number)
  (= 1 (length (delete-duplicates (number->digits number)))))
(integer-sequence repdigit tester)

(define (narcissistic? number)
  (let* ((dig (digits number))
         (k (length dig)))
    (= number (sum (map (lambda (d) (expt d k)) dig)))))
(integer-sequence narcissistic tester)

(define (undulating? number)
  (define (undulating? a b lst)
    (cond ((null? lst)
	   #t)
	  ((not (= a (car lst)))
	   #f)
	  (else (undulating? b a (cdr lst)))))
  (let ((digits (number->digits number)))
    (and (> (length digits) 2)
	 (undulating? (car digits) (cadr digits) (cddr digits)))))
(integer-sequence undulating tester)

(define (palindrome? number)
  (equal? (number->digits number)
	  (reverse (number->digits number))))
(integer-sequence palindrome tester)

;;;; More than just digits

(define (emirp? number)
  (and (prime? number)
       (prime? (digits->number (reverse (number->digits number))))
       (not (palindrome? number))))
(integer-sequence emirp tester)

(define (emirpimes? number)
  (and (semiprime? number)
       (semiprime? (digits->number (reverse (number->digits number))))
       (not (palindrome? number))))
(integer-sequence emirpimes tester)

(define (upside-down-glyph digit)
  (let ((pair (assoc digit '((0 . 0) (1 . 1) (6 . 9) (8 . 8) (9 . 6)))))
    (and pair (cdr pair))))

(define (strobogrammatic? number)
  (equal? (number->digits number)
	  (map upside-down-glyph (reverse (number->digits number)))))
(integer-sequence strobogrammatic tester)

(define (smith? number)
  (and (composite? number)
       (= (sum (number->digits number))
	  (sum (map (lambda (p)
		      (sum (number->digits p)))
		    (prime-factors number))))))
(integer-sequence smith tester)

(define (hoax? number)
  (and (composite? number)
       (= (sum (number->digits number))
	  (sum (map (lambda (p)
		      (sum (number->digits p)))
		    (delete-duplicates (prime-factors number)))))))
(integer-sequence hoax tester)
