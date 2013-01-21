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

(in-test-group
 properties

 (define-each-check
  (= 4 (increment 3))
  (= 2 (decrement 3))
  (= 10 (sum '(1 2 3 4)))
  (= 24 (product '(1 2 3 4)))
  (= 24 (factorial 4))
  (factorial? 24)
  (not (factorial? 25))
  (not (factorial? 8))
  (equal? '(1 2 6 24) (stream-take->list (the-factorials) 4))
  (= 55 (fibonacci 10))
  (fibonacci? 55)
  (not (fibonacci? 70))
  (= -55 (fibonacci -10))
  (= 34 (fibonacci -9))
  (fibonacci? 1)
  (= 1 (fibonacci-root 1)) ; Not 2, even though next example
  (= 1 (fibonacci 2))
  (= 3 (fibonacci-root 2))
  (= 0 (count-fibonaccis 1 1)) ; Exclusive of upper bound
  (= 2 (count-fibonaccis 1 2)) ; 1,1 counts as two Fibonaccis
  (= 4 (count-fibonaccis 1 4)) ; 1,1 counts as two Fibonaccis

  (= 2 (smallest-divisor 6))
  (= 5 (smallest-divisor 175))
  (prime? 17)
  (not (prime? 16))
  (= 11 (prime 5))
  (not (composite? 17))
  (composite? 16)
  (semiprime? 10)
  (not (semiprime? 7))
  (not (semiprime? 175))
  (semiprime? 4) ; Multiplicity counts
  (square-free? 6)
  (not (square-free? 12))
  (equal? '(2 2 2 3) (prime-factors 24))
  (equal? '(5 5 7) (prime-factors 175))
  (equal? '(1 2 3 4 5 6 10 12 15 20 30 60) (divisors 60))
  (equal? '(1 2 4 5 10 20 25 50 100) (divisors 100))
  (equal? '(1 2 3 4 5 6 10 12 15 20 30) (proper-divisors 60))
  (equal? '(1) (proper-divisors 101))
  (= 108 (sigma 60))
  (perfect? 6)
  (perfect? 28)
  (not (perfect? 27))
  (= 28 (perfect 2))
  (= 496 (perfect 3))
  (equal? '(6 28 496) (stream-take->list (the-perfects) 3))
  (abundant? 60)
  (not (abundant? 7))
  (not (abundant? 6))
  (equal? '(12 18 20 24) (stream-take->list (the-abundants) 4))
  (not (deficient? 60))
  (deficient? 7)
  (not (deficient? 6))
  (= 25 (deficient 20))
  (amicable? 220)
  (not (amicable? 221))
  (not (amicable? 1))
  (equal? '(220 284) (stream-take->list (the-amicables) 2))
  (aspiring? 25)
  (not (aspiring? 26))
  (not (aspiring? 276)) ; Well, we don't really know...
  (equal? '(25 95 119 143) (stream-take->list (the-aspirings) 4))
  (= 2047 (mersenne 5))
  (mersenne? 2047)
  (not (mersenne? 2000))
  (= 2310 (primorial 5))
  (primorial? 2310)
  (not (primorial? 2000))
  (= 17280 (compositorial 5))
  (compositorial? 17280)
  (not (compositorial? 17000))

  (= 36 (square 6))
  (square? 36)
  (not (square? 35))
  (not (square? 0)) ; Properties are only defined on positive integers
  (square? (expt 5 200))
  (not (square? (+ (expt 5 200) 1)))
  (= 27 (cube 3))
  (cube? 27)
  (not (cube? 26))
  (= 2 (cube-root 8))
  (< 2 (cube-root  9) 3)
  (< 2 (cube-root 10) 3)
  (< 2 (cube-root 26) 3)
  (= 15 (triangle 5))
  (triangle? 15)
  (not (triangle? 20))
  (triangle? 21)
  (= 5 (triangle-root 15))
  (< 5 (triangle-root 16) 6)
  (= 5 (count-triangles 3 25))
  (equal? '(1 3 6 10 15) (stream-take->list (the-triangles) 5))
  (equal? '(10 15 21 28 36) (stream-take->list (triangles-from 10) 5))
  (equal? '(15 10 6 3 1) (stream->list (triangles-down-from 15)))
  (equal? '(10 15 21 28 36) (stream->list (triangles-between 9 37)))
  (equal? '(36 28 21 15 10) (stream->list (triangles-between-down 9 37)))
  ;; TODO Other figurate numbers

  (equal? '(1 2 3 4) (number->digits 1234))
  (equal? '(1 0 1) (number->bits 5))
  (= 1234 (digits->number '(1 2 3 4)))
  (pandigital? 1234567890)
  (not (pandigital? 3257454))
  (= 3 (bitcount 7))
  (evil? 6)
  (not (evil? 7))
  (odious? 7)
  (not (odious? 6))
  (multidigit? 23)
  (not (multidigit? 5))
  (palindrome? 1)
  (palindrome? 55)
  (palindrome? 141)
  (not (palindrome? 142))
  (emirp? 17)
  (emirp? 71)
  (not (emirp? 53))
  (emirpimes? 15)
  (emirpimes? 51)
  (not (emirpimes? 17))
  (not (emirpimes? 91))
  (strobogrammatic? 609)
  (strobogrammatic? 619)
  (not (strobogrammatic? 99))
  (apocalyptic-power? 157)
  (not (apocalyptic-power? 158))
  (smith? 22)
  (not (smith? 21))
  (hoax? 22)
  (not (hoax? 21))
  (= 111 (repunit 3))
  (repunit? 111)
  (not (repunit? 112))
  (equal? '(1 11 111 1111) (stream-take->list (the-repunits) 4))
  (repdigit? 333)
  (repdigit? 444)
  (not (repdigit? 454))
  (undulating? 343434)
  (not (undulating? 343437))
  (equal? '(101 111 121) (stream-take->list (the-undulatings) 3))))
