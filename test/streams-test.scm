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
 streams

 (define-each-check
   (equal?
    '(1/4 1/3 1/2 1)
    (stream->list 4 (stream-map / (stream 4 3 2 1 0))))
   (equal?
    '(1/4 1/3 1/2)
    (stream->list 3
     (stream-filter (lambda (n)
                      (< n 1))
      (stream-map / (stream 4 3 2 1 0)))))
   (equal?
    '(1 2 3 4 5 6)
    (stream->list (stream-unfold 1 (lambda (x) (+ x 1)) (lambda (x) (>= x 7)))))
   (equal?
    '(2 4 6)
    (stream->list 
     (stream-filter even?
      (stream-unfold 1 (lambda (x) (+ x 1)) (lambda (x) (>= x 7))))))
   (equal?
    '(10001)
    (stream->list 1
     (stream-filter (lambda (n) (> n 10000))
      (stream-unfold 1 (lambda (x) (+ x 1))))))
   (equal?
    '(100001)
    (stream->list 1
     (stream-filter (lambda (n) (> n 100000))
      (stream-unfold 1 (lambda (x) (+ x 1))))))
   (equal?
    '(1 2 3)
    (stream->list
     (stream-take (stream-unfold 1 (lambda (x) (+ x 1))) 3)))))
