;;; -*- Mode: Scheme -*-
;;; This file is a supporting library for Integer Sequences, a library
;;; for recreational number theory in MIT Scheme.
;;; Copyright 2007 Alexey Radul, Taylor Campbell, and Yu-hsin Chen.
;;; 
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;; 
;;; Integer Sequences itself can be redistributed and/or modified under
;;; the terms of the GNU Affero General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;; 
;;; You should have received a copy of the GNU Affero General Public
;;; License along with Integer Sequences.  If not, see
;;; <http://www.gnu.org/licenses/>.

(declare (usual-integrations))

;;;; Even Streams

(define-syntax stream-cons
  (syntax-rules ()
    ((stream-cons a d)
     (delay (cons a d)))))

(define (stream-pair? stream) (pair? (force stream)))
(define (stream-null? stream) (null? (force stream)))

(define stream-nil (delay '()))

(define (stream-car stream) (car (force stream)))
(define (stream-cdr stream) (cdr (force stream)))

(define (stream-map procedure stream)
  (lazy
   (if (stream-pair? stream)
       (stream-cons (procedure (stream-car stream))
                    (stream-map procedure (stream-cdr stream)))
       stream-nil)))

(define (stream-filter predicate stream)
  (let recur ((stream stream))
    (lazy
     (if (stream-pair? stream)
         (let ((item (stream-car stream))
               (recur (lambda () (recur (stream-cdr stream)))))
           (if (predicate item)
               (stream-cons item (recur))
               (recur)))
         stream-nil))))

;;; TODO Does this version screw up anything the previous version gets
;;; right?  I know it fixes the long-string-of-falses problem in the
;;; absence of iterative forcing.  Maybe I should ask Taylor.
(define (stream-filter predicate stream)
  (lazy
   (let loop ((stream stream))
     (if (stream-pair? stream)
	 (if (predicate (stream-car stream))
	     (stream-cons (stream-car stream)
			  (stream-filter predicate (stream-cdr stream)))
	     (loop (stream-cdr stream)))
	 stream-nil))))

(define (stream-filter-map procedure stream)
  (let recur ((stream stream))
    (lazy
     (if (stream-pair? stream)
         (let ((item (stream-car stream))
               (recur (lambda () (recur (stream-cdr stream)))))
           (cond ((procedure item)
                  => (lambda (item*)
                       (stream-cons item* (recur))))
                 (else (recur))))
         stream-nil))))

(define (stream-for-each procedure stream)
  (if (stream-pair? stream)
      (begin (procedure (stream-car stream))
	     (stream-for-each procedure (stream-cdr stream)))))

(define (stream-append stream-a stream-b)
  (let recur ((stream-a stream-a))
    (lazy
     (if (stream-pair? stream-a)
         (stream-cons (stream-car stream-a)
                      (recur (stream-cdr stream-a)))
         stream-b))))

(define (stream-diagonalize streams)
  (let diagonalize ((streams streams))
    (lazy
     (if (not (stream-pair? streams))
         stream-nil
         (let ((stream (stream-car streams)))
           (if (not (stream-pair? stream))
               (recur (stream-cdr streams))
               (stream-cons
                (stream-car stream)
                (let recur ((stream-a (diagonalize (stream-cdr streams)))
                            (stream-b (stream-cdr stream)))
                  (lazy
                   (if (stream-pair? stream-a)
                       (stream-cons (stream-car stream-a)
                                    (recur stream-b (stream-cdr stream-a)))
                       stream-b))))))))))

(define (list->stream list)
  (let recur ((list list))
    (lazy
     (if (pair? list)
         (stream-cons (car list) (recur (cdr list)))
         stream-nil))))

(define (stream->list n #!optional stream)
  (if (default-object? stream)
      (begin
	(set! stream n)
	(set! n #f)))
  (let loop ((stream stream) (reversed-list '()) (count 0))
    (if (and (or (not n) (< count n)) (stream-pair? stream))
        (loop (stream-cdr stream)
              (cons (stream-car stream) reversed-list)
	      (+ count 1))
        (reverse! reversed-list))))

(define-syntax stream
  (syntax-rules ()
    ((STREAM) stream-nil)
    ((STREAM x y ...) (stream-cons x (stream y ...)))))

(define (stream-take stream n)
  (stream->list n stream))

(define (stream-drop stream n)
  (lazy
   (let loop ((stream stream) (n n))
     (cond ((<= n 0) stream)
	   ((stream-pair? stream)
	    (loop (stream-cdr stream) (- n 1)))
	   (else (error "The stream wasn't long enough"))))))

(define (stream-drop-while pred? stream)
  (lazy
   (let loop ((stream stream))
     (cond ((stream-pair? stream)
	    (if (pred? (stream-car stream))
		(loop (stream-cdr stream))
		stream))
	   (else stream-nil)))))

(define (stream-take-while pred? stream)
  (let loop ((stream stream))
    (lazy
     (cond ((stream-pair? stream)
	    (if (pred? (stream-car stream))
		(stream-cons (stream-car stream)
			     (loop (stream-cdr stream)))
		stream-nil))
	   (else stream-nil)))))

(define (stream-reverse stream)
  (list->stream (reverse (stream->list stream))))

(define (stream-count pred? stream)
  (let loop ((stream stream) (answer 0))
    (if (stream-pair? stream)
	(if (pred? (stream-car stream))
	    (loop (stream-cdr stream) (+ answer 1))
	    (loop (stream-cdr stream) answer))
	answer)))

(define (stream-unfold seed gen #!optional stop? f tail-gen)
  (if (default-object? tail-gen)
      (set! tail-gen (lambda (x) stream-nil)))
  (if (default-object? f)
      (set! f (lambda (x) x)))
  (if (default-object? stop?)
      (set! stop? (lambda (x) #f)))
  (let recur ((seed seed))
    (lazy
     (if (stop? seed)
	 (tail-gen seed)
	 (stream-cons
	  (f seed)
	  (recur (gen seed)))))))

