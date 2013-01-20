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
 meta-system
 
 (define-each-test
  (assert-equal 9/2  ((invert-by-counting square) 20) "4 < sqrt(20) < 5")
  (assert-equal 5    ((invert-by-counting square) 25) "5 >= sqrt(25)")
  (assert-equal 11/2 ((invert-by-counting square) 26) "5 < sqrt(26) < 6")
  (assert-equal 1    ((invert-by-binary-search square) 1) "1 >= sqrt(1)")
  (assert-equal 9/2  ((invert-by-binary-search square) 20) "4 < sqrt(20) < 5")
  (assert-equal 5    ((invert-by-binary-search square) 25) "5 >= sqrt(25)")
  (assert-equal 11/2 ((invert-by-binary-search square) 26) "5 < sqrt(26) < 6")
  (assert-equal 4 ((inverter->counter (generator->inverter square)) 16 50))
  (assert-true  ((inverter->tester (generator->inverter square)) 36))
  (assert-false ((inverter->tester (generator->inverter square)) 35))
  (assert-equal '(16 25 36 49)
		(stream->list
		 ((tester->up-ranger
		   (inverter->tester
		    (generator->inverter
		     square)))
		  16 50)))
  (assert-equal '(16 25 36 49)
		(stream->list
		 ((generator+inverter->up-ranger
		   square (generator->inverter square))
		  16 50)))
  (assert-equal 4
		((up-ranger->counter
		  (tester->up-ranger
		   (inverter->tester
		    (generator->inverter
		     square))))
		 16 50))
  (assert-equal '(49 36 25)
		(stream->list
		 ((tester->down-ranger
		   (inverter->tester
		    (generator->inverter
		     square)))
		  16 50)))
  (assert-equal '(49 36 25)
		(stream->list
		 ((generator+inverter->down-ranger
		   square (generator->inverter square))
		  16 50)))
  (assert-equal '(16 25 36 49 64)
		(stream->list 5
		 ((tester->up-streamer
		   (inverter->tester
		    (generator->inverter
		     square)))
		  16)))
  (assert-equal '(16 25 36 49 64)
		(stream->list 5
		 ((generator+inverter->up-streamer
		   square (generator->inverter square))
		  16)))
  (assert-equal '(49 36 25 16 9 4 1)
		(stream->list
		 ((down-ranger->down-streamer
		   (tester->down-ranger
		    (inverter->tester
		     (generator->inverter
		      square))))
		  49)))
  (assert-equal '(1 4 9 16 25 36 49 64)
		(stream->list 8
		 ((up-streamer->streamer
		   (tester->up-streamer
		    (inverter->tester
		     (generator->inverter
		      square)))))))
  (assert-equal 81
		((streamer->generator
		  (up-streamer->streamer
		   (tester->up-streamer
		    (inverter->tester
		     (generator->inverter
		      square)))))
		 9))

  (assert-equal '(1 3 6 10) (stream->list 4 ((generator->streamer triangle))))
  (assert-equal '(10 15 21 28)
		(stream->list 4
                 ((streamer->up-streamer
		   (generator->streamer triangle))
		  8)))
  (assert-equal '(10 15 21 28 36)
		(stream->list
                 ((up-streamer->up-ranger
		   (streamer->up-streamer
		    (generator->streamer triangle)))
		  8 45)))
  (assert-equal '(45 36 28 21 15 10)
		(stream->list
                 ((up-ranger->down-ranger
		   (up-streamer->up-ranger
		    (streamer->up-streamer
		     (generator->streamer triangle))))
		  8 45)))
  (assert-true  ((up-ranger->tester
		  (up-streamer->up-ranger
		   (streamer->up-streamer
		    (generator->streamer triangle))))
		 45))
  (assert-false ((up-ranger->tester
		  (up-streamer->up-ranger
		   (streamer->up-streamer
		    (generator->streamer triangle))))
		 46))
  (assert-true  ((down-ranger->tester
		  (up-ranger->down-ranger
		   (up-streamer->up-ranger
		    (streamer->up-streamer
		     (generator->streamer triangle)))))
		 45))
  (assert-false ((down-ranger->tester
		  (up-ranger->down-ranger
		   (up-streamer->up-ranger
		    (streamer->up-streamer
		     (generator->streamer triangle)))))
		 46))
  (assert-equal 5
		((up-ranger->counter
		  (up-streamer->up-ranger
		   (streamer->up-streamer
		    (generator->streamer triangle))))
		 8 45)))

 (let ((square-seq (construct-seq 'generator square)))
   (define-each-test
     (assert-equal 36 ((seq-generator     square-seq) 6))
     (assert-equal 6  ((seq-inverter      square-seq) 36))
     (assert-true     ((seq-tester        square-seq) 25))
     (assert-equal 5  ((seq-counter       square-seq) 17 100))
     (assert-equal
      '(1 4 9 16)
      (stream->list 4 ((seq-streamer      square-seq))))
     (assert-equal
      '(25 36 49 64)
      (stream->list 4 ((seq-up-streamer   square-seq) 20)))
     (assert-equal
      '(64 49 36 25 16 9 4 1)
      (stream->list   ((seq-down-streamer square-seq) 70)))
     (assert-equal
      '(25 36 49 64)
      (stream->list   ((seq-up-ranger     square-seq) 25 70)))
     (assert-equal
      '(64 49 36)
      (stream->list   ((seq-down-ranger   square-seq) 25 70)))))

 (let ((prime-seq (construct-seq 'tester prime?)))
   (define-each-test
     (assert-equal 13 ((seq-generator     prime-seq) 6))
     (assert-equal 6  ((seq-inverter      prime-seq) 13))
     (assert-false    ((seq-tester        prime-seq) 25))
     (assert-equal 5  ((seq-counter       prime-seq) 17 37))
     (assert-equal
      '(2 3 5 7)
      (stream->list 4 ((seq-streamer      prime-seq))))
     (assert-equal
      '(23 29 31 37)
      (stream->list 4 ((seq-up-streamer   prime-seq) 20)))
     (assert-equal
      '(37 31 29 23 19 17 13 11 7 5 3 2)
      (stream->list   ((seq-down-streamer prime-seq) 37)))
     (assert-equal
      '(29 31 37)
      (stream->list   ((seq-up-ranger     prime-seq) 25 40)))
     (assert-equal
      '(37 31 29)
      (stream->list   ((seq-down-ranger   prime-seq) 25 40))))))
