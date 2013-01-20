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
		 8 45))))
