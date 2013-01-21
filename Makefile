### This file is part of Integer Sequences, a library for recreational
### number theory in MIT Scheme.
### Copyright (C) 2013 Alexey Radul
###
### Integer Sequences is free software: you can redistribute it and/or
### modify it under the terms of the GNU Affero General Public License
### as published by the Free Software Foundation, either version 3 of
### the License, or (at your option) any later version.
###
### Integer Sequences is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
### See the GNU Affero General Public License for more details.
###
### You should have received a copy of the GNU Affero General Public
### License along with Integer Sequences.  If not, see
### <http://www.gnu.org/licenses/>.

test:
	mit-scheme --compiler -heap 6000 --batch-mode --no-init-file --eval '(set! load/suppress-loading-message? #t)' --eval '(begin (load "load") (load "test/load") (run-tests-and-exit))'

numbers-meta.png: numbers-meta.fig Makefile
# The place on github where this goes appears to be 824 pixels wide, but -Z is in
# centimeters for this figure.
	fig2dev -L png -Z 24.8 -D +50 numbers-meta.fig numbers-meta.png

numbers-meta-implemented.png: numbers-meta.fig Makefile
	fig2dev -L png -Z 24.8 -D +50,55 numbers-meta.fig numbers-meta.png

numbers-meta-invented.png: numbers-meta.fig Makefile
	fig2dev -L png -Z 24.8 -D +50,55,60 numbers-meta.fig numbers-meta.png

README.html: README.md
	redcarpet --parse-fenced-code-blocks --render-with-toc-data --render-with-tables README.md > README.html

.PHONY: test
