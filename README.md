Integer Sequences
=================

A library for recreational number theory in MIT Scheme.

By "recreational number theory" I mean facilities for playing around
with various properties and sequences of integers, such as factorials,
fibonacci numbers, primes, triangle numbers, etc.

The Integer Sequences library is, surprise surprise,
organized around the concept of an integer sequence.  Every integer
sequence can be viewed as a property that an integer might have,
namely whether that integer is in that sequence or not; and every
property of integers can be viewed as a monotonically increasing
sequence of all the positive integers that have this property (note
that these two reinterpretations are only mutual inverses when
restricted to monotonically increasing sequences and properties,
respectively, of positive integers).  Monotonically increasing
sequences can also be inverted, that is, the index of an element is
well-defined and can be computed.

Sequence Operations
-------------------

Every sequence defined by Integer Sequences provides the following
operations:

| Operation     | Name                      | Returns                                   |
|---------------|---------------------------|-------------------------------------------|
| generator     | `(foo k)`                 | The kth foo (1-indexed)                   |
| inverter      | `(foo-root n)`            | Integer Inverse of foo at n (see below)   |
| tester        | `(foo? n)`                | Is n a foo?                               |
| counter       | `(count-foos l h)`        | How many foos in l <= foo < h             |
| streamer      | `(the-foos)`              | Stream of all (positive) foos             |
| up-streamer   | `(foos-from n)`           | Same, starting from >= n                  |
| down-streamer | `(foos-down-from n)`      | Same, but <= n going down                 |
| up-ranger     | `(foos-between l h)`      | Stream of foos in l <= foo < h            |
| down-ranger   | `(foos-between-down l h)` | Stream of foos in h >= foo > l going down |

For example, `(perfect 3)` returns 496, `(factorial? 8)` returns false,
`(the-primes)` returns an infinite stream that starts with 2, 3, 5, 7,
11, 13, ..., and `(squares-down-from 70)` returns a (finite!) stream
whose contents are 64, 49, 36, 25, 16, 9, 4, 1.  There are plenty more
examples in test/properties-test.scm.

Integer Inverses
----------------

Inversion is a very useful concept when working with anything that
looks like a function (namely, from indecies to sequence elements).
In the case of integer sequences, this means, given an element of a
sequence, computing its index, and given an integer that is not an
element of a sequence, computing the indecies of the two adjacent
elements it falls between.  To be precise:

> Define an _integer inverse_ of a monotonic function f: Z+ --> Z+ to be
> any function g: Z+ --> Q+ such that, for each n, either
>
> - g(n) is an integer and f(g(n)) = n, or
> - g(n) is not an integer and f(floor(g(n))) < n < f(ceiling(g(n))),
>   where we formally take f(0) = 0 to cover the case where n < f(1).

The function g is not itself unique (because it can return any
non-integer within the desired bounds when its input is not a member
of the sequence), but by monotonicity of f, floor(g(n)) and
ceiling(g(n)) always exist and are unique.

Note that such a g can be defined to always compute with and return
exact Scheme numbers, thereby avoiding all problems with roundoff
error (which can be very significant when dealing with large integers,
as for instance testing whether 5^200 is a square).

The integer inverse of each sequence `foo` is implemented by the
function `(foo-root n)`.  In this library, I choose to return the
half-integer between the two answers if there is no exact integer
inverse; so for instance `(cube-root 8)` returns 2, but `(cube-root
10)` returns 5/2 (as does `cube-root` of anything else between 9 and
26, inclusive).

Provided Sequences
------------------

The following sequences are provided with Integer Sequences, and
implement all the sequences functions described
[above](#sequence-operations).

| Singular Name     | Brief Definition                                               |
|-------------------|----------------------------------------------------------------|
| integer           | the integers                                                   |
| factorial         | product of the first k consecutive integers                    |
| fibonacci         | 1, 1, ..., fib(k-1) + fib(k-2), ...                            |
| prime             | integers > 1 divisible by no other                             |
| composite         | integers > 1 divisible by another                              |
| semiprime         | integers with exactly two prime factors, counting multiplicity |
| square-free       | integers divisible by each prime at most once                  |
| perfect           | n = sum of all proper divisors of n := aliquot(n)              |
| abundant          | n < sum of all proper divisors of n                            |
| deficient         | n > sum of all proper divisors of n                            |
| amicable          | not perfect, but n = aliquot(aliquot(n))                       |
| aspiring          | not perfect, but aliquot^m(n) is perfect for some m            |
| mersenne          | 2^p - 1 for prime p.  If also prime, called a Mersenne prime   |
| primorial         | product of the first k primes                                  |
| compositorial     | product of the first k composites                              |
| square            | number of objects in some k by k square = k*k                  |
| cube              | number of objects in some k by k by k cube = k*k*k             |
| triangle          | same for triangle with k objects on a side = k*(k+1)/2         |
| pentagon          | same for pentagon = k*(3k-1)/2                                 |
| hexagon           | same for hexagon = k*(2k-1)                                    |
| heptagon          | same for heptagon = k*(5k-3)/2                                 |
| octagon           | same for octagon = k*(3k-2)                                    |
| nonagon           | same for nonagon = k*(7k-5)/2                                  |
| decagon           | same for decagon = k*(4k-3)                                    |
| tetrahedron       | same for tetrahedron = k*(k+1)*(k+2)/6                         |
| pronic            | k*(k+1) for some k                                             |
| pandigital        | decimal expansion uses all 10 digits                           |
| evil              | binary expansion uses an even number of 1s                     |
| odious            | binary expansion uses an odd number of 1s                      |
| multidigit        | decimal expansion has more than 1 digit (i.e., n >= 10)        |
| palindrome        | reads the same forwards and backwards in decimal               |
| emirp             | non-palindromic prime which is also prime read backwards       |
| emirpimes         | same, but both semiprime                                       |
| strobogrammatic   | reads the same normally and upside-down in decimal (e.g., 609) |
| apocalyptic-power | n such that 2^n contains "666" as a substring (in decimal)     |
| smith             | a composite whose sum of digits equals the sum of the digits of its prime factors |
| hoax              | same, but distinct prime factors                               |
| repunit           | every digit (in decimal) is 1                                  |
| repdigit          | decimal expansion uses only one distinct digit (e.g., 333)     |
| undulating        | decimal expansion follows pattern ababababab (e.g., 212)       |

All of these are strictly monotonic except the Fibonacci numbers, so
their integer inverses are well defined.  By special dispensation,
`(fibonacci-root 1)` returns 1 (as opposed to 2); and
`(count-fibonaccis 1 4)` returns 4 (as opposed to 3), on the gounds
that 1 is a Fibonacci number twice.

Making Your Own Sequences
-------------------------

The sequence operations are mutually interdefinable: if you have any
one of them, you can construct all the rest mechanically (these
contructions depend on monotonicity, in general).  Integer Sequences
provides a facility for doing this for user sequences.  For example,
if you have a formula, you can make a full sequence out of it like
this:

```scheme
(define (my-number k)
  ... ; your code to compute the kth "my-number"
  )
;; Defines my-number?, my-number-root, count-my-numbers,
;; the-my-numbers, my-numbers-from, my-numbers-down-from,
;; my-numbers-between, and my-numbers-between-down for you, in terms
;; of my-number.
(integer-sequence my-number generator)
```

The other common pattern is to turn a tester into a sequence:

```scheme
(define (my-other-number? n)
  ... ; your code to check wether n is a "my-other-number"
  )
;; Defines my-other-number, my-other-number-root,
;; count-my-other-numbers, the-my-other-numbers,
;; my-other-numbers-from, my-other-numbers-down-from,
;; my-other-numbers-between, and my-other-numbers-between-down for
;; you, in terms of my-other-number?.
(integer-sequence my-other-number tester)
```

You can, however, predefine however many of the operations you like
and ask `integer-sequence` to define the others in terms of them.
Doing this can lead to substanital speedups: the only general way to
compute the kth foo if all you can do is check whether something is a
foo is to test all integers starting at 1 until you've found k foos.
Needless to say, an explicit formula would be much preferable.

<table>
<tr><td align="center">
<img src="http://web.mit.edu/~axch/www/numbers-meta.png" alt="Diagram
 of operation derivations">
</td></tr>
<tr><td align="center">
<p><b>Figure 1</b>: A summary of how operations are derived from each other.
The full description is in numbers-meta.scm.</p>
</td></tr>
</table>

`(integer-sequence name available-operation1 available-operation2 ...)` syntax

Completes the definition of a sequence named `name` (which is not
evaluated and must be a symbol) from the given available operations,
defining all the missing ones.  Input operations must be given by
procedures that follow the [naming convention](#sequence-operations),
and new operations are defined to follow it also.  Each
`available-operation` must be one of the (unevaluated) symbols
`generator`, `inverter`, `tester`, `counter`, `streamer`,
`up-streamer`, `down-streamer`, `up-ranger`, or `down-ranger`.

There is also a procedural interface to deriving sequence operations
and accessing the results; see numbers-meta.scm.

Streams
-------

Since lazy streams are not standard in Scheme, but it can be natural
for many purposes to view an integer sequence as an infinite stream of
its elements, Integer Sequences includes a library for creating and
manipulating streams.  Programmatically the streams come up because
the operations `the-foos`, `foos-from`, `foos-down-from`,
`foos-between`, and `foos-between-down` for each sequence return
streams.  I note for connoisseurs that this library implements _even
streams_.

This is not the place for an explanation of the idea of streams or the
interesting phenomena that arise in their implementation in a strict
language like Scheme, so I will content myself with a summary of the
available procedures.  Except where noted, they are entirely analagous
to the like-named procedures operating on lists.

- `(stream-cons first rest)` Unlike standard `cons`, this is a macro,
  since the point is to delay evaluating `first` or `rest` until
  needed.
- `(stream-pair? stream)`
- `(stream-null? stream)`
- `stream-nil` is the empty stream
- `(stream-car stream)`
- `(stream-cdr stream)`
- `(stream-map procedure stream)`
- `(stream-filter predicate stream)`
- `(stream-filter-map procedure stream)` like `stream-map`, but
  exclude elements on which `procedure` returns `#f`.
- `(stream-for-each procedure stream)` note that this differs from
  `stream-map` in that it actually forces evaluation of the
  `procedure` on the `stream`, instead of simply returning a new
  stream.  It also differs from `stream->list` in that it does not
  retain the stream as it goes.  In contrast with a list, a stream
  produced computationally, transformed by `stream-map`,
  `stream-filter`, etc, and consumed by `stream-for-each` need never be
  stored in memory all at once.
- `(stream-append stream1 stream2)`
- `(list->stream list)`
- `(stream->list stream)` does not terminate if the stream is infinite.
- `(stream x y ...)` analagous to the procedure `list`, but a macro
  because the point is to delay evaluating `x`, `y`, ...
- `(stream-take stream n)`
- `(stream-take->list stream n)` convenience procedure; returns the
  first `n` elements of `stream` as a list.
- `(stream-drop stream n)`
- `(stream-drop-while predicate stream)`
- `(stream-take-while predicate stream)`
- `(stream-reverse stream)` does not terminate if the stream is infinite
- `(stream-count predicate stream)` does not terminate if the stream is infinite
- `(stream-unfold seed generator #!optional stop? tail-generator)`
  Return a stream of `seed`, `(generator seed)`, `(generator
  (generator seed))`, etc, until `(stop? (generator^k seed))` is true.  If
  `tail-generator` is supplied, the stream ends with `(tail-generator
  (generator^k seed))`, which, if not `stream-nil`, will cause the
  stream to be improper.  If `stop?` is not supplied or never returns
  `#t`, the stream will be infinite.

Supporting Facilities
---------------------

Some of the helper functions used in defining sequences are useful in
their own right, for thinking about numbers and their properties.  In
addition to the operations implied by the [provided
sequences](#provided-sequences), Integer Sequences provides

- `(increment n)`
- `(decrement n)`
- `(sum list-of-numbers)`
- `(product list-of-numbers)`
- `(divides? divisor number)`
- `(smallest-divisor n #!optional start-from)` If the optional
  argument is supplied, only divisors `>=` to it will be considered.
- `(prime-factors n)`  This is not a fancy factorization algorithm.
- `(divisors n)` All divisors as a list
- `(proper-divisors n)`
- `(sigma n)` The operator that generates the Aliquot sequence: the
  sum of the proper divisors.
- `(number->digits n #!optional base)` The digits in the base `base`
  (default 10) expansion of `n` as a list (most significant first).
- `(digits n #!optional base)` Alias for `number->digits`
- `(binary-digits n)`
- `(number->bits n)` Alias for `binary-digits`
- `(digits->number list-of-digits #!optional base)` Inverse of
  `number->digits` (assuming the same `base`).  Default `base` is 10.
- `(bitcount n)` Number of 1s in the binary expansions of `n`.
- `(upside-down-glyph digit)` Returns the digit that the given one
  reads as upside down, of `#f` if there is none.

Portability
===========

Integer Sequences is written in MIT Scheme with no particular
portability considerations in mind.  On the one hand, it is purely
computational, relying on no external resources whatever; on the other
hand, it does liberally use MIT Scheme extensions that are not
standard Scheme.  Of particular note is the syntactic-closures macro
system, whose controlled non-hygiene enables the `integer-sequence`
macro to implement the naming convention for sequence operations.

I expect Integer Sequences to run unmodified on any platform MIT
Scheme supports, and I expect Integer Sequences should be semantically
fairly easy to port to other Scheme systems, provided they offer a
macro facility with controlled non-hygiene.

Author
======

Alexey Radul, axch@mit.edu.  The streams library was written primarily
by Taylor Campbell, maintained and modified by Alexey Radul and Joyce
Chen.

License
=======

This file is part of Integer Sequences, a library for recreational
number theory in MIT Scheme.
Copyright 2013 Alexey Radul.

Integer Sequences is free software; you can redistribute it and/or
modify it under the terms of the GNU Affero General Public License
as published by the Free Software Foundation; either version 3 of
the License, or (at your option) any later version.

This code is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU Affero General Public
License along with Integer Sequences; if not, see
<http://www.gnu.org/licenses/>.
