Integer Sequences
=================

A library for recreational number theory in MIT Scheme.

By "recreational number theory" I mean facilities for playing around
with various properties and sequences of integers, such as factorials,
fibonacci numbers, primes, triangle numbers, etc.

The bulk of the Integer Sequences library is, surprise surprise,
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

| Operation     | Name                    | Returns                                   |
|---------------|-------------------------|-------------------------------------------|
| generator     | (foo k)                 | The kth foo (1-indexed)                   |
| inverter      | (foo-root n)            | Integer Inverse of foo at n (see below)   |
| tester        | (foo? n)                | Is n a foo?                               |
| counter       | (count-foos l h)        | How many foos in l <= foo < h             |
| streamer      | (the-foos)              | Stream of all (positive) foos             |
| up-streamer   | (foos-from n)           | Same, starting from >= n                  |
| down-streamer | (foos-down-from n)      | Same, but <= n going down                 |
| up-ranger     | (foos-between l h)      | Stream of foos in l <= foo < h            |
| down-ranger   | (foos-between-down l h) | Stream of foos in h >= foo > l going down |

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

> Define an _integer inverse_ of a monotonic function f: N --> N to be
> any function g: N --> Q such that, for each n, either
>
> - g(n) is an integer and f(g(n)) = n, or
> - g(n) is not an integer and f(floor(g(n))) < n < f(ceiling(g(n))).

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
