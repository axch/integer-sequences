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

Provided Sequences
------------------

The following sequences are provided with Integer Sequences, and
implement all the sequences functions described
[above](#sequence-operations).

| Singular Name     | Brief Definition                                               |
|-------------------+----------------------------------------------------------------|
| integer           | the integers                                                   |
| factorial         | product of the first k consecutive integers                    |
| fibonacci         | 1, 1, ..., fib(n-1) + fib(n-2)                                 |
| prime             | integers > 1 divisible by no other                             |
| composite         | integers > 1 divisible by another                              |
| semiprime         | integers with exactly two prime factors, counting multiplicity |
| square-free       | integers divisible by each prime at most once                  |
| perfect           | n = sum of all proper divisors of n := aliquot(n)              |
| abundant          | n < sum of all proper divisors of n =                          |
| deficient         | n > sum of all proper divisors of n =                          |
| amicable          | not perfect, but n = aliquot(aliquot(n))                       |
| aspiring          | not perfect, but aliquot^m(n) is perfect for some m            |
| mersenne          | 2^p-1 for prime p.  If also prime, called a Mersenne prime     |
| primorial         | product of the first k primes                                  |
| compositorial     | produce of the first k composites                              |
| square            | number of objects in some k by k square = k*k                  |
| cube              | number of objects in some k by k by k cube = k*k*k             |
| triangle          | same for triangle with k objects on a side = k*(k+1)/2         |
| pentagon          | same for pentagon = k*(3k-1)/2                                 |
| hexagon           | same for hexagon = k*(2k-1)                                    |
| heptagon          | same for heptagon = k*(5k-3)/2                                 |
| octagon           | same for octagon = k*(3k-2)                                    |
| nonagon           | same for nonagon = k*(7k-5)/2                                  |
| decagon           | same for decagon = k*(4k-3)                                    |
| tetrahedron       | same for tetrahedron = k*(k+1)*(k+2)                           |
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
| smith             | a composite whose sum of digits equals                         |
|                   | the sum of the digits of its prime factors                     |
| hoax              | same, but distinct prime factors                               |
| repunit           | every digit (in decimal) is 1                                  |
| repdigit          | decimal expansion uses only one distinct digit (e.g., 333)     |
| undulating        | decimal expansion follows pattern ababababab (e.g., 212)       |
