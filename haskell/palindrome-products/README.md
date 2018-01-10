# Palindrome Products

Write a program that can detect palindrome products in a given range.

A palindromic number reads the same both ways. The largest palindrome
made from the product of two 2-digit numbers is 9009 = 91 x 99.


## Getting Started

For installation and learning resources, refer to the
[exercism help page](http://exercism.io/languages/haskell).

## Running the tests

To run the test suite, execute the following command:

```bash
stack test
```

#### If you get an error message like this...

```
No .cabal file found in directory
```

You are probably running an old stack version and need
to upgrade it.

#### Otherwise, if you get an error message like this...

```
No compiler found, expected minor version match with...
Try running "stack setup" to install the correct GHC...
```

Just do as it says and it will download and install
the correct compiler version:

```bash
stack setup
```

## Running *GHCi*

If you want to play with your solution in GHCi, just run the command:

```bash
stack ghci
```

## Feedback, Issues, Pull Requests

The [exercism/xhaskell](https://github.com/exercism/xhaskell) repository on
GitHub is the home for all of the Haskell exercises.

If you have feedback about an exercise, or want to help implementing a new
one, head over there and create an issue.  We'll do our best to help you!

## Hints

To solve this exercise you need to implement these two functions:

- `largestPalindrome`
- `smallestPalindrome`

Both functions receive lower and upper factor limits, returning a pair
`(value, [(factor1, factor2)])` containing the palindrome and its possible
pairs of factors.

Your can use the provided signatures if you are unsure about the types, but
don't let them restrict your creativity.

It's ok to return duplicates in the factors list, and the order of the factors
is irrelevant.

You should consider using a slightly different algorithm to find small or
large palindromes.

## Source

Problem 4 at Project Euler [http://projecteuler.net/problem=4](http://projecteuler.net/problem=4)

## Submitting Incomplete Problems
It's possible to submit an incomplete solution so you can see how others have completed the exercise.

