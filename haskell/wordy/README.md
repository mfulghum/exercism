# Wordy

Parse and evaluate simple math word problems returning the answer as an integer.

## Iteration 1 — Addition

Add two numbers together.

> What is 5 plus 13?

Evaluates to 18.

Handle large numbers and negative numbers.


## Iteration 2 — Subtraction, Multiplication and Division

Now, perform the other three operations.

> What is 7 minus 5?

2

> What is 6 multiplied by 4?

24

> What is 25 divided by 5?

5


## Iteration 3 — Multiple Operations

Handle a set of operations, in sequence.

Since these are verbal word problems, evaluate the expression from
left-to-right, _ignoring the typical order of operations._

> What is 5 plus 13 plus 6?

24

> What is 3 plus 2 times 3?

15  (i.e. not 9)


## Bonus — Exponentials

If you'd like, handle exponentials.

> What is 2 raised to the 5th power?

32



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

This is a perfect opportunity to learn some Attoparsec or Parsec!

## Source

Inspired by one of the generated questions in the Extreme Startup game. [https://github.com/rchatley/extreme_startup](https://github.com/rchatley/extreme_startup)

## Submitting Incomplete Problems
It's possible to submit an incomplete solution so you can see how others have completed the exercise.

