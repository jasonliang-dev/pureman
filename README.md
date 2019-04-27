# pureman

Install [Stack](https://docs.haskellstack.org/en/stable/README/). Then:

```
$ stack build
$ stack exec pureman
```

## Sample program output

```
Letters guessed:

_______

12 guesses left
Guess a letter: e
Letters guessed: e

_e_____

12 guesses left
Guess a letter: p
Letters guessed: e p

_e_____

11 guesses left
Guess a letter: y
Letters guessed: e p y

_e_____

10 guesses left
Guess a letter: z
Letters guessed: e p y z

Ze_____

9 guesses left
Guess a letter: l
Letters guessed: e l p y z

Ze_l___

9 guesses left
Guess a letter: o
Letters guessed: e l o p y z

Ze_lo__

9 guesses left
Guess a letter: u
Letters guessed: e l o p u y z

Ze_lou_

9 guesses left
Guess a letter: s
Letters guessed: e l o p s u y z

Ze_lous

9 guesses left
Guess a letter: a

The word was Zealous!
```
