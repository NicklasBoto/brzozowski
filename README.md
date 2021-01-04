# [brzozowski](https://zozo.nicbot.xyz/Zozo.html)

Library for constructing generalized regular expressions (and more, in the future).

## Examples

We can construct a simple regex saying that santas has many ho's, that is one or more.

```hs
>>> santa = many "ho"
>>> "hohoho" ^- santa
^((ho)+|ε)$
```

Deriving santa with respect to three ho's, gives us a regex matching remaining possible strings that also match santa. Seeing that `(ho)+ | ε` is saying zero or more ho's, we can further reduce this to `(ho)*`. It is easy to see that this regex contains the empty string but the `?` function will tell you nonetheless.

```hs
>>> "hohoho" ? santa
True
```

We can also demonstrate some strings that do not match.

```hs
>>> "hahaha" ^- santa
^∅$
>>> "hahaha" ? santa
False
>>> -- santa does not laugh
>>> "" ^- santa
^(ho)+$
>>> "" ? santa
False
>>> -- santa needs at least one ho
```
