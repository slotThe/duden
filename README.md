# duden

TODO

Also, for no reason at all, I wrote the whole thing in a more covariant style,
using `(.>) = (>>>)` instead of `(.)`, `(>>=)` instead of `(=<<)`, and `(<&>)`
instead of `(<$>)` throughout.  I did at one point think about defining `(&)` to
be right-associative and writing `f a b` as `b & a & f`, but I stopped myself
before things got out of hand.
