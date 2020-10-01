# duden

This program allows you to search words on [Duden's
website](https://www.duden.de/) from your command line!  For example, if one
wanted to search for "intendieren", all one had to do is to invoke the program
without any arguments:

``` shell
duden intendieren
```

This will, by default, show the sections `Wortart, Gebrauch, Bedeutung,
Synonyme` of the first two search results.

Words can also be directly looked up (instead of searched for) with the `-l`
flag:

``` shell
duden -l intendieren
```

For more information, as well as all available command line arguments, invoke
the program with the `--help` flag.

# Misc (?)
For no reason at all---though probably mostly in order to invoke fierce and
undying hatred in my future self, as well as every single person to ever read
this code---I wrote the whole thing in a heavily covariant style.  This involves
using `(.>) = (>>>)` instead of `(.)`, `(>>=)` instead of `(=<<)`, and `(<&>)`
instead of `(<$>)` throughout.  I did at one point think about defining `(&)` to
be right-associative and writing `f a b` as `b & a & f`, but I stopped myself
before things got this far out of hand.
