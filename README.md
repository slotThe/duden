# duden

This program allows you to search words on [Duden's website] from your
command line!  For example, if one wanted to search for "intendieren",
all one had to do is to invoke the program without any arguments:

![](https://user-images.githubusercontent.com/50166980/111788191-14dc1c80-88c0-11eb-96ec-c9c69e869f94.png)

This will, by default, show the sections `Wortart, Gebrauch, Bedeutung,
Synonyme` of the first two search results.

Words can also be directly looked up (instead of searched for) with the `-l`
flag:

![](https://user-images.githubusercontent.com/50166980/111794766-da29b280-88c6-11eb-9aec-80bb26bc08d3.png)

For more information, as well as all available command line arguments, invoke
the program with the `--help` flag.

[Duden's website]: https://www.duden.de/

# Building

Build with `stack build` and copy the resulting binaries to a suitable location.
You can also use `stack install` to do the copying for you automatically (this
will most likely move the executable to `~/.local/bin`).  There is also a small
build script available, you may use it as follows:

``` shell
./build /path/to/dir/duden
```

# Misc (?)

For no reason at all---though probably mostly in order to invoke fierce and
undying hatred in my future self, as well as every single person to ever read
this code---I wrote the whole thing in a heavily covariant style.  This involves
using `(.>) = (>>>)` instead of `(.)`, `(>>=)` instead of `(=<<)`, and `(<&>)`
instead of `(<$>)` throughout.  I did at one point think about defining `(&)` to
be right-associative and writing `f a b` as `b & a & f`, but I stopped myself
before things got that far out of hand.
