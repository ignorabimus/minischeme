minischeme
==========

Mini-Scheme Interpreter with Copying GC


Features
--------

* based on Mini-Scheme 0.85k4-a
* continuations support without using 'SCHEME STACK' (#undef USE_SCHEME_STACK)
* implemented a Copying GC using the Cheney's algorithm (#define USE_COPYING_GC)
* reinventing the [tinyscheme](https://github.com/ignorabimus/tinyscheme "Experimental fork of TinyScheme and extensions TSX, RE.") :)


Benchmark
---------

fib.scm:
```scheme
(define (fib n)
    (if (< n 2)
        n
        (+ (fib (- n 2)) (fib (- n 1)))))
    
(display (fib 38))
```

#### Mini-Scheme 0.85w1
```
cl /O2 miniscm.c
```
```
$ time ./miniscm fib.scm
39088169
real    0m47.549s
user    0m0.000s
sys     0m0.015s
```

#### Mini-Scheme 0.85k4-a
```
cl /O2 /DMSC miniscm.c
```
```
$ time ./miniscm < fib.scm
...
> 39088169#t
> Good-bye

real    2m54.486s
user    0m0.000s
sys     0m0.015s
```

#### TinyScheme 1.41 (for reference)
```
cl /O2 /DWIN32 scheme.c
```
```
$ time ./scheme fib.scm
39088169
real    8m18.623s
user    0m0.000s
sys     0m0.015s
```


Links
-----

[D. Souflis, J. Shapiro - TinyScheme Home](http://tinyscheme.sourceforge.net/home.html)


License
-------

Copyright (c) 2015 Tatsuya Watanabe. See the LICENSE file for license rights and limitations (MIT).
