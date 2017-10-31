minischeme
==========

Mini-Scheme Interpreter with Copying GC


Features
--------

* based on Mini-Scheme 0.85k4-a
* continuations support without using 'SCHEME STACK' (#undef USE_SCHEME_STACK)
* implemented a Copying GC using the Cheney's algorithm (#define USE_COPYING_GC)
* reinventing the [tinyscheme](https://github.com/ignorabimus/tinyscheme "Experimental fork of TinyScheme and extensions TSX, RE.") :)


Build (with MSVC)
-----------------

Install "Visual C++ 2015" or later.

* I compiled using "Microsoft Visual C++ 2015", but "2013" or earlier maybe OK.

### Setting up environemt

Open the Visual Studio command prompt, or open Normal command prompt then run

    > (Visual Studio installed path)\VC\vcvarsall.bat

and change directory to unpacking source files.

    > cd src

### Build an executable

To build an executable, just run

    > msvcbuild.bat

and you'll get

    > bin\minischeme.exe

### Build a static library

To build as a static library, run with "static" option

    > msvcbuild.bat static

and you'll get

    > lib\minischeme.lib


[R5RS standard](http://www.schemers.org/Documents/Standards/R5RS/ "schemers.org: Documents: Standards: R5RS") compatibility
------------------

* [x] *
* [x] +
* [x] -
* [x] /
* [x] =
* [x] <
* [x] >
* [x] <=
* [x] >=
* [x] abs
* [x] acos
* [ ] angle
* [x] append
* [x] apply
* [x] asin
* [x] assoc
* [x] assq
* [x] assv
* [x] atan
* [x] begin
* [x] boolean?
* [x] caaaar
* [x] caaadr
* [x] caaar
* [x] caadr
* [x] caadar
* [x] caaddr
* [x] caar
* [x] cadaar
* [x] cadadr
* [x] cadar
* [x] caddr
* [x] caddar
* [x] cadddr
* [x] cadr
* [x] call-with-current-continuation
* [x] call-with-input-file
* [x] call-with-output-file
* [x] call-with-values
* [x] car
* [x] case
* [x] cdar
* [x] cdaar
* [x] cdaaar
* [x] cdaadr
* [x] cdadar
* [x] cdaddr
* [x] cdadr
* [x] cddr
* [x] cddar
* [x] cddaar
* [x] cddadr
* [x] cdddr
* [x] cdddar
* [x] cddddr
* [x] cdr
* [x] ceiling
* [x] char->integer
* [x] char-alphabetic?
* [x] char-ci<=?
* [x] char-ci<?
* [x] char-ci=?
* [x] char-ci>=?
* [x] char-ci>?
* [x] char-downcase
* [x] char-lower-case?
* [x] char-numeric?
* [x] char-ready?
* [x] char-upcase
* [x] char-upper-case?
* [x] char-whitespace?
* [x] char<=?
* [x] char<?
* [x] char=?
* [x] char>=?
* [x] char>?
* [x] char?
* [x] close-input-port
* [x] close-output-port
* [ ] complex?
* [x] cond
* [x] cons
* [x] cos
* [x] current-input-port
* [x] current-output-port
* [x] define
* [x] define-syntax
* [x] delay
* [ ] denominator
* [x] display
* [x] do
* [x] dynamic-wind
* [x] else
* [x] eof-object?
* [x] eq?
* [x] equal?
* [x] eqv?
* [x] eval
* [x] even?
* [x] exact->inexact
* [x] exact?
* [x] exp
* [x] expt
* [x] floor
* [x] for-each
* [x] force
* [x] gcd
* [x] if
* [ ] imag-part
* [x] inexact->exact
* [x] inexact?
* [x] input-port?
* [x] integer->char
* [x] integer?
* [x] interaction-environment
* [x] lambda
* [x] lcm
* [x] length
* [x] let
* [x] let*
* [x] let-syntax
* [x] letrec
* [ ] letrec-syntax
* [x] list
* [x] list->string
* [x] list->vector
* [x] list-ref
* [x] list-tail
* [x] list?
* [x] load
* [x] log
* [ ] magnitude
* [ ] make-polar
* [ ] make-rectangular
* [x] make-string
* [x] make-vector
* [x] map
* [x] max
* [x] member
* [x] memq
* [x] memv
* [x] min
* [x] modulo
* [x] negative?
* [x] newline
* [x] not
* [ ] null-environment
* [x] null?
* [x] number->string
* [x] number?
* [ ] numerator
* [x] odd?
* [x] open-input-file
* [x] open-output-file
* [x] or
* [x] output-port?
* [x] pair?
* [x] peek-char?
* [x] port?
* [x] positive?
* [x] promise
* [x] procedure?
* [x] quasiquote
* [x] quote
* [x] quotient
* [ ] rational?
* [ ] rationalize
* [x] read
* [x] read-char?
* [ ] real-part
* [x] real?
* [x] remainder
* [x] reverse
* [x] round
* [ ] scheme-report-environment
* [x] set!
* [x] set-car!
* [x] set-cdr!
* [x] sin
* [x] sqrt
* [x] string
* [x] string->list
* [x] string->number
* [x] string->symbol
* [x] string-append
* [x] string-ci<=?
* [x] string-ci<?
* [x] string-ci=?
* [x] string-ci>=?
* [x] string-ci>?
* [x] string-copy
* [x] string-fill!
* [x] string-length
* [x] string-ref
* [x] string-set!
* [x] string<=?
* [x] string<?
* [x] string=?
* [x] string>=?
* [x] string>?
* [x] string?
* [x] substring
* [x] symbol->string
* [x] symbol?
* [x] syntax-rules
* [x] tan
* [ ] transcript-off
* [ ] transcript-on
* [x] truncate
* [x] values
* [x] vector
* [x] vector->list
* [x] vector-fill!
* [x] vector-length
* [x] vector-ref
* [x] vector-set!
* [x] vector?
* [x] with-input-from-file
* [x] with-output-to-file
* [x] write
* [x] write-char
* [x] zero?


Links
-----

[D. Souflis, J. Shapiro - TinyScheme Home](http://tinyscheme.sourceforge.net/home.html)

[Visual Studio Downloads](http://www.visualstudio.com/downloads/)


License
-------

Copyright (c) 2015 Tatsuya Watanabe. See the LICENSE file for license rights and limitations (MIT).
