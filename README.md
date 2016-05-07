# functional [![Build Status](https://travis-ci.org/lexi-lambda/functional.svg?branch=master)](https://travis-ci.org/lexi-lambda/functional) [![Coverage Status](https://coveralls.io/repos/github/lexi-lambda/functional/badge.svg?branch=master)](https://coveralls.io/github/lexi-lambda/functional?branch=master)

This library provides **functional programming utilities** for Racket, including interfaces, such as functors and monads, and common data types that implement those interfaces, such as maybe and either.

Here’s an example of performing monadic computations using `functional`:

```racket
#lang racket

(require data/applicative
         data/monad
         data/maybe)

(define try-bytes->string/utf8
  (curry exn->maybe exn:fail:contract? bytes->string/utf-8))

(define input-char-length
  (do [str <- (try-bytes->string/utf-8 (port->bytes))]
      (pure (length str))))
```

[**For more information, see the documentation.**][functional-doc]

[functional-doc]: http://docs.racket-lang.org/functional/index.html
