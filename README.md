# lack-middleware-debug

## Usage

Wrap app:

```lisp
(funcall lack/middleware/debug:*lack-middleware-debug*
         *app*
         :output *error-output*
         :special-variables '(foo.lisp.vinland:*route*
                              foo.lisp.vinland:*binding*))
```

Lack Builder:

```lisp
(lack:builder
 (:debug :output #p"/tmp/myapp-debug.log"
         :special-variables '(foo.lisp.vinland:*route*
                              foo.lisp.vinland:*binding*))
 *app*)
```

### Options

* `OUTPUT`: stream or pathname indicating where to write the env and backtrace on an error; a NIL value means to skip logging the error entirely; default: `*STANDARD-OUTPUT*`
* `SPECIAL-VARIABLES`: list of symbols naming special variables that should be included in the debug page; optional
* `PRINT-ENV`: whether to print the redact ENV to OUTPUT; boolean (default: T)
* `PRINT-BACKTRACE`: boolean; whether to print the backtrace to OUTPUT(default: T)
* `REDACT-BACKTRACE`: boolean whether to redact the backtrace printed to OUTPUT and included in the debug page; (default: T)
* `INCLUDE-SPECIAL-VARIABLES-HTML`: boolean; whether to include the special variables in the debug page (default: T)
* `INCLUDE-BACKTRACE-HTML`: boolean; whether to include the backtrace in the debug page (default: T)

## Development

Run tests:

```lisp
(asdf:test-system :foo.lisp.lack-middleware-debug)
```

## Installation

Not in Quicklisp, so clone to "local-projects/".

## Dependencies

* [cl-info](https://github.com/40ants/cl-info)
* [foo.lisp.http-response](https://github.com/lisplizards/http-response)
* [foo.lisp.redact](https://github.com/lisplizards/redact)
* [spinneret](https://github.com/ruricolist/spinneret)

### Tests

* [rove](https://github.com/fukamachi/rove)

## Author

* John Newton (<a href="mailto:jnewton@lisplizards.dev">jnewton@lisplizards.dev</a>)

## Copyright

Copyright (c) 2024 John Newton

## License

Apache-2.0
