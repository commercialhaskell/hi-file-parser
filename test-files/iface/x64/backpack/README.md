`backpack`
==========

This Stack project, when built, produces three `*.hi` files that are then used
to test whether `hi-file-parser` can deal with Backpack-related `*.hi` files.

The project comprises four Haskell packages:
- `str-sig` (which exposes a signature `Str`)
- `logger-sig` (depends directly on `str-sig` and also exposes a signature
  `Logger` and exposes a module `LogHelper`)
- `impl-pkg`
- `consumer-pkg` (depends directly on `impl-pkg`, `str-sig` and `logger-sig`,
  exposes a module `Consumer` and specifies an executable `consumer-demo` with
  `main` module `Main`)

With a version of Stack that supports Backpack, the project can be built by
commanding `stack build` in its project directory.

The relevant `*.hi` files are then:

* `LogHelper.hi`, which should be located in the `build` directory of the
  `dist` directory of the `logger-sig` project package Stack work directory;
* `Consumer.hi`, which should be located in the `build` directory of the
  `dist` directory of the `consumer-pkg` project package Stack work directory;
  and
* `Main.hi`, which should be located in a `consumer-demo\consumer-demo-tmp`
  directory in  the `build` directory of the `dist` directory of the
  `consumer-pkg` project package Stack work directory.
