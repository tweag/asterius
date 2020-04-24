## Writing WebAssembly code in Haskell

In `Asterius.Builtins`, there are WebAssembly shims which serve as our runtime.
We choose to write WebAssembly code in Haskell, using Haskell as our familiar
meta-language.

As of now, there are two ways of writing WebAssembly code in Haskell. The first
way is directly manipulating AST types as specified in `Asterius.Types`. Those
types are pretty bare-metal and maps closely to binaryen IR. Simply write some
code to generate an `AsteriusFunction`, and ensure the function and its symbol
is present in the store when linking starts. It will eventually be bundled into
output WebAssembly binary file.

Directly using `Asterius.Types` is not a pleasant experience, it's basically a
DDoS on one's working memory, since the developer needs to keep a lot of things
in mind: parameter/local ids, block/loop labels, etc. Also, the resulting
Haskell code is pretty verbose, littered with syntactic noise (e.g. tons of
list concats when constructing a block)

We now provide an EDSL in `Asterius.EDSL` to construct an `AsteriusFunction`.
Its core type is `EDSL a`, and can be composed with a `Monad` or `Monoid`
interface. Most builtin functions in `Asterius.Builtins` are already refactored
to use this EDSL. Typical usages:

* "Allocate" a parameter/local. Use `param` or `local` to obtain an immutable
  `Expression` which corresponds to the value of a new parameter/local. There
  are also mutable variants.

* An opaque `LVal` type is provided to uniformly deal with local
  reads/assignments and memory loads/stores. Once an `LVal` is instantiated, it
  can be used to read an `Expression` in the pure world, or set an `Expression`
  in the `EDSL` monad.

* Several side-effecting instructions can simply be composed with the
  monadic/monoidal interface, without the need to explicitly construct an
  anonymous block.

* When we need named blocks/loops with branching instructions inside, use the
 `block`/`loop` combinators which has the type `(Label -> EDSL ()) -> EDSL ()`.
  Inside the passed in continuation, we can use `break'` to perform branching.
  The `Label` type is also opaque and cannot be inspected, the only thing we know
  is that it's scope-checked just like any ordinary Haskell value, so it's
  impossible to accidently branch to an "inner" label.

The EDSL only checks for scope safety, so we don't mess up different locals or
jump to non-existent labels. Type-safety is not guaranteed (binaryen validator
checks for it anyway). Underneath it's just a shallow embedded DSL implemented
with a plain old state monad. Some people call it the "remote monad design
pattern".
