# The runtime debugging feature

There is a runtime debugging mode which can be enabled by the `--debug` flag
for `ahc-link`. When enabled, the compiler inserts "tracing" instructions in
the following places:

* The start of a function/basic block
* `SetLocal` when the local type is `I64`
* Memory load/stores, when the value type is `I64`

The tracing messages are quite helpful in observing control flow transfers and
memory operations. Remember to also use the `--output-link-report` flag to dump
the linking report, which contains mapping from data/function symbols to
addresses.

The runtime debugging mode also enables a "memory trap" which intercepts every
memory load/store instruction and checks if the address is null pointer or
other uninitialized regions of the linear memory. The program immediately
aborts if an invalid address is encountered. (When debugging mode is switched
off, program continues execution and the rest of control flow is all undefined
behavior!)

## Virtual address spaces

Remember that we're compiling to `wasm32` which has a 32-bit address space, but
the host GHC is actually 64-bits, so all pointers in asterius are 64-bits, and
upon `load`/`store`/`call_indirect`, we truncate the 64-bit pointer, using only
the lower 32-bits for indexing.

The higher 32-bits of pointers are idle tag bits at our disposal, so, we
implemented simple virtual address spaces. The linker/runtime is aware of the
distinction between:

* The *physical* address, which is either an `i32` index of the linear memory
  for data, or an `i32` index of the table for functions.

* The *logical* address, which is the `i64` pointer value we're passing around.

All access to the memory/table is achieved by using the *logical* address. The
access operations are accompanied by a *mapping* operation which translates a
*logical* address to a *physical* one. Currently it's just a truncate, but in
the future we may get a more feature-complete `mmap`/`munmap` implementation,
and some additional computation may occur when address translation is done.

We chose two magic numbers (in `Asterius.Internals.MagicNumber`) as the tag
bits for data/function pointers. The numbers are chosen so that when applied,
the *logical* address does not exceed JavaScript's safe integer limit.

When we emit debug log entries, we may encounter various `i64` values. We
examine the higher 32-bits, and if it matches the pointer tag bits, we do a
lookup in the data/function symbol table, and if there's a hit, we output the
symbol along the value. This spares us the pain to keep a lot of symbol/address
mappings in our working memory when examining the debug logs. Some false
positives (e.g. some random intermediate `i64` value in a Haskell computation
accidently collides with a *logical* address) may exist in theory, but the
probability should be very low.

Note that for consistency between vanilla/debug mode, the virtual address
spaces are in effect even in vanilla mode. This won't add extra overhead, since
the truncate instruction for 64-bit addresses has been present since the
beginning.

## Complete list of emitted debugging log entries

* Assertions: some hand-written WebAssembly functions in `Asterius.Builtins`
  contain assertions which are only active in debugging mode. Failure of an
  assertion causes a string error message to be printed, and the whole
  execution flow aborted.

* Memory traps: In `Asterius.MemoryTrap`, we implement a rewriting pass which
  rewrites all load/store instructions into invocations of load/store wrapper
  functions. The wrapper functions are defined in `Asterius.Builtins`, which
  checks the address and traps if it's an invalid one (null pointer,
  uninitialized region, etc).

* Control-flow: In `Asterius.Tracing`, we implement a rewriting pass on
  functions (which are later invoked at link-time in `Asterius.Resolve`), which
  emits messages when:
    - Entering a Cmm function.
    - Entering a basic block. To make sense of block ids, you need to dump
      pre-linking IRs (which isn't processed by the relooper yet, and preserves
      the control-flow graph structure)
    - Assigning a value to an i64 local. To make sense of local ids, dump IRs.
      Also note that the local ids here doesn't match the actual local ids in
      wasm binary code (there is a re-mapping upon serialization), but it
      shouldn't be a problem since we are debugging the higher level IR here.

## Dumping IRs

There are multiple ways to dump IRs:

* Via GHC flags: GHC flags like `-ddump-to-file -ddump-cmm-raw` dump
  pretty-printed GHC IRs to files.

* Via environment variable: Set the `ASTERIUS_DEBUG` environment variable, then
  during booting, a number of IRs (mainly raw Cmm in its AST form, instead of
  pretty-printed form) will be dumped.

* Via `ahc-link` flag: Use `ahc-link --output-ir` to dump IRs when compiling
  user code.
