## The runtime debugging feature

There is a runtime debugging mode which can be enabled by the `--debug` flag for `ahc-link`. When enabled, the compiler inserts "tracing" instructions in the following places:

* The start of a function/basic block
* `SetLocal` when the local type is `I64`
* Memory load/stores, when the value type is `I64`

The tracing messages are quite helpful in observing control flow transfers and memory operations. Remember to also use the `--output-link-report` flag to dump the linking report, which contains mapping from data/function symbols to addresses.

The runtime debugging mode also enables a "memory trap" which intercepts every memory load/store instruction and checks if the address is null pointer or other uninitialized regions of the linear memory. The program immediately aborts if an invalid address is encountered. (When debugging mode is switched off, program continues execution and the rest of control flow is all undefined behavior!)

### Complete list of emitted debugging log entries

* Assertions: some hand-written WebAssembly functions in `Asterius.Builtins` contain assertions which are only active in debugging mode. Failure of an assertion causes a string error message to be printed, and the whole execution flow aborted.
* Memory traps: In `Asterius.MemoryTrap`, we implement a rewriting pass which rewrites all load/store instructions into invocations of load/store wrapper functions. The wrapper functions are defined in `Asterius.Builtins`, which checks the address and traps if it's an invalid one (null pointer, uninitialized region, etc).
* Control-flow: In `Asterius.Tracing`, we implement a rewriting pass on functions (which are later invoked at link-time in `Asterius.Resolve`), which emits messages when:
    * Entering a Cmm function.
    * Entering a basic block. To make sense of block ids, you need to dump pre-linking IRs (which isn't processed by the relooper yet, and preserves the control-flow graph structure)
    * Assigning a value to an i64 local. To make sense of local ids, dump IRs. Also note that the local ids here doesn't match the actual local ids in wasm binary code (there is a re-mapping upon serialization), but it shouldn't be a problem since we are debugging the higher level IR here.

### Dumping IRs

There are multiple ways to dump IRs:

* Via GHC flags: GHC flags like `-ddump-to-file -ddump-cmm-raw` dump pretty-printed GHC IRs to files.
* Via environment variable: Set the `ASTERIUS_DEBUG` environment variable, then during booting, a number of IRs (mainly raw Cmm in its AST form, instead of pretty-printed form) will be dumped.
* Via `ahc-link` flag: Use `ahc-link --output-ir` to dump IRs when compiling user code.
