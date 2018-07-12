## The runtime debugging feature

There is a runtime debugging mode which can be enabled by the `--debug` flag for `ahc-link`. When enabled, the compiler inserts "tracing" instructions in the following places:

* The start of a function/basic block
* `SetLocal` when the local type is `I64`
* Memory load/stores, when the value type is `I64`

The tracing messages are quite helpful in observing control flow transfers and memory operations. Remember to also use the `--output-link-report` flag to dump the linking report, which contains mapping from data/function symbols to addresses.

The runtime debugging mode also enables a "memory trap" which intercepts every memory load/store instruction and checks if the address is null pointer or other uninitialized regions of the linear memory. The program immediately aborts if an invalid address is encountered. (When debugging mode is switched off, program continues execution and the rest of control flow is all undefined behavior!)
