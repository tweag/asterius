# Template Haskell

We added
[hooks](https://github.com/TerrorJack/ghc/blob/15bc2928590c41e445565263a688548746cad5c7/compiler/main/Hooks.hs#L126)
to these `iserv`-related functions:

* `startIServ`
* `stopIServ`
* `iservCall`
* `readIServ`
* `writeIServ`

The hook of `hscCompileCoreExpr` is also used. The implementation of the hooks
are in
[`Asterius.GHCi.Internals`](https://github.com/tweag/asterius/blob/asterius-TH/asterius/src/Asterius/GHCi/Internals.hs)

Normally, `startIServ` and `stopIServ` starts/stops the current `iserv` process.
We don't use the normal `iserv` library for `iserv` though; we use
`inline-js-core` to start a node process. `inline-js-core` has its own mechanism
of message passing between host/node, which is used for sending JavaScript code
to node for execution and getting results. In the case of TH, the linked
JavaScript and WebAssembly code is sent. Additionally, we create POSIX pipes and
pass the file descriptors as environment variables to the sent code; so most TH
messages are still passed via the pipes, like normal `iserv` processes.

The `iservCall` function is used for sending a `Message` to `iserv` and
synchronously getting the result. The sent messages are related to linking, like
loading archives and objects. Normally, linking is handled by the `iserv`
process, since it's linked with GHC's own runtime linker. In our case, porting
GHC's runtime linker to WebAssembly is going to be a huge project, so we still
perform TH linking in the host `ahc` process. The linking messages aren't sent
to node at all; using the hooked `iservCall`, we maintain our own in-memory
linker state which records information like the loaded archives and objects.

When splices are executed, GHC first emits a `RunTH` message, then repeatedly
queries the response message from `iserv`; if it's a `RunTHDone`, then the dust
settles and GHC reads the execution result. The response message may also be a
query to GHC, then GHC sends back the query result and repeat the loop. In our
case, we don't send the `RunTH` message itself to node; `RunTH` indicates
execution has begun, so we perform linking, and use `inline-js-core` to load the
linked JavaScript and WebAssembly code, then create and initialize the Asterius
instance object. The splice's closure address is known at link time, so we can
apply the TH runner's function closure to the splice closure, and kick off
evaluation from there. The TH runner function creates a fresh `IORef QState`, a
`Pipe` from the passed in pipe file descriptors, and uses `ghci` library's own
`runTH` function to run the splice. During execution, the `Quasi` class methods
may be called, and on the node side, they are turned to `THMessage`s sent back
to the host via the `Pipe`, and the responses are then fetched.

Our function signatures of `readIServ` and `writeIServ` are modified. Normal GHC
simply uses `Get` and `Put` in the `binary` library for reading/writing via the
`Pipe`, but we simply read/write a polymorphic type variable `a`, with `Binary`
and `Typeable` constraints. Having `Binary` constraint allows fetching the
needed `get` and `put` functions, and `Typeable` allows us to inspect the
message pre-serialization. This is important, since we need to catch `RunTH` or
`RunModFinalizer` messages. As mentioned before, these messages aren't sent to
node, and we have special logic to handle them.

As for `hscCompileCoreExpr`: it's used for compiling the `CoreExpr` of a splice
and getting the resulting `RemoteHValue`. We don't support GHC bytecode, so we
overload it and go through the regular pipeline, compile it down to Cmm, then
WebAssembly, finally performing linking, using the closures of the TH runner
function and the splice as "root symbols". The resulting `RemoteHValue` is not
"remote" though; it's simply the static address of the splice's closure, and the
TH runner function will need to encapsulate it as a `RemoteRef` before feeding
to `runTH`.

TH WIP branch:
[`asterius-TH`](https://github.com/tweag/asterius/tree/asterius-TH)

GitHub [Project](https://github.com/tweag/asterius/projects/1) with relevant
issues
