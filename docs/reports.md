# Status reports

This page maintains a list of weekly status reports for the project.

## 2019-04-15

Covers last week.

Ongoing work:

* Trying to use `inline-js-core` to run asterius compiled wasm/js. Works for:
    * Running regular `main` actions and retrieving results.
    * Calling exported rts internals to manipulate closures on the Haskell heap.
* Currently into a debugging rabbit hole related to getting simple expression splices to run and return serialized result:
    * We can compile a splice of type like `Q Exp`, obtain the wasm closure, then use utilities from `ghci` to initiate a TH state and actually run it.
    * We're hit with `unreachable`s at runtime; they're due to unresolved symbols substituted to an invalid address so to let linking pass, meaning there's hidden bug in the logic of either the linker itself, or how we obtain and compile the splices.
* Miscellaneous other improvements in [`inline-js`](https://github.com/tweag/inline-js).
    * No more radical API changes are planned from now, and we have proper haddock documentation. Spread the word, give it a try in your projects too :)

Estimated work for the week:

* Solve the runtime error related to the TH splices.

## 2019-04-08

Covers last week.

Ongoing work:

* Finished all required `inline-js` improvements to make it feasible as an `iserv` component. Notable changes include:
    * The half-baked JSON implementation is removed. All JSON-related logic is now based on `aeson` and the higher-level `inline-js` package is required.
    * We used to only pass encoded JSON messages via the IPC interface. Now we pass binary messages, and thus we can directly allocate a node `Buffer` from a Haskell `ByteString`, and return the `JSVal` handle to Haskell.
    * We implemented send/receive queues and fixed a race condition related to multiple receivers to a same `Transport`.
    * We switched the Haskell/`node` pipes from `stdin`/`stdout` to using file descriptors. This fixes the issue when `inline-js-core` executes wasm/js code produced by asterius, but due to calling `console` API in generated code, the pipes are corrupt and leaves the `ahc-iserv` process in an undefined state.
    * We investigated the issue of supporting both static/dynamic `import` in the eval server's input code and did a prototype using the experimental `vm.SourceTextModule` interface in nodejs. Conclusion: it's not worth the trouble, and although static `import`s aren't supported by the eval server, the issue can be worked around without major changes in the asterius js codegen.
* Updated ghc and fixed #98, working around a Cabal bug (#4651) impacting our Cabal support.

Third-party contributions:

* Thanks to Stuart Geipel(@pimlu) for discovering an issue in the garbage collector and providing a minimal repro (#97). Investigation of this issue required fixing the "memory traps"/"tracing" rewriting passes previously removed in order to speed up the linker, so is currently scheduled behind ongoing TH work.
* Thanks to Yuji Yamamoto(@igrep) for discovering an issue in the Cabal support (#98), and an issue in the JSFFI implementation (#102).

Estimated work for the week:

* Just finish the `iserv` implementation (on the node side; nothing left to do on the Haskell side), get a `th` unit test up and running.

## 2019-04-01

Covers last week.

Ongoing work:

* Refactored `inline-js-core`/`inline-js` and implemented the binary IPC interface. This is a part of the work on the `node` side of `iserv`.
    * `inline-js` was based on a textual IPC interface using JSON messages via `readline`. When writing `iserv` logic this proved to be an annoyance; we'd like to reuse the `Message` type and its `Binary` instance in `libiserv`, instead of coming up with a JSON schema for every message; also it doesn't play nice with messages with blobs. Now `inline-js-core` directly transmits binary IPC, and it can be backed by any backend (e.g. stdio or network).
* `inline-js-core` now supports evaluating ES6 module code containing dynamic `import()`s. This is critical to `iserv` implementation since our code generator also generates ES6 modules.
* Did some linker profiling and revealed that repeated serialization is a previously undiscovered bottleneck:
    * In `ahc-ld`, we pick up all library archives and object files, emit a persistent "linker state" as a pseudo-executable, which is later read by `ahc-dist` to produce executable wasm/js.
    * When no-DCE mode is on, the linker state contains data/functions in all libraries, and although we have some degree of lazy-loading, we can't lazily save things without forcing their evaluation. Thus come the extra costs.

Estimated work for this week:

* Support evaluating static `import` declarations in `inline-js-core`, since they are included in our js stubs. If proven to be hard, we add a switch in the code generator to only emit `import()`s instead.
* Implement the `iserv` message handlers in node.

## 2019-03-25

Covers last week.

Ongoing work:

* Implemented the linker's "no-DCE" mode. Dead code elimination is now optional and can be switched off via `--no-gc-sections`, and this is preferrable behavior for dynamic linking. Note that for regular linking in `ahc-link`, no-DCE mode takes considerably longer and emits a `.wasm` as large as 40MB.
* Removed all "rewriting passes" for code in the linker.
    * Previously, the linker grabs a self-contained set of data/functions, perform whole-program AST rewritings for several times, then feed the output to `wasm-toolkit`/`binaryen` backend to emit real WebAssembly binaries.
    * By removing these passes, there's a roughly 75% speedup measured in `fib` for no-DCE mode, around one minute from loading all archives to `node` compiling and running output code.
    * Another advantage of removing those passes: there's now a lot less linker state to keep track of, thus clearing up the last mile path to fully incremental linking.
    * Minor downsides to the linker refactorings:
        * We used to have an "EmitEvent" primitive in our IR which allows embedding any Haskell string as an "event" and emit it when using the monadic EDSL to construct WebAssembly code. This mechanism now degraded from allowing any string to an enumerable set of events.
        * When the linker found a function which contains a compile-time error message, it used to emit a stub function which reports that message and crashes at runtime. Now we don't emit those anymore, and when such a function is called at runtime, the compile-time error message is not reported.

Other work, when the main thread stalled:

* Implemented the WebAssembly tail call opcodes in `wasm-toolkit` and the tail-calls mode in asterius, enabled with `--tail-calls`, tested on CI with V8 team's latest nodejs build.
    * The tail-calls mode gets rid of trampolining during Haskell/Cmm execution. When the user is in full control of the execution platform and doesn't mind adding a V8 flag to enable tail calls, this should result in better performance.
    * It's unsupported by the binaryen backend yet, but we managed to re-enable the binaryen relooper, which at this moment still emits better binary than our own relooper. It seems the binaryen relooper works fine only if no value is returned from any basic block. Back when we didn't roll our own relooper and didn't realize this, it was a constant source of undefined behavior and debugging nightmares.
* Simplified the `binaryen` build script to not rely on `ar -M`. Got rid of CPP usage and fixed a build warning on non-macOS platforms.

Planned work for the week:

* Conclude all linker work.
    * All performance potential for the current linker architecture is possibly squeezed out now. It's not as fast as we want, but it's easily cachable. Once we have a fully persistent linker state, we can cache and reuse it on the first TH run, so it won't take minutes to run splices.
* Finish the `node` side of `iserv` logic and get preliminary TH support.

## 2019-03-18

Covers last week.

Ongoing work:

* Removed the memory & table sections in output wasm binaries; now we initiate wasm memory/table in js, and it's possible to transfer them across multiple asterius instances.
    * Rationale: The wasm spec includes data/element segment sections to initialize certain ranges of the memory/table, and also separate memory/table sections to implicitly create an empty memory/table. When incrementally linking multiple compilation units, we need to take memory/table of the previous asterius instance, "append" some data & code while preserving previous content.
    * This involved some unexpected debugging sessions, and we also needed to patch the in-tree `binaryen` library to avoid dropping the `binaryen` backend. Thankfully we sorted out that mess
* Figured out that running regular linker passes on all loaded boot library archives is *incredibly* slow, and it was also harder than expected to add an incremental mode.
    * Besides regular wasm function/data, there are also other stuff in asterius compilation units like FFI marshal info, pooled error messages, etc, which used to work nicely with "one-shot" linking, but harder for incremental mode.
    * A failed attempt to speed things up: use global string interning for symbols, which are central to almost every function in asterius modules. Surprisingly no noticable speedup after the patch is applied, so this remains unmerged.

Planned work for this week:

* Factor out a set of self-contained "linker state" and "runtime state" types, which are guaranteed serializable and thus easy to transfer. Memory/table was not enough, we also needed to take care of things like JSFFI imports/exports, SPT, eventlogs, error message pools, etc.
* Generate both "vanilla" and "dynamic" ways of wasm code in asterius object files. The "dynamic" code should already be wasm binaries without requiring whole-program rewriting passes, and we also include a custom section with symbol locations in the binary, so only a fast link-time relocating pass is required when loading a "dynamic" way wasm binary.

## 2019-03-10

Covers this week.

Ongoing work:

* About the `iserv` experiment:
    * Started implementing `ahc-iserv`, which is capable of receiving messages from the host `ghc` process.
    * Originally, GHC compiles TH splices to bytecode and sends them to `iserv`. Given we don't support bytecode (yet), we hacked on that direction a little bit, and now we can compile splices to wasm modules using our regular cmm to wasm functionality.
    * Updated `ghc` again, and included several new packages into the boot libs: `filepath`/`time`/`unix`/`directory`/`ghc-boot`/`ghc-heap`/`ghci`. A lot of functionalities in these packages won't work yet, they exist to support `ghci`, which includes necessary code to run TH and communicate with the host `ghc` process.
* Miscellaneous other improvements:
    * Implemented more accurate dependency analysis for JSFFI. Previously, whenever something in a module was included in linker output, all JSFFI info of that module is also included, resulting in potentially bloated wasm import section and generated wrapper js module. Now the output wasm code and wrapper js modules are even slimmer.
    * Removed the legacy `AsteriusStore` type and related logic from the linker. It was a combination of two mappings: from symbols to module names and from module names to lazily loaded modules. It was closely related to the legacy "object store" mechanism which stores wasm objects elsewhere and doesn't respect ghc's object output path, and since we fixed that for Cabal support, it's time to pull this weed and simplify things. The linker now solely operates on the `AsteriusModule` type.
    * Implemented lazier deserialization for `AsteriusModule`. Now when loading a wasm object, we defer the deserialization of individual entities(data segments or functions) until they're actually included into the output module. Compared to the old module-level lazy loading mechanism, there's no noticable increase on memory usage.
    * Optimized the code of dependency analysis.
    * Enabled the `binaryen` backend to output wasm name section in debug mode. Previously, when wasm execution traps, V8 would output a stack trace, with only function ids for wasm functions. Now we spare the trouble of reading the linker report and remapping ids to function names when debugging. Note that the `wasm-toolkit` backend doesn't handle the name section yet.

Planned work for next week:

* The plumbings from `ahc` to `ahc-iserv` are ready, now we need to implement an eval server running on `node` and the RPC logic between it and `ahc-iserv`. The eval server should be capable of:
    * Initializing rts state
    * Receiving a wasm module from `ahc-iserv` and remapping existing wasm memory and table
    * Running a splice, sending serialized results to `ahc-iserv`
    * Sending necessary ghc queries when running splices
* The requirements listed above also implies that we need to finish the linker's incremental mode first.
* The runtime also needs to support:
    * Interleaved static/non-static memory regions, and reserving an address range for data segments in new incoming wasm modules
    * Minimal Haskell exception handling support, so we can reply a `QFail` in `ahc-iserv` correctly when an exception is thrown instead of silently dying. (Informative error messages which include information recovered from `Show` instance of `SomeException` are harder to implement)

## 2019-03-04

Covers last week.

Completed work:

* Thorough refactorings in the linker to improve performance & modularity.
    * The linker used to produce small code faithfully, but the speed was slow. Moreover, the code was quite messy, with multiple pieces of whole-program traversal code scattered across a single module.
    * In order to deliver TH support, we need the linker to be:
        * Fast, loading archives & objects and performing all necessary rewriting passes quickly. Most likely the expensive dead code elimination optimization will hurt us in this scenario and we need a switch to disable it.
        * Incremental. Loading some new object code into the linker state must not trigger expensive re-compilation of old code.
    * We tidied up the linker code, moved each rewriting pass to a separate module and fused them into a single pipeline. The fusion guarantees that after dependency analysis, the AST is only traversed once to perform all necessary rewritings and produce executable wasm code.
* Updated `ghc` and standard libraries to a recent revision (`8.7.20190217` -> `8.9.20190301`). This includes Moritz Angermann's work to improve `iserv`.

Planned work for the week:

* Start experimenting with `iserv` stuff.
* Continue working to improve the linker:
    * The linker code now has some space for adding incremental linking logic. Whether/how it works for TH depends on more knowledge accumulated from `iserv` experiments.
    * Besides rewriting passes, another major performance bottleneck is dependency analysis, where we start from a global "store" and root symbols to scrap a self-contained module. We'll deal with this one for this week. For TH, we'll explore the possibility of adding a switch for faster linking & larger output code.
    * All rewriting passes for non-debug mode are migrated to the new pipeline, but two additional passes for debug mode are left untouched for now: "memory traps" & "tracing". They are tricky to get right in the new framework, and given debug mode doesn't impact regular users, these two may be left as they are for a bit more time.

## 2019-02-22

Covers this week.

Completed work:

* Finished preliminary Cabal support.
    * The executable targets are implemented. It's possible to call `ahc --make` directly or via `ahc-cabal new-build` to get an "executable". The "executable" can be quickly converted to node/web artifacts by `ahc-dist`.
    * `ahc-cabal` is a simple wrapper of `cabal`. `stack` is possibly also supported if we provide the same configure flags. Might worth a try in the future.
    * Cabal tests/benchmarks/documentation is not implemented yet.
        * `haddock` won't work yet.
        * Tests/benchmarks should build fine like normal executables, but Cabal can't run them like vanilla executables yet. The executables can still be "run" with `ahc-dist --run`.
    * `ahc-dist` works similarly like the legacy `ahc-link` tool. They share most command line arguments, except `ahc-dist` expects `--input-exe`; it starts from executable files, where `ahc-link` starts from Haskell sources.

Third-party contributions:

* Thanks to Piotr Majkrzak(@majkrzak) for a PR fixing a `--browser` problem (#73), and issue #70 for reducing Docker image size, #74 for simplifying export module interface.

Planned work for next week:

* Start working on Template Haskell/GHCi/Plugins (#54). This is the last major planned feature of 2019 Q1.
* Other potential work, in case my main thread become stalled like it always did in the past:
    * Easy improvements in gc, e.g. adding stats.
    * Experiment on creating a more asynchronous runtime. A relevant issue will be added shortly.

## 2019-02-18

Covers last week.

Completed work, mainly routine maintainence:

* Updated `ghc` and standard libraries to a recent revision (`8.7.20181115` -> `8.7.20190217`)
* Updated `binaryen` and `wabt` toolchains
* Added the experimental bulk memory opcodes in the `binaryen` Haskell bindings

Ongoing work:

* Finished implementation of library targets for Cabal.
    * The `ahc` executable was only meant to be invoked from the boot script. Now it can be run to compile any user input Haskell/Cmm code and produce object files.
    * `ahc-ar` is implemented to generate `.a` static libs, and those libs contain symbol indices required by asterius linker. The static libs can later be used as linker inputs.
    * Currently, no patching to Cabal is required.

Planned work for this week:

* Finish implementation of executable targets for Cabal.
    * When compiling the "executable" targets, final linking (including any necessary LTO & rewriting passes) is done. The resulting file can be quickly converted to node/web artifacts by an external tool.
    * The legacy mechanism for storing/retrieving wasm objects will be removed, and we'll only rely on the static libs and input object files for linking.
    * Add unit tests for compiling & running stuff via `cabal-install`.
* Some improvements in gc, if we manage to finish Cabal stuff.

## 2019-02-11

Covers last week.

Completed work:

* Fixed known regressions of GC & released the initial version.

Additional known drawbacks of the initial version (see report of previous week):

* There is currently no runtime logs/stats about garbage collection.
* There is currently no tunable parameters; at least we should allow specifying when "real" gc happens and when we just allocate a new nursery instead of traversing the heap (e.g. "real" gc happens when the live semispace size grows beyond a threshold)
* `StgTSO`/`StgStack` objects are unnecessarily pinned to simplify scheduler implementation a bit, but they really should be unpinned.

Planned work for this week:

* Start working on Cabal support.
* Some easy improvements in gc, e.g. adding stats/logs, implementing parameters.

## 2019-02-04

Covers last week.

Ongoing work:

* Finished the preliminary implementation of GC.
    * To increase reliability and catch regressions, after each GC pass, the recycled space is zeroed. If the tospace still contains pointers to recycled space (which is definitely a bug), the program is likely to crash early.
    * This has helped us to identify & fix a few bugs in the GC implementation. Right now there is only one regression left: the todomvc example crashes after initial loading completes. The crash goes away if we don't zero recycled space, but it's not a good idea to just turn that off and pretend there's no bug!

Remaining work for GC:

* Fix the todomvc regression. Given GC is such a critical component in the runtime, it's probably also good timing to integrate some more unit tests from the GHC test suite.
    * This also needs some improvement in our debugging infrastructure: our memory traps (wasm read/write barriers) is currently unaware of recycled/live space, and now we should make it cooperate with the allocator to catch invalid access to recycled space earlier.

Known drawbacks of current GC implementation once it's fully fixed & merged:

* No generational GC yet, so high GC overhead if a large volume of long-lived data is retained through program execution.
* Heap fragmentation is more severe when allocating a lot of small pinned objects.
* `Weak#` support is expected to split into two different stages and will land after initial GC merge:
    * Support for running "C finalizers" added by the `addCFinalizerToWeak#` primop. Here, the "C finalizers" are really just JavaScript functions, and the "function pointers" are JavaScript references.
    * Support for running arbitrary `IO` action as finalizers. This task requires support for Haskell multi-threading, and given multi-threading is not a scheduled goal of 2019 Q1, this will come later.
* Haskell closures exported to JavaScript using `makeHaskellCallback*` cannot be automatically recycled when they aren't used anymore. This is due to JavaScript's lacking of finalizers; users will need to call `freeHaskellCallback*` by hand to prevent leaking on the Haskell side.

We'll yield to Cabal support & TH/GHCi/Plugins support after the first version of GC is delivered. There's definitely room for improvement later (e.g. reduce heap fragmentation, different GC algorithms for different workloads, more detailed GC statistics, etc), but those will be managed by separate tickets.

## 2019-01-28

Covers last week.

Ongoing work:

* More work to improve the sanity checker and get GC near completion:
    * The sanity checker spotted a fatal situation where previously unreachable static closures become reachable again. More experiments in this direction invalidated a previous conjecture that no special treatments for static closure is required as long as they have valid block descriptors and can be moved just like dynamic ones.
    * The solution to the problem above is not hard: when scanning an info table, we also follow the SRT if it's present, and we still need to identify static/dynamic closures and prevent moving static ones. This is implemented in the sanity checker.
    * The sanity checker is no longer backed by explicit recursion. When scanning a long chain of closures, we won't run out of JavaScript stack space.
    * Adjusting the codegen & standard libraries to cope with upcoming GC:
        * The `makeHaskellCallback*` interfaces now properly allocate a stable pointer for the exported Haskell closure. This is to ensure that they remain valid when later called from JavaScript, even after GC runs.
        * The function closures of `foreign export javascript` clauses are recognized and become GC roots for similar reasons.
        * `Asterius.Types` is moved from `ghc-prim` to `base`, so the `JSVal` type can be backed by `StablePtr`. The GC will use a tag bit to identify regular stable pointers and JavaScript references, and automatically free unused references.
        * `Integer` is promoted to a standalone datatype, so the GC can scan reachable `Integer`s and free unreachable ones which point to `BigInt`s.

Remaining work for GC:

* Implement evac/scav functionality.
* Implement support for `Weak#`s based on the constraint that no Haskell execution is required when firing a finalizer.

## 2019-01-21

Covers last week.

Ongoing work:

* Bugfixes & partially done GC work of later stages (recycling Haskell heap space & JavaScript references):
    * Unified treatment of regular `StablePtr#`s and `JSVal` in the runtime. They are identified by a tag bit, and GC will be able to recognize live `JSVal`s when scanning the Haskell heap.
    * Added the SPT as sanity check/garbage collection roots.
    * Fixed a sanity check bug related to AP/PAP heap objects. This could be triggered when checking the SPT after passing a closure built by chained `rts_apply` calls to `rts_evalStableIO`.
    * Reimplemented the linker layout code. We now put non-closures & closures to separate regions, and the regions are statically allocated block groups which is handled by the sm uniformly like dynamically allocated groups. This enables us to treat static closures as if they're dynamic ones without any special hack.
    * Simplified the block allocator. We no longer manage block at 4K granularity; now we only manage 1M sized ones. Pros & cons:
        * Much fewer blocks needed to manage, so simpler/faster runtime code.
        * Larger nurseries mean the Haskell mutator code signals `HeapOverflow` much less frequently. Should reduce amortized GC cost.
        * The main drawback is increated heap fragmentation when it comes to allocating lots of small pinned objects. This is not yet a primary concern, and can be addressed later by a hybrid GC which switches to non-moving mark-sweep algorithm for block groups with pinned objects.
    * Added functionality to free block groups, so they can later be reused without growing the linear memory. Their payloads can be zeroed to eliminate a potential attack surface (or for better reproduction of bugs in case something goes wrong)
    * Moved `allocate*` to the JavaScript runtime and properly implemented `allocatePinned`. Previously it was simply an alias of `allocate` since we didn't move anything around.

Planned work for next week:

* Wrap up all GC work and get a fully functional GC up & running. This was originally planned to finish by end of last week, but fell behind schedule due to the hidden workload described above. Required work:
    * Implement evac/scav functionalities in the runtime.
    * Remove the now obsolete symbol table exporting mechanism, and any closure required to survive all GC scans need to be explicitly present in the SPT upon startup.
    * Remove the terrible hacks of directly coercing between GC pointers of boxed types and regular `Addr#`s when crossing the FFI boundary. Now we must properly pass `StablePtr#`s.
    * Breaking refactorings in the current boot libs:
        * The `JSVal` family of types need to be moved from `ghc-prim` to `base` (or a separate package depending on `base`), since it needs to be a `newtype` wrapper of `StablePtr` which is defined in `base`.
        * The `Integer` type gets promoted to a standalone datatype. We still use tagging to identify small `Integer`s and `BigInt`s which is managed by SPT just like other `JSVal`s.

## 2019-01-13

Covers the last week. The week before was new year vacation; happy new year everyone!

Completed & ongoing work:

* Completed the "modularized runtime" refactorings. (#50)
* Drafted three feature roadmaps:
    * Implement proper garbage collection (#52)
    * Implement Cabal support (#53)
    * Implement support for Template Haskell/GHCi/Plugins (#54)
    * The above proposals are scheduled to be completed on 2019 Q1.
* Began working on GC, and finished the first stage: accurate heap objects traversal.
    * Identify different types of data sections in object files (regular bytes/info tables/closures). The info table addresses are emitted into generated JavaScript to allow an accurate info table sanity check.
    * Implemented runtime utils for directly manipulating the linear memory with tagged addresses.
    * Implemented the sanity check which traverses the whole heap and visits every live object. All existing unit tests pass this check.

Planned work for next week:

* Finish the second stage of GC support: evacuate/scavenge.
    * See #52 for details. After this is finished, GC will be operational.
    * Support for handling `JSVal` and `Weak#` is scheduled in later stages.

Originally scheduled but lowered priority:

* Improving the Cloudflare worker demo. We're prioritizing more pressing issues like GC over specific use cases right now.

Special thanks to Moritz Angermann (@angerman) for contributing a patch (#55) for fixing `ar` problem on macOS, and helping to improve macOS & cabal support, discovering a GHC bug related to tables-next-to-code (#16174).

## 2018-12-28

Covers the last two weeks.

Completed work:

* Significant refactorings in the runtime.
    * Pruned ~500 loc weed code in `Asterius.Builtins` without breaking tests.
    * Enhanced the scheduler.
        * Previously, when entering a Haskell thread, we evaluated to completion and checked the return code; if something goes wrong, we would just throw an error.
        * Now, the scheduler is capable of handling certain scenarios like heap overflow and resuming Haskell execution.
    * Enhanced the storage manager.
        * Previously, the block allocator always triggered a `grow_memory` opcode when requesting blocks, making a lot of `Array#`/`ByteArray#` related primops rather in-efficient. Also, we allocated a huge heap (defaults to 1G) upon startup and pretended it won't run out.
        * Now, the block allocator grows the linear memory efficiently. And the initial heap is small (4K for both the nursery and the object pool); an overflow condition automatically extends it.
* Implemented the "persistent vault" feature.
    * Every asterius instance has a KV store called a "vault" which can be accessed in both Haskell/JavaScript. It can be used to transfer state across instances, so when an instance throws errors we can't handle, we can restart a new one without losing context.
    * This is a part of the work for Cloudflare Worker showcase.
* Delivered a working TodoMVC example and issued a blog post.
* Other notable bugfixes/improvements:
    * Fixed the `dirty_MUT_VAR` write barrier for `MutVar#`s. All non-atomic `MutVar#`/`IORef`/`STRef` operations now work. This is a part of the work for TodoMVC showcase.
    * We implemented UTF8/UTF16-LE/UTF32-LE/Latin-1 encoding/decoding in the runtime. This is a part of the work for `text` support.
    * The `makeHaskellCallback` functions are slightly more efficient by avoiding the overhead of allocating `StablePtr`s.

On-going work not completed yet:

* Modularizing the runtime.
    * Previously, the runtime is a single monolithic JavaScript script which is pasted into the output script. We'd like to split it to modules, and allow users to supply their own module to override the default behavior (evaluating `Main.main` once).
    * Rationales:
        * For users, it's much more convenient to implement custom logic via a proper module file. Especially in the Cloudflare Worker case, where we need:
            * Fully synchronous initialization
            * Capturing errors/rebooting a new instance
        * It's now possible to write tests for individual pieces of the runtime. This is critical to improve the runtime's reliability.
        * There were some pasted parts in the monolithic runtime; now we can properly reuse code.
        * It's also convenient to inject link-time information into the runtime.
    * We've introduced `parcel` into our toolchain to implement a "bundling" functionality: at link-time, re-generating a standalone `.js` file containing all the runtime modules. This is already implemented.
    * We're gradually splitting the monolithic runtime to modules, taking care not to break stuff. Not completed; so far so good.
* Delivering a non-trivial Cloudflare Worker demo.
    * We already have a trivial one working. It's trivial because it only does synchronous request -> response computation; more "real-world" ones will need to invoke asynchronous JavaScript computation (e.g. using Fetch API)
    * Dealing with JavaScript asynchronous computation is not quite tolerable yet; we need to litter the code with `makeHaskellCallback*`, at least one such call for a JavaScript `await`.
    * We currently have two potential approaches of improving user experience with async js code:
        * Implement some CPS-based EDSL to automatically deal with callback marshaling.
        * Implement a simple IO manager in the runtime which is capable of suspending Haskell threads when calling async js code and resuming them upon resolving/rejecting.
        * The second one sounds much more decent, but has a high difficulty level. We'll start from the first one.

Rough plans for next week:

* Finish the work on modularizing the runtime, document new behavior of JavaScript generation, then merge to `master`.
* Deliver a more decent Cloudflare worker demo which calls some async js code.
