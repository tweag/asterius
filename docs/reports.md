# Status reports

This page maintains a list of weekly status reports for the project.

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
