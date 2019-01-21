# Status reports

This page maintains a list of weekly status reports for the project.

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

Planned work for last week:

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
