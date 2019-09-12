# Memory

WebAssembly [allows the allocation of
Memory objects](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/WebAssembly/Memory)
which can be read/write from JavaScript and Wasm codes. Allocations are made by
blocks of 64KB and the number of blocks can be increased later on. From
JavaScript, the array appears as an
[ArrayBuffer](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/ArrayBuffer).

On top of this, the RTS allocates larger MBlocks with ``getMBlocks`` in
``MBlockAlloc`` and "release" them with ``free``. Currently they are not really
released but marked as unused and they may be reused for another allocation.

## Memory object

The RTS Memory object provides primitives to load and to store data of different
types into the underlying allocated MBlocks. It simulates (tagged) pointers and
translates them into offsets. Tags are used to distinguish function pointers from
data pointers. The object also provides primitives such as ``memmove``,
``memcpy``, etc.

The initially allocated MBlocks corresponds to the "static memory": that is the
memory containing static data and functions (CAFs) usually allocated by a
program loader on native platforms. The Memory object provides ``headAlloced``
to distinguish heap pointers from other pointers.

**MemoryTrap**

When a symbol is accessed in memory (load/store), internally the primitives of
the Memory object are used. For debugging purpose it is possible to use those
defined in MemoryTrap object instead: they check the liveness of the pointed
MBlock and the pointer tag (which must be a data pointer).

## HeapAlloc

HeapAlloc object deals with dynamically allocated memory. HeapAlloc
allocates in two "pools" (groups of MBlocks):
  - one for unpinned objects smaller than a Block
  - one for pinned objects or objects larger than a Block

When a pool isn't large enough to alloc an object, a new pool large enough is allocated.

The Block granularity (smaller than MBlocks) isn't currently well implemented.
We should be able to allocate several groups of Blocks into a group of MBlocks
with a Block descriptor for each Block. Currently we only use a single Block
descriptor for each group of MBlocks!

## Garbage Collector

The GC object provides garbage collection primitives. It implements a copying
garbage collector.

## Registers

WebAssembly doesn't expose any register. Hence STG-machine global registers (Sp,
Hp, etc.) are mapped to a global static location in memory (in the first
MBlock). Cmm local registers are mapped to WebAssembly locals.

Note: passing all the function parameters in memory is an argument against using
Eval/Apply strategy instead of Push/Enter. Perhaps it should be reconsidered for
the WebAssembly target.
