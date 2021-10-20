import "./rts.setimmediate.mjs";
import { modulify } from "./rts.modulify.mjs";
import { ReentrancyGuard } from "./rts.reentrancy.mjs";
import { EventLogManager } from "./rts.eventlog.mjs";
import { Tracer } from "./rts.tracing.mjs";
import { Memory } from "./rts.memory.mjs";
import { MemoryTrap } from "./rts.memorytrap.mjs";
import { HeapAlloc } from "./rts.heapalloc.mjs";
import { JSValManager } from "./rts.jsval.mjs";
import { StablePtrManager } from "./rts.stableptr.mjs";
import { StableNameManager } from "./rts.stablename.mjs";
import { StaticPtrManager } from "./rts.staticptr.mjs";
import { Scheduler } from "./rts.scheduler.mjs";
import { IntegerManager } from "./rts.integer.mjs";
import { TimeCBits } from "./rts.time.mjs";
import { GC } from "./rts.gc.mjs";
import { ExceptionHelper } from "./rts.exception.mjs";
import { Messages } from "./rts.messages.mjs";
import { FloatCBits } from "./rts.float.mjs";
import { Unicode } from "./rts.unicode.mjs";
import { Exports } from "./rts.exports.mjs";
import { FS } from "./rts.fs.mjs";
import { SymbolTable } from "./rts.symtable.mjs";
import { WASI } from "./rts.wasi.mjs";
import * as rtsConstants from "./rts.constants.mjs";

export async function newAsteriusInstance(req) {
  const __asterius_components = {};

  let __asterius_table_base = new WebAssembly.Global(
      { value: "i32", mutable: false },
      req.defaultTableBase // TODO: make dynamic.
    ),
    __asterius_memory_base = new WebAssembly.Global(
      { value: "i32", mutable: false },
      req.memoryBase // TODO: make dynamic.
    );

  let mkSptEntries = function (spt_offset_entries) {
    const absolute_spt_entries = new Map();
    for (const [k, off] of spt_offset_entries.entries()) {
      absolute_spt_entries.set(
        k,
        __asterius_memory_base.value + off
      );
    }
    return absolute_spt_entries;
  };

  let mkInfoTable = function (offset_info_tables) {
    if (!(typeof offset_info_table === "undefined")) {
      const absolute_info_tables = new Set();
      for (const off of offset_info_tables.keys()) {
        absolute_info_tables.add(
          __asterius_memory_base.value + off
        );
      }
      return absolute_info_tables;
    }
  };

  let __asterius_persistent_state = req.persistentState
      ? req.persistentState
      : {},
    __asterius_symbol_table = new SymbolTable(
      req.functionsOffsetTable,
      req.staticsOffsetTable,
      __asterius_table_base.value,
      __asterius_memory_base.value
    ),
    __asterius_spt_entries = mkSptEntries(req.sptOffsetEntries),
    __asterius_info_tables = mkInfoTable(req.offsetInfoTables),
    __asterius_reentrancy_guard = new ReentrancyGuard(["Scheduler", "GC"]),
    __asterius_fs = new FS(__asterius_components),
    __asterius_logger = new EventLogManager(),
    __asterius_tracer = new Tracer(__asterius_logger, __asterius_symbol_table),
    __asterius_memory = new Memory(__asterius_components),
    __asterius_memory_trap = new MemoryTrap(
      __asterius_logger,
      __asterius_symbol_table,
      __asterius_memory
    ),
    __asterius_heapalloc = new HeapAlloc(
      __asterius_components
    ),
    __asterius_jsval_manager = new JSValManager(__asterius_components),
    __asterius_stableptr_manager = new StablePtrManager(),
    __asterius_stablename_manager = new StableNameManager(
      __asterius_memory,
      __asterius_heapalloc,
      __asterius_symbol_table
    ),
    __asterius_staticptr_manager = new StaticPtrManager(
      __asterius_memory,
      __asterius_stableptr_manager,
      __asterius_spt_entries
    ),
    __asterius_scheduler = new Scheduler(
      __asterius_components,
      __asterius_memory,
      __asterius_symbol_table,
      __asterius_stableptr_manager
    ),
    __asterius_integer_manager = new IntegerManager(),
    __asterius_time_cbits = new TimeCBits(__asterius_memory, req.targetSpecificModule),
    __asterius_gc = new GC(
      __asterius_components,
      __asterius_memory,
      __asterius_heapalloc,
      __asterius_stableptr_manager,
      __asterius_stablename_manager,
      __asterius_scheduler,
      __asterius_info_tables,
      __asterius_symbol_table,
      __asterius_reentrancy_guard,
      req.yolo,
      req.gcThreshold
    ),
    __asterius_float_cbits = new FloatCBits(__asterius_memory),
    __asterius_messages = new Messages(__asterius_memory, __asterius_fs),
    __asterius_unicode = new Unicode(),
    __asterius_exports = new Exports(
      __asterius_components,
      __asterius_memory,
      __asterius_reentrancy_guard,
      __asterius_symbol_table,
      __asterius_scheduler,
      __asterius_stableptr_manager
    ),
    __asterius_exception_helper = new ExceptionHelper(
      __asterius_memory,
      __asterius_heapalloc,
      __asterius_exports,
      __asterius_info_tables,
      __asterius_symbol_table
    );
  const __asterius_wasi = new WASI(req.progName);
  __asterius_scheduler.exports = __asterius_exports;

  __asterius_components.memory = __asterius_memory;
  __asterius_components.exports = __asterius_exports;
  __asterius_components.heapAlloc = __asterius_heapalloc;
  __asterius_components.symbolTable = __asterius_symbol_table;
  __asterius_components.jsvalManager = __asterius_jsval_manager;

  function __asterius_show_I64(x) {
    return `0x${x.toString(16).padStart(8, "0")}`;
  }

  const __asterius_jsffi_instance = {
    exposeMemory: (p, len, t = Uint8Array) => __asterius_memory.expose(p, len, t),
    newJSValzh: v => __asterius_components.jsvalManager.newJSValzh(v),
    getJSValzh: i => __asterius_components.jsvalManager.getJSValzh(i),
    freeJSValzh: i => __asterius_components.jsvalManager.freeJSValzh(i),
    fs: __asterius_fs,
    stdio: {
      stdout: () => __asterius_fs.history(1),
      stderr: () => __asterius_fs.history(2)
    },
    returnFFIPromise: (promise) =>
      __asterius_scheduler.returnFFIPromise(promise)
  };


  const importObject = Object.assign(
    req.jsffiFactory(__asterius_jsffi_instance),
    {
      wasi_snapshot_preview1: __asterius_wasi.wasiImport,
      env: {
        __memory_base: __asterius_memory_base,
        __table_base: __asterius_table_base
      },
      rts: {
        printI64: x => __asterius_fs.writeNonMemory(1, `${__asterius_show_I64(x)}\n`),
        assertEqI64: function(x, y) {
          if (x != y) {
            throw new WebAssembly.RuntimeError(`unequal I64: ${x}, ${y}`);
          }
        },
        print: x => __asterius_fs.writeNonMemory(1, `${x}\n`)
      },
      fs: {
        read: (fd, buf, count) => __asterius_fs.read(fd, buf, count),
        write: (fd, buf, count) => __asterius_fs.write(fd, buf, count)
      },
      posix: modulify(new (req.targetSpecificModule.posix)(__asterius_memory, rtsConstants)),
      time: modulify(__asterius_time_cbits),
      // cannot name this float since float is a keyword.
      floatCBits: modulify(__asterius_float_cbits),
      GC: modulify(__asterius_gc),
      ExceptionHelper: modulify(__asterius_exception_helper),
      HeapAlloc: modulify(__asterius_heapalloc),
      Integer: modulify(__asterius_integer_manager),
      Memory: modulify(__asterius_memory),
      MemoryTrap: modulify(__asterius_memory_trap),
      Messages: modulify(__asterius_messages),
      StablePtr: modulify(__asterius_stableptr_manager),
      StableName: modulify(__asterius_stablename_manager),
      StaticPtr: modulify(__asterius_staticptr_manager),
      Unicode: modulify(__asterius_unicode),
      Tracing: modulify(__asterius_tracer),
      Exports: {
        newHaskellCallback: (sp, arg_tag, ret_tag, io, oneshot) => {
          let sn = [];
          let cb = __asterius_exports.newHaskellCallback(
            sp,
            arg_tag,
            ret_tag,
            io,
            oneshot
              ? () => __asterius_exports.freeHaskellCallback(sn[0])
              : () => {}
          );
          sn[0] = __asterius_components.jsvalManager.newJSValzh(cb);
          return sn[0];
        },
        freeHaskellCallback: sn => __asterius_exports.freeHaskellCallback(sn)
      },
      Scheduler: modulify(__asterius_scheduler)
    }
  );

  return WebAssembly.instantiate(req.module, importObject).then(i => {
    __asterius_wasi.initialize(i);

    Object.assign(__asterius_exports, i.exports);

    __asterius_memory.init(i.exports.memory);
    __asterius_heapalloc.init();
    __asterius_scheduler.setGC(__asterius_gc);

    for (const [f, off, a, r, i] of req.exportsStaticOffsets) {
      __asterius_exports[
        f
      ] = __asterius_exports.newHaskellCallback(
        __asterius_stableptr_manager.newStablePtr(
         __asterius_memory_base.value + off
        ),
        a,
        r,
        i,
        () => {}
      );
    }

    __asterius_exports.hs_init();

    return Object.assign(__asterius_jsffi_instance, {
      exports: __asterius_exports,
      symbolTable: __asterius_symbol_table,
      persistentState: __asterius_persistent_state
    });
  });
}
