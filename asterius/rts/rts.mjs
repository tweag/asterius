import { modulify } from "./rts.modulify.mjs";
import { encodeUTF8, decodeUTF8 } from "./rts.utf8.mjs";
import { encodeUTF16, decodeUTF16 } from "./rts.utf16.mjs";
import { encodeUTF32, decodeUTF32 } from "./rts.utf32.mjs";
import { encodeLatin1, decodeLatin1 } from "./rts.latin1.mjs";
import { EventLogManager } from "./rts.eventlog.mjs";
import { Tracer } from "./rts.tracing.mjs";
import { Memory } from "./rts.memory.mjs";
import { MemoryTrap } from "./rts.memorytrap.mjs";
import { MBlockAlloc } from "./rts.mblockalloc.mjs";
import { HeapAlloc } from "./rts.heapalloc.mjs";
import { StablePtrManager } from "./rts.stableptr.mjs";
import { TSOManager } from "./rts.tso.mjs";
import { HeapBuilder } from "./rts.heapbuilder.mjs";
import { IntegerManager } from "./rts.integer.mjs";
import { MemoryFileSystem } from "./rts.fs.mjs";
import { ByteStringCBits } from "./rts.bytestring.mjs";
import { SanCheck } from "./rts.sanity.mjs";
import { GC } from "./rts.gc.mjs";

export function newAsteriusInstance(req) {
  let __asterius_logger = new EventLogManager(req.symbolTable),
    __asterius_tracer = new Tracer(__asterius_logger, req.symbolTable),
    __asterius_wasm_instance = null,
    __asterius_memory = new Memory(),
    __asterius_memory_trap = new MemoryTrap(__asterius_logger, req.symbolTable),
    __asterius_mblockalloc = new MBlockAlloc(__asterius_memory),
    __asterius_heapalloc = new HeapAlloc(__asterius_memory, __asterius_mblockalloc),
    __asterius_stableptr_manager = new StablePtrManager(),
    __asterius_tso_manager = new TSOManager(),
    __asterius_heap_builder = new HeapBuilder(req.symbolTable, __asterius_heapalloc, __asterius_memory, __asterius_stableptr_manager),
    __asterius_integer_manager = new IntegerManager(__asterius_stableptr_manager, __asterius_heap_builder),
    __asterius_fs = new MemoryFileSystem(__asterius_logger),
    __asterius_vault = req.vault ? req.vault : new Map(),
    __asterius_bytestring_cbits = new ByteStringCBits(null),
    __asterius_sancheck = new SanCheck(__asterius_memory, __asterius_stableptr_manager, req.infoTables, req.pinnedStaticClosures),
    __asterius_gc = new GC(__asterius_memory, __asterius_heapalloc, __asterius_stableptr_manager, req.infoTables, req.pinnedStaticClosures);
  function __asterius_show_I64(x) {
    return "0x" + x.toString(16).padStart(8, "0");
  }
  const __asterius_jsffi_instance = {
    decodeUTF8: decodeUTF8,
    encodeUTF8: encodeUTF8,
    decodeLatin1: decodeLatin1,
    encodeLatin1: encodeLatin1,
    decodeUTF16LE: decodeUTF16,
    encodeUTF16LE: encodeUTF16,
    decodeUTF32LE: decodeUTF32,
    encodeUTF32LE: encodeUTF32,
    newJSVal: v => __asterius_stableptr_manager.newJSVal(v),
    getJSVal: i => __asterius_stableptr_manager.getJSVal(i),
    newTmpJSVal: v => __asterius_stableptr_manager.newTmpJSVal(v),
    mutTmpJSVal: (i, f) => __asterius_stableptr_manager.mutTmpJSVal(i, f),
    freezeTmpJSVal: i => __asterius_stableptr_manager.freezeTmpJSVal(i),
    vaultInsert: (k, v) =>
      __asterius_jsffi_instance.vault.set(decodeLatin1(k), v),
    vaultHas: k =>
      __asterius_jsffi_instance.vault.has(decodeLatin1(k)),
    vaultLookup: k =>
      __asterius_jsffi_instance.vault.get(decodeLatin1(k)),
    vaultDelete: k =>
      __asterius_jsffi_instance.vault.delete(decodeLatin1(k)),
    makeHaskellCallback: sp => () => {
      const tid = __asterius_wasm_instance.exports.rts_evalLazyIO(__asterius_stableptr_manager.deRefStablePtr(sp));
      __asterius_wasm_instance.exports.rts_checkSchedStatus(tid);
    },
    makeHaskellCallback1: sp => ev => {
      const tid = __asterius_wasm_instance.exports.rts_evalLazyIO(
        __asterius_wasm_instance.exports.rts_apply(
          __asterius_stableptr_manager.deRefStablePtr(sp),
          __asterius_wasm_instance.exports.rts_mkInt(
            __asterius_stableptr_manager.newJSVal(ev)
          )
        )
      );
      __asterius_wasm_instance.exports.rts_checkSchedStatus(tid);
    },
    makeHaskellCallback2: sp => (x, y) => {
      const tid = __asterius_wasm_instance.exports.rts_evalLazyIO(
        __asterius_wasm_instance.exports.rts_apply(
          __asterius_wasm_instance.exports.rts_apply(
            __asterius_stableptr_manager.deRefStablePtr(sp), __asterius_wasm_instance.exports.rts_mkInt(
              __asterius_stableptr_manager.newJSVal(x))),
          __asterius_wasm_instance.exports.rts_mkInt(
            __asterius_stableptr_manager.newJSVal(y))));
      __asterius_wasm_instance.exports.rts_checkSchedStatus(tid);
    },
    Integer: __asterius_integer_manager,
    stdio: {
      putChar: (h, c) => __asterius_fs.writeSync(h, String.fromCodePoint(c)),
      stdout: () => __asterius_fs.root.get("/dev/stdout"),
      stderr: () => __asterius_fs.root.get("/dev/stderr")
    }
  };
  const importObject = Object.assign(
    req.jsffiFactory(__asterius_jsffi_instance),
    {
      Math: {
        sin: x => Math.sin(x),
        cos: x => Math.cos(x),
        tan: x => Math.tan(x),
        sinh: x => Math.sinh(x),
        cosh: x => Math.cosh(x),
        tanh: x => Math.tanh(x),
        asin: x => Math.asin(x),
        acos: x => Math.acos(x),
        atan: x => Math.atan(x),
        log: x => Math.log(x),
        exp: x => Math.exp(x),
        pow: (x, y) => Math.pow(x, y)
      },
      rts: {
        printI64: x => __asterius_fs.writeSync(__asterius_fs.stdout(), __asterius_show_I64(x) + "\n"),
        print: x => __asterius_fs.writeSync(__asterius_fs.stdout(), x + "\n"),
        emitEvent: ev => __asterius_logger.logEvent(req.events[ev])
      },
      bytestring: modulify(__asterius_bytestring_cbits),
      GC: modulify(__asterius_gc),
      HeapAlloc: modulify(__asterius_heapalloc),
      HeapBuilder: modulify(__asterius_heap_builder),
      MBlockAlloc: modulify(__asterius_mblockalloc),
      Memory: modulify(__asterius_memory),
      MemoryTrap: modulify(__asterius_memory_trap),
      SanCheck: modulify(__asterius_sancheck),
      StablePtr: modulify(__asterius_stableptr_manager),
      Tracing: modulify(__asterius_tracer),
      TSO: modulify(__asterius_tso_manager)
    }
  );
  if (req.sync) {
    const i = new WebAssembly.Instance(req.module, importObject);
    __asterius_wasm_instance = i;
    __asterius_memory.init(__asterius_wasm_instance.exports.memory, req.staticMBlocks);
    __asterius_mblockalloc.init();
    __asterius_heapalloc.init();
    __asterius_integer_manager.heap = __asterius_heap_builder;
    __asterius_bytestring_cbits.memory = __asterius_memory;
    return Object.assign(__asterius_jsffi_instance, {
      wasmModule: req.module,
      wasmInstance: __asterius_wasm_instance,
      symbolTable: req.symbolTable,
      vault: __asterius_vault,
      logger: __asterius_logger
    });
  } else
    return WebAssembly.instantiate(req.module, importObject).then(i => {
      __asterius_wasm_instance = i;
      __asterius_memory.init(__asterius_wasm_instance.exports.memory, req.staticMBlocks);
      __asterius_mblockalloc.init();
      __asterius_heapalloc.init();
      __asterius_integer_manager.heap = __asterius_heap_builder;
      __asterius_bytestring_cbits.memory = __asterius_memory;
      return Object.assign(__asterius_jsffi_instance, {
        wasmModule: req.module,
        wasmInstance: __asterius_wasm_instance,
        symbolTable: req.symbolTable,
        vault: __asterius_vault,
        logger: __asterius_logger
      });
    });
}
