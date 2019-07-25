import { modulify } from "./rts.modulify.mjs";
import { encodeUTF8, decodeUTF8 } from "./rts.utf8.mjs";
import { encodeUTF16, decodeUTF16 } from "./rts.utf16.mjs";
import { encodeUTF32, decodeUTF32 } from "./rts.utf32.mjs";
import { encodeLatin1, decodeLatin1 } from "./rts.latin1.mjs";
import { ReentrancyGuard } from "./rts.reentrancy.mjs";
import { EventLogManager } from "./rts.eventlog.mjs";
import { Tracer } from "./rts.tracing.mjs";
import { Memory } from "./rts.memory.mjs";
import { MemoryTrap } from "./rts.memorytrap.mjs";
import { MBlockAlloc } from "./rts.mblockalloc.mjs";
import { HeapAlloc } from "./rts.heapalloc.mjs";
import { StablePtrManager } from "./rts.stableptr.mjs";
import { StableNameManager } from "./rts.stablename.mjs";
import { TSOManager } from "./rts.tso.mjs";
import { HeapBuilder } from "./rts.heapbuilder.mjs";
import { IntegerManager } from "./rts.integer.mjs";
import { MemoryFileSystem } from "./rts.fs.mjs";
import { ByteStringCBits } from "./rts.bytestring.mjs";
import { GC } from "./rts.gc.mjs";
import { ExceptionHelper } from "./rts.exception.mjs";
import { Messages } from "./rts.messages.mjs";
import { ThreadPaused } from "./rts.threadpaused.mjs";
import { MD5 } from "./rts.md5.mjs"
import { FloatCBits } from "./rts.float.mjs";
import { Unicode } from "./rts.unicode.mjs";
import { Exports } from "./rts.exports.mjs";
import * as rtsConstants from "./rts.constants.mjs";

export function newAsteriusInstance(req) {
  let __asterius_reentrancy_guard = new ReentrancyGuard(["Scheduler", "GC"]),
    __asterius_logger = new EventLogManager(req.symbolTable),
    __asterius_tracer = new Tracer(__asterius_logger, req.symbolTable),
    __asterius_wasm_instance = null,
    __asterius_wasm_table = new WebAssembly.Table({element: "anyfunc", initial: req.tableSlots}),
    __asterius_wasm_memory = new WebAssembly.Memory({initial: req.staticMBlocks * (rtsConstants.mblock_size / 65536)}),
    __asterius_memory = new Memory(),
    __asterius_mblockalloc = new MBlockAlloc(),
    __asterius_memory_trap = new MemoryTrap(__asterius_logger, req.symbolTable, __asterius_memory, __asterius_mblockalloc),
    __asterius_heapalloc = new HeapAlloc(__asterius_memory, __asterius_mblockalloc),
    __asterius_stableptr_manager = new StablePtrManager(),
    __asterius_stablename_manager = new StableNameManager(__asterius_memory, __asterius_heapalloc, req.symbolTable),
    __asterius_tso_manager = new TSOManager(__asterius_memory, req.symbolTable, __asterius_stableptr_manager),
    __asterius_heap_builder = new HeapBuilder(req.symbolTable, __asterius_heapalloc, __asterius_memory, __asterius_stableptr_manager),
    __asterius_integer_manager = new IntegerManager(__asterius_stableptr_manager, __asterius_heap_builder),
    __asterius_fs = new MemoryFileSystem(__asterius_logger),
    __asterius_bytestring_cbits = new ByteStringCBits(null),
    __asterius_gc = new GC(__asterius_memory, __asterius_mblockalloc, __asterius_heapalloc, __asterius_stableptr_manager, __asterius_stablename_manager, __asterius_tso_manager, req.infoTables, req.pinnedStaticClosures, req.symbolTable, __asterius_reentrancy_guard, req.yolo),
    __asterius_exception_helper = new ExceptionHelper(__asterius_memory, __asterius_heapalloc, req.infoTables, req.symbolTable),
    __asterius_threadpaused = new ThreadPaused(__asterius_memory, req.infoTables, req.symbolTable),
    __asterius_float_cbits = new FloatCBits(__asterius_memory),
    __asterius_messages = new Messages(__asterius_memory, __asterius_fs),
    __asterius_unicode = new Unicode(),
    __asterius_exports = new Exports(__asterius_memory, __asterius_reentrancy_guard, req.symbolTable, __asterius_tso_manager, req.exports, __asterius_stableptr_manager),
    __asterius_md5 = new MD5(__asterius_memory);
  __asterius_tso_manager.exports = __asterius_exports;

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
    makeHaskellCallback: sp => async () => {
      const tid = await __asterius_exports.rts_evalLazyIO(__asterius_stableptr_manager.deRefStablePtr(sp));
      __asterius_exports.rts_checkSchedStatus(tid);
    },
    makeHaskellCallback1: sp => async ev => {
      const tid = await __asterius_exports.rts_evalLazyIO(
        __asterius_exports.rts_apply(
          __asterius_stableptr_manager.deRefStablePtr(sp),
          __asterius_exports.rts_mkStablePtr(
            __asterius_stableptr_manager.newJSVal(ev)
          )
        )
      );
      __asterius_exports.rts_checkSchedStatus(tid);
    },
    makeHaskellCallback2: sp => async (x, y) => {
      const tid = await __asterius_exports.rts_evalLazyIO(
        __asterius_exports.rts_apply(
          __asterius_exports.rts_apply(
            __asterius_stableptr_manager.deRefStablePtr(sp), __asterius_exports.rts_mkStablePtr(
              __asterius_stableptr_manager.newJSVal(x))),
          __asterius_exports.rts_mkStablePtr(
            __asterius_stableptr_manager.newJSVal(y))));
      __asterius_exports.rts_checkSchedStatus(tid);
    },
    Integer: __asterius_integer_manager,
    FloatCBits: __asterius_float_cbits,
    stdio: {
      putChar: (h, c) => __asterius_fs.writeSync(h, String.fromCodePoint(c)),
      stdout: () => __asterius_fs.root.get("/dev/stdout"),
      stderr: () => __asterius_fs.root.get("/dev/stderr")
    },
    setPromise: (vt, p) => __asterius_tso_manager.setPromise(vt, p)
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
        asinh: x => Math.asinh(x),
        acosh: x => Math.acosh(x),
        atanh: x => Math.atanh(x),
        log: x => Math.log(x),
        exp: x => Math.exp(x),
        pow: (x, y) => Math.pow(x, y)
      },
      WasmTable: {
        table: __asterius_wasm_table
      },
      WasmMemory: {
        memory: __asterius_wasm_memory
      },
      rts: {
        printI64: x => __asterius_fs.writeSync(__asterius_fs.stdout(), __asterius_show_I64(x) + "\n"),
        assertEqI64: function(x, y) { if(x != y) {   throw new WebAssembly.RuntimeError("unequal I64: " + x + ", " + y); } },
        print: x => __asterius_fs.writeSync(__asterius_fs.stdout(), x + "\n")
      },
      bytestring: modulify(__asterius_bytestring_cbits),
      // cannot name this float since float is a keyword.
      floatCBits: modulify(__asterius_float_cbits),
      ReentrancyGuard: modulify(__asterius_reentrancy_guard),
      GC: modulify(__asterius_gc),
      ExceptionHelper: modulify(__asterius_exception_helper),
      ThreadPaused: modulify(__asterius_threadpaused),
      HeapAlloc: modulify(__asterius_heapalloc),
      HeapBuilder: modulify(__asterius_heap_builder),
      Integer: modulify(__asterius_integer_manager),
      MBlockAlloc: modulify(__asterius_mblockalloc),
      Memory: modulify(__asterius_memory),
      MemoryTrap: modulify(__asterius_memory_trap),
      Messages: modulify(__asterius_messages),
      StablePtr: modulify(__asterius_stableptr_manager),
      StableName: modulify(__asterius_stablename_manager),
      Unicode: modulify(__asterius_unicode),
      MD5: modulify(__asterius_md5),
      Tracing: modulify(__asterius_tracer),
      TSO: modulify(__asterius_tso_manager)
    }
  );
  return WebAssembly.instantiate(req.module, importObject).then(i => {
      __asterius_wasm_instance = i;
      __asterius_memory.init(__asterius_wasm_memory, req.staticMBlocks);
      __asterius_mblockalloc.init(__asterius_memory, req.staticMBlocks);
      __asterius_heapalloc.init();
      __asterius_integer_manager.heap = __asterius_heap_builder;
      __asterius_bytestring_cbits.memory = __asterius_memory;
      return Object.assign(__asterius_jsffi_instance, {
        wasmModule: req.module,
        wasmInstance: __asterius_wasm_instance,
        exports: Object.freeze(Object.assign(__asterius_exports, __asterius_wasm_instance.exports)),
        symbolTable: req.symbolTable,
        logger: __asterius_logger
      });
    });
}
