"use strict";

async function newAsteriusInstance(req) {
  let __asterius_debug_log_enabled = true;
  function __asterius_debug_log_info(msg) {
    if (__asterius_debug_log_enabled) console.log("[INFO] " + msg);
  }
  let __asterius_wasm_instance = null;
  function __asterius_show_I(x) {
    return x.toString(16).padStart(8, "0");
  }
  function __asterius_show_I64(lo, hi) {
    return (
      "0x" +
      (__asterius_show_I(hi) + __asterius_show_I(lo))
        .replace(/^0+/, "")
        .padStart(8, "0")
    );
  }
  function __asterius_make_symbol_lookup_table(sym_map) {
    let tbl = {};
    for (const [k, v] of Object.entries(sym_map)) tbl[v & 0xffffffff] = k;
    return tbl;
  }
  const __asterius_statics_lookup_table = __asterius_make_symbol_lookup_table(
      req.staticsSymbolMap
    ),
    __asterius_function_lookup_table = __asterius_make_symbol_lookup_table(
      req.functionSymbolMap
    );
  function __asterius_show_func_sym(x) {
    return __asterius_function_lookup_table[x & 0xffffffff];
  }
  function __asterius_show_I64_with_sym(lo, hi) {
    switch (hi) {
      case 2097143:
        return (
          __asterius_show_I64(lo, hi) +
          (__asterius_statics_lookup_table[lo]
            ? "(" + __asterius_statics_lookup_table[lo] + ")"
            : "")
        );
      case 2097133:
        return (
          __asterius_show_I64(lo, hi) +
          (__asterius_function_lookup_table[lo]
            ? "(" + __asterius_function_lookup_table[lo] + ")"
            : "")
        );
      default:
        return __asterius_show_I64(lo, hi);
    }
  }
  const __asterius_jsffi_JSRefs = [undefined];
  function __asterius_jsffi_newJSRef(e) {
    return __asterius_jsffi_JSRefs.push(e) - 1;
  }
  const __asterius_SPT = [undefined];
  function __asterius_newStablePtr(obj) {
    return __asterius_SPT.push(obj) - 1;
  }
  function __asterius_deRefStablePtr(sp) {
    return __asterius_SPT[sp];
  }
  function __asterius_freeStablePtr(sp) {
    delete __asterius_SPT[sp];
  }
  function __asterius_memory_trap(p_lo, p_hi) {
    if (p_hi !== 2097143) {
      throw new WebAssembly.RuntimeError(
        "[ERROR] Memory trap caught invalid memory access at " +
          __asterius_show_I64(p_lo, p_hi)
      );
    }
  }
  const importObject = Object.assign(
    req.jsffiFactory({
      JSRefs: __asterius_jsffi_JSRefs,
      newJSRef: __asterius_jsffi_newJSRef,
      makeHaskellCallback: s => () => {
        const cap = req.staticsSymbolMap.MainCapability,
          export_funcs = __asterius_wasm_instance.exports;
        export_funcs.rts_evalIO(cap, __asterius_deRefStablePtr(s), 0);
      },
      makeHaskellCallback1: s => ev => {
        const cap = req.staticsSymbolMap.MainCapability,
          export_funcs = __asterius_wasm_instance.exports;
        export_funcs.rts_evalIO(
          cap,
          export_funcs.rts_apply(
            cap,
            __asterius_deRefStablePtr(s),
            export_funcs.rts_mkInt(cap, __asterius_jsffi_newJSRef(ev))
          ),
          0
        );
      }
    }),
    {
      Math: Math,
      rts: {
        newStablePtr: __asterius_newStablePtr,
        deRefStablePtr: __asterius_deRefStablePtr,
        freeStablePtr: __asterius_freeStablePtr,
        printI64: (lo, hi) => console.log(__asterius_show_I64(lo, hi)),
        printI64_with_sym: (lo, hi) =>
          console.log("[INFO] " + __asterius_show_I64_with_sym(lo, hi)),
        print: console.log,
        panic: e => console.error("[ERROR] " + req.errorMessages[e]),
        __asterius_current_memory: p => {
          __asterius_debug_log_info("Current Memory Pages: " + p);
          return p;
        },
        __asterius_debug_log_is_enabled: () => __asterius_debug_log_enabled,
        __asterius_debug_log_set_enabled: f => {
          __asterius_debug_log_enabled = Boolean(f);
        },
        __asterius_grow_memory: (p0, dp) => {
          __asterius_debug_log_info(
            "Previous Memory Pages: " + p0 + ", Allocated Memory Pages: " + dp
          );
          return p0;
        },
        __asterius_load_i64: (p_lo, p_hi, o, v_lo, v_hi) => {
          __asterius_memory_trap(p_lo, p_hi);
          __asterius_debug_log_info(
            "Loading i64 at " +
              __asterius_show_I64_with_sym(p_lo, p_hi) +
              "+" +
              o +
              ", value: 0x" +
              __asterius_show_I64_with_sym(v_lo, v_hi)
          );
        },
        __asterius_store_i64: (p_lo, p_hi, o, v_lo, v_hi) => {
          __asterius_memory_trap(p_lo, p_hi);
          __asterius_debug_log_info(
            "Storing i64 at " +
              __asterius_show_I64_with_sym(p_lo, p_hi) +
              "+" +
              o +
              ", value: 0x" +
              __asterius_show_I64_with_sym(v_lo, v_hi)
          );
        },
        __asterius_load_i8: (p_lo, p_hi, o, v) => {
          __asterius_memory_trap(p_lo, p_hi);
          __asterius_debug_log_info(
            "Loading i8 at " +
              __asterius_show_I64_with_sym(p_lo, p_hi) +
              "+" +
              o +
              ", value: " +
              v
          );
        },
        __asterius_store_i8: (p_lo, p_hi, o, v) => {
          __asterius_memory_trap(p_lo, p_hi);
          __asterius_debug_log_info(
            "Storing i8 at " +
              __asterius_show_I64_with_sym(p_lo, p_hi) +
              "+" +
              o +
              ", value: " +
              v
          );
        },
        __asterius_load_i16: (p_lo, p_hi, o, v) => {
          __asterius_memory_trap(p_lo, p_hi);
          __asterius_debug_log_info(
            "Loading i16 at " +
              __asterius_show_I64_with_sym(p_lo, p_hi) +
              "+" +
              o +
              ", value: " +
              v
          );
        },
        __asterius_store_i16: (p_lo, p_hi, o, v) => {
          __asterius_memory_trap(p_lo, p_hi);
          __asterius_debug_log_info(
            "Storing i16 at " +
              __asterius_show_I64_with_sym(p_lo, p_hi) +
              "+" +
              o +
              ", value: " +
              v
          );
        },
        __asterius_load_i32: (p_lo, p_hi, o, v) => {
          __asterius_memory_trap(p_lo, p_hi);
          __asterius_debug_log_info(
            "Loading i32 at " +
              __asterius_show_I64_with_sym(p_lo, p_hi) +
              "+" +
              o +
              ", value: " +
              v
          );
        },
        __asterius_store_i32: (p_lo, p_hi, o, v) => {
          __asterius_memory_trap(p_lo, p_hi);
          __asterius_debug_log_info(
            "Storing i32 at " +
              __asterius_show_I64_with_sym(p_lo, p_hi) +
              "+" +
              o +
              ", value: " +
              v
          );
        },
        __asterius_load_f32: (p_lo, p_hi, o, v) => {
          __asterius_memory_trap(p_lo, p_hi);
          __asterius_debug_log_info(
            "Loading f32 at " +
              __asterius_show_I64_with_sym(p_lo, p_hi) +
              "+" +
              o +
              ", value: " +
              v
          );
        },
        __asterius_store_f32: (p_lo, p_hi, o, v) => {
          __asterius_memory_trap(p_lo, p_hi);
          __asterius_debug_log_info(
            "Storing f32 at " +
              __asterius_show_I64_with_sym(p_lo, p_hi) +
              "+" +
              o +
              ", value: " +
              v
          );
        },
        __asterius_load_f64: (p_lo, p_hi, o, v) => {
          __asterius_memory_trap(p_lo, p_hi);
          __asterius_debug_log_info(
            "Loading f64 at " +
              __asterius_show_I64_with_sym(p_lo, p_hi) +
              "+" +
              o +
              ", value: " +
              v
          );
        },
        __asterius_store_f64: (p_lo, p_hi, o, v) => {
          __asterius_memory_trap(p_lo, p_hi);
          __asterius_debug_log_info(
            "Storing f64 at " +
              __asterius_show_I64_with_sym(p_lo, p_hi) +
              "+" +
              o +
              ", value: " +
              v
          );
        },
        __asterius_traceCmm: f =>
          __asterius_debug_log_info(
            "Entering " +
              __asterius_show_func_sym(f) +
              ", Sp: 0x" +
              __asterius_show_I(
                __asterius_wasm_instance.exports.__asterius_Load_Sp()
              ) +
              ", SpLim: 0x" +
              __asterius_show_I(
                __asterius_wasm_instance.exports.__asterius_Load_SpLim()
              ) +
              ", Hp: 0x" +
              __asterius_show_I(
                __asterius_wasm_instance.exports.__asterius_Load_Hp()
              ) +
              ", HpLim: 0x" +
              __asterius_show_I(
                __asterius_wasm_instance.exports.__asterius_Load_HpLim()
              )
          ),
        __asterius_traceCmmBlock: (f, lbl) =>
          __asterius_debug_log_info(
            "Branching to " +
              __asterius_show_func_sym(f) +
              " basic block " +
              lbl +
              ", Sp: 0x" +
              __asterius_show_I(
                __asterius_wasm_instance.exports.__asterius_Load_Sp()
              ) +
              ", SpLim: 0x" +
              __asterius_show_I(
                __asterius_wasm_instance.exports.__asterius_Load_SpLim()
              ) +
              ", Hp: 0x" +
              __asterius_show_I(
                __asterius_wasm_instance.exports.__asterius_Load_Hp()
              ) +
              ", HpLim: 0x" +
              __asterius_show_I(
                __asterius_wasm_instance.exports.__asterius_Load_HpLim()
              )
          ),
        __asterius_traceCmmSetLocal: (f, i, lo, hi) =>
          __asterius_debug_log_info(
            "In " +
              __asterius_show_func_sym(f) +
              ", Setting local register " +
              i +
              " to " +
              __asterius_show_I64_with_sym(lo, hi)
          )
      }
    }
  );
  const resultObject = await (WebAssembly.instantiateStreaming
    ? WebAssembly.instantiateStreaming(req.bufferSource, importObject)
    : WebAssembly.instantiate(req.bufferSource, importObject));
  __asterius_wasm_instance = resultObject.instance;
  return {
    wasmModule: resultObject.module,
    wasmInstance: resultObject.instance,
    staticsSymbolMap: req.staticsSymbolMap,
    functionSymbolMap: req.functionSymbolMap,
    __asterius_jsffi_JSRefs: __asterius_jsffi_JSRefs,
    __asterius_SPT: __asterius_SPT
  };
}
