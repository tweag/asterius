'use strict';

async function newAsteriusInstance(req) {
  let __asterius_wasm_instance = null;
  const __asterius_func_syms = req.functionSymbols;
  function __asterius_newI64(lo, hi) {
    return BigInt(lo) | (BigInt(hi) << 32n);
  }
  let __asterius_jsffi_JSRefs = [undefined];
  function __asterius_jsffi_newJSRef(e) {
    const n = __asterius_jsffi_JSRefs.length;
    __asterius_jsffi_JSRefs[n] = e;
    return n;
  }
  let __asterius_SPT = [undefined];
  function __asterius_newStablePtr(obj) {
    const n = __asterius_SPT.length;
    __asterius_SPT[n] = obj;
    return n;
  }
  function __asterius_deRefStablePtr(sp) {
    return __asterius_SPT[sp];
  }
  function __asterius_freeStablePtr(sp) {
    delete __asterius_SPT[sp];
  }
  const resultObject = await WebAssembly.instantiate(
      req.bufferSource,
      Object.assign(
          req.jsffiFactory({
            JSRefs: __asterius_jsffi_JSRefs,
            newJSRef: __asterius_jsffi_newJSRef
          }),
          {
            Math: Math,
            rts: {
              newStablePtr: __asterius_newStablePtr,
              deRefStablePtr: __asterius_deRefStablePtr,
              freeStablePtr: __asterius_freeStablePtr,
              printI64: (lo, hi) => console.log(__asterius_newI64(lo, hi)),
              print: console.log,
              panic: e => console.error(
                  '[ERROR] ' +
                  [
                    'errGCEnter1', 'errGCFun', 'errBarf', 'errStgGC',
                    'errUnreachableBlock', 'errHeapOverflow',
                    'errMegaBlockGroup', 'errUnimplemented', 'errAtomics',
                    'errSetBaseReg', 'errBrokenFunction', 'errAssert',
                    'errSchedulerReenteredFromHaskell', 'errIllegalSchedState',
                    'errIllegalPrevWhatNext', 'errIllegalThreadReturnCode'
                  ][e - 1]),
              __asterius_current_memory: p => {
                console.log('[INFO] Current Memory Pages: ' + p);
                return p;
              },
              __asterius_grow_memory: (p0, dp) => {
                console.log(
                    '[INFO] Previous Memory Pages: ' + p0 +
                    ', Allocated Memory Pages: ' + dp);
                return p0;
              },
              __asterius_memory_trap_trigger: (p_lo, p_hi) => console.error(
                  '[ERROR] Uninitialized memory trapped at 0x' +
                  __asterius_newI64(p_lo, p_hi).toString(16).padStart(8, '0')),
              __asterius_load_i64: (p_lo, p_hi, v_lo, v_hi) => console.log(
                  '[INFO] Loading i64 at 0x' +
                  __asterius_newI64(p_lo, p_hi).toString(16).padStart(8, '0') +
                  ', value: 0x' +
                  __asterius_newI64(v_lo, v_hi).toString(16).padStart(8, '0')),
              __asterius_store_i64: (p_lo, p_hi, v_lo, v_hi) => console.log(
                  '[INFO] Storing i64 at 0x' +
                  __asterius_newI64(p_lo, p_hi).toString(16).padStart(8, '0') +
                  ', value: 0x' +
                  __asterius_newI64(v_lo, v_hi).toString(16).padStart(8, '0')),
              __asterius_load_i8: (p_lo, p_hi, v) => console.log(
                  '[INFO] Loading i8 at 0x' +
                  __asterius_newI64(p_lo, p_hi).toString(16).padStart(8, '0') +
                  ', value: ' + v),
              __asterius_store_i8: (p_lo, p_hi, v) => console.log(
                  '[INFO] Storing i8 at 0x' +
                  __asterius_newI64(p_lo, p_hi).toString(16).padStart(8, '0') +
                  ', value: ' + v),
              __asterius_load_i16: (p_lo, p_hi, v) => console.log(
                  '[INFO] Loading i16 at 0x' +
                  __asterius_newI64(p_lo, p_hi).toString(16).padStart(8, '0') +
                  ', value: ' + v),
              __asterius_store_i16: (p_lo, p_hi, v) => console.log(
                  '[INFO] Storing i16 at 0x' +
                  __asterius_newI64(p_lo, p_hi).toString(16).padStart(8, '0') +
                  ', value: ' + v),
              __asterius_load_i32: (p_lo, p_hi, v) => console.log(
                  '[INFO] Loading i32 at 0x' +
                  __asterius_newI64(p_lo, p_hi).toString(16).padStart(8, '0') +
                  ', value: ' + v),
              __asterius_store_i32: (p_lo, p_hi, v) => console.log(
                  '[INFO] Storing i32 at 0x' +
                  __asterius_newI64(p_lo, p_hi).toString(16).padStart(8, '0') +
                  ', value: ' + v),
              __asterius_load_f32: (p_lo, p_hi, v) => console.log(
                  '[INFO] Loading f32 at 0x' +
                  __asterius_newI64(p_lo, p_hi).toString(16).padStart(8, '0') +
                  ', value: ' + v),
              __asterius_store_f32: (p_lo, p_hi, v) => console.log(
                  '[INFO] Storing f32 at 0x' +
                  __asterius_newI64(p_lo, p_hi).toString(16).padStart(8, '0') +
                  ', value: ' + v),
              __asterius_load_f64: (p_lo, p_hi, v) => console.log(
                  '[INFO] Loading f64 at 0x' +
                  __asterius_newI64(p_lo, p_hi).toString(16).padStart(8, '0') +
                  ', value: ' + v),
              __asterius_store_f64: (p_lo, p_hi, v) => console.log(
                  '[INFO] Storing f64 at 0x' +
                  __asterius_newI64(p_lo, p_hi).toString(16).padStart(8, '0') +
                  ', value: ' + v),
              __asterius_traceCmm: f => console.log(
                  '[INFO] Entering ' + __asterius_func_syms[f - 1] +
                  ', Sp: 0x' +
                  __asterius_wasm_instance.exports.__asterius_Load_Sp()
                      .toString(16)
                      .padStart(8, '0') +
                  ', SpLim: 0x' +
                  __asterius_wasm_instance.exports.__asterius_Load_SpLim()
                      .toString(16)
                      .padStart(8, '0') +
                  ', Hp: 0x' +
                  __asterius_wasm_instance.exports.__asterius_Load_Hp()
                      .toString(16)
                      .padStart(8, '0') +
                  ', HpLim: 0x' +
                  __asterius_wasm_instance.exports.__asterius_Load_HpLim()
                      .toString(16)
                      .padStart(8, '0')),
              __asterius_traceCmmBlock: (f, lbl) => console.log(
                  '[INFO] Branching to ' + __asterius_func_syms[f - 1] +
                  ' basic block ' + lbl + ', Sp: 0x' +
                  __asterius_wasm_instance.exports.__asterius_Load_Sp()
                      .toString(16)
                      .padStart(8, '0') +
                  ', SpLim: 0x' +
                  __asterius_wasm_instance.exports.__asterius_Load_SpLim()
                      .toString(16)
                      .padStart(8, '0') +
                  ', Hp: 0x' +
                  __asterius_wasm_instance.exports.__asterius_Load_Hp()
                      .toString(16)
                      .padStart(8, '0') +
                  ', HpLim: 0x' +
                  __asterius_wasm_instance.exports.__asterius_Load_HpLim()
                      .toString(16)
                      .padStart(8, '0')),
              __asterius_traceCmmSetLocal: (f, i, lo, hi) => console.log(
                  '[INFO] In ' + __asterius_func_syms[f - 1] +
                  ', Setting local register ' + i + ' to 0x' +
                  __asterius_newI64(lo, hi).toString(16).padStart(8, '0'))
            }
          }));
  __asterius_wasm_instance = resultObject.instance;
  return {
    wasmModule: resultObject.module,
    wasmInstance: resultObject.instance,
    staticsSymbolMap: req.staticsSymbolMap,
    functionSymbolMap: req.functionSymbolMap
  };
}
