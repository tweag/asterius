i => {
  const cap = i.staticsSymbolMap.MainCapability;
  i.wasmInstance.exports.hs_init();
  const ret = i.wasmInstance.exports.allocate(cap, 1);
  i.wasmInstance.exports.rts_evalIO(
      cap, i.staticsSymbolMap.Main_initModel_closure, ret);
  let hs_state = i.wasmInstance.exports.loadI64(ret);
  console.log(hs_state);
  i.wasmInstance.exports.rts_evalIO(
      cap, i.staticsSymbolMap.Main_main_closure, 0);

  const mealy = i.staticsSymbolMap.mealy;
  window.addEventListener('popstate', ev => {
    const r = i.wasmInstance.exports.rts_apply(
        cap, i.wasmInstance.exports.rts_apply(cap, mealy, hs_state),
        i.wasmInstance.exports.getStablePtr(ev));
    i.wasmInstance.exports.rts_eval(cap, r, ret);
    const r_tup = i.wasmInstance.exports.loadI64(ret);
    hs_state = i.wasmInstance.exports.rts_apply(cap, base_DataziTuple_fst_closure, r_tup);
  });
}
