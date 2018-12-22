import {module} from "./rtsapi.wasm.mjs";
import * as rtsapi from "./rtsapi.lib.mjs";

process.on("unhandledRejection", err => { throw err; });

module.then(m => rtsapi.newInstance(m)).then(i => {
    i.wasmInstance.exports.hs_init();
    i.wasmInstance.exports.main();
    i.wasmInstance.exports.rts_evalLazyIO(i.wasmInstance.exports.rts_apply(i.symbolTable.Main_printInt_closure, i.wasmInstance.exports.rts_apply(i.symbolTable.Main_fact_closure, i.wasmInstance.exports.rts_mkInt(5))));
    const tid_p1 = i.wasmInstance.exports.rts_eval(i.wasmInstance.exports.rts_apply(i.symbolTable.Main_fact_closure, i.wasmInstance.exports.rts_mkInt(5)));
    console.log(i.wasmInstance.exports.rts_getInt(i.wasmInstance.exports.getTSOret(tid_p1)));
    const tid_p2 = i.wasmInstance.exports.rts_evalStableIO(i.wasmInstance.exports.getStablePtr(i.wasmInstance.exports.rts_apply(i.symbolTable.Main_printInt_closure, i.wasmInstance.exports.rts_apply(i.symbolTable.Main_fact_closure, i.wasmInstance.exports.rts_mkInt(5)))));
    console.log((i.wasmInstance.exports.deRefStablePtr(i.wasmInstance.exports.getTSOret(tid_p2)) & (~ 7)) === i.symbolTable.ghczmprim_GHCziTuple_Z0T_closure);
    console.log(i.wasmInstance.exports.rts_getBool(i.symbolTable.ghczmprim_GHCziTypes_False_closure));
    console.log(i.wasmInstance.exports.rts_getBool(i.symbolTable.ghczmprim_GHCziTypes_True_closure));
    console.log(i.wasmInstance.exports.rts_getBool(i.wasmInstance.exports.rts_mkBool(0)));
    console.log(i.wasmInstance.exports.rts_getBool(i.wasmInstance.exports.rts_mkBool(42)));
    const x0 = Math.random();
    const tid_p3 = i.wasmInstance.exports.rts_eval(i.wasmInstance.exports.rts_apply(i.symbolTable.base_GHCziBase_id_closure, i.wasmInstance.exports.rts_mkDouble(x0)));
    const x1 = i.wasmInstance.exports.rts_getDouble(i.wasmInstance.exports.getTSOret(tid_p3));
    console.log([x0, x1, x0 === x1]);
});
