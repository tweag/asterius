import module from "./jsffi.wasm.mjs";
import * as jsffi from "./jsffi.lib.mjs";

process.on("unhandledRejection", err => { throw err; });

module.then(m => jsffi.newInstance(m)).then(i => {
    i.wasmInstance.exports.hs_init();
    i.wasmInstance.exports.main();
    console.log(i.wasmInstance.exports.mult_hs_int(9, 9));
    console.log(i.wasmInstance.exports.mult_hs_double(9, 9));
    i.wasmInstance.exports.putchar("H".codePointAt(0));
});
