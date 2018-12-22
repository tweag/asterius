import {module} from "./bytearray.wasm.mjs";
import * as bytearray from "./bytearray.lib.mjs";

process.on("unhandledRejection", err => { throw err; });

module.then(m => bytearray.newInstance(m)).then(i => {
    i.wasmInstance.exports.hs_init();
    i.wasmInstance.exports.main();
    console.log(i.stdio.stdout());
    console.log(i.stdio.stderr());
});
