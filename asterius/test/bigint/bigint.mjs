import {module} from "./bigint.wasm.mjs";
import * as bigint from "./bigint.lib.mjs";

process.on("unhandledRejection", err => { throw err; });

module.then(m => bigint.newInstance(m)).then(i => {
    i.wasmInstance.exports.hs_init();
    i.wasmInstance.exports.main();
    console.log(i.stdio.stdout());
    console.log(i.stdio.stderr());
});
