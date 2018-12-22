import {module} from "./teletype.wasm.mjs";
import * as teletype from "./teletype.lib.mjs";

process.on("unhandledRejection", err => { throw err; });

module.then(m => teletype.newInstance(m)).then(i => {
    i.wasmInstance.exports.hs_init();
    i.wasmInstance.exports.main();
    console.log(i.stdio.stdout());
    console.log(i.stdio.stderr());
});
