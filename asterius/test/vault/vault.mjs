import {module} from "./vault.wasm.mjs";
import * as vault from "./vault.lib.mjs";

process.on("unhandledRejection", err => { throw err; });

module.then(m => vault.newInstance(m)).then(async i => {
    i.wasmInstance.exports.hs_init();
    // i.wasmInstance.exports.main();
    i.vault = new Map([['key', 'Vault value set from js']]);
    await new Promise(resolve => setTimeout(resolve, 1024));
    i.wasmInstance.exports.main();
    console.log(i.stdio.stdout());
});
