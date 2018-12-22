import * as cloudflare from "./cloudflare.lib.mjs";

let i = cloudflare.newInstance(m);
i.wasmInstance.exports.hs_init();
i.wasmInstance.exports.main();
