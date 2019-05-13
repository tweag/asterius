import cloudflare from "./cloudflare.lib.mjs";

let i = cloudflare(m);
i.wasmInstance.exports.hs_init();
i.wasmInstance.exports.main();
