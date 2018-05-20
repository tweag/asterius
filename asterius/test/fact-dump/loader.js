const fs = require("fs");

async function runWasm(p) {
    const i = await WebAssembly.instantiate(fs.readFileSync(p), {rts: {print: console.log}});
    i.instance.exports.main();
}

runWasm("Fact.wasm");
