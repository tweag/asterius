import module from "./jsffi.wasm.mjs";
import jsffi from "./jsffi.lib.mjs";

process.on("unhandledRejection", err => { throw err; });

module.then(m => jsffi(m)).then(async i => {
    i.exports.hs_init();
    await i.exports.main();
    console.log(await i.exports.mult_hs_int(9, 9));
    console.log(await i.exports.mult_hs_double(9, 9));
    await i.exports.putchar("H".codePointAt(0));
});
