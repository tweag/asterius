import module from "./jsffi.wasm.mjs";
import jsffi from "./jsffi.lib.mjs";

process.on("unhandledRejection", err => { throw err; });

module.then(m => jsffi(m)).then(i => {
    i.exports.hs_init();
    i.exports.main();
    console.log(i.exports.mult_hs_int(9, 9));
    console.log(i.exports.mult_hs_double(9, 9));
    i.exports.putchar("H".codePointAt(0));
});
