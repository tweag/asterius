import * as rts from "./rts.mjs";
import module from "./jsffi.wasm.mjs";
import jsffi from "./jsffi.req.mjs";

process.on("unhandledRejection", err => {
  throw err;
});

module
  .then(m => rts.newAsteriusInstance(Object.assign(jsffi, { module: m })))
  .then(async i => {
    await i.exports.main();
    console.log(await i.exports.mult_hs_int(9, 9));
    console.log(await i.exports.mult_hs_double(9, 9));
    await i.exports.putchar("H".codePointAt(0));
  });
