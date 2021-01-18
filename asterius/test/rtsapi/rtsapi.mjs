import * as rts from "./rts.mjs";
import module from "./rtsapi.wasm.mjs";
import rtsapi from "./rtsapi.req.mjs";

process.on("unhandledRejection", (err) => {
  throw err;
});

module
  .then((m) => rts.newAsteriusInstance(Object.assign(rtsapi, { module: m })))
  .then(async (i) => {
    await i.exports.main();
    await i.exports.rts_evalLazyIO(
      Number(
        i.exports.rts_apply(
          BigInt(i.symbolTable.addressOf("Main_printInt_closure")),
          i.exports.rts_apply(
            BigInt(i.symbolTable.addressOf("Main_fact_closure")),
            i.exports.rts_mkInt(5n)
          )
        )
      )
    );
    const tid_p1 = await i.exports.rts_evalIO(
      Number(
        i.exports.rts_apply(
          BigInt(
            i.symbolTable.addressOf(
              "base_AsteriusziTopHandler_runNonIO_closure"
            )
          ),
          i.exports.rts_apply(
            BigInt(i.symbolTable.addressOf("Main_fact_closure")),
            i.exports.rts_mkInt(5n)
          )
        )
      )
    );
    console.log(i.exports.rts_getInt(BigInt(i.exports.getTSOret(tid_p1))));
    console.log(i.exports.rts_getBool(BigInt(i.exports.rts_mkBool(0))));
    console.log(i.exports.rts_getBool(BigInt(i.exports.rts_mkBool(42))));
    const x0 = Math.random();
    const tid_p3 = await i.exports.rts_evalIO(
      Number(
        i.exports.rts_apply(
          BigInt(
            i.symbolTable.addressOf(
              "base_AsteriusziTopHandler_runNonIO_closure"
            )
          ),
          i.exports.rts_apply(
            BigInt(i.symbolTable.addressOf("base_GHCziBase_id_closure")),
            i.exports.rts_mkDouble(x0)
          )
        )
      )
    );
    const x1 = i.exports.rts_getDouble(BigInt(i.exports.getTSOret(tid_p3)));
    console.log([x0, x1, x0 === x1]);
  });
