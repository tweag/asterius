import System.Environment
import System.Process

main :: IO ()
main = do
  args <- getArgs
  callProcess "ahc-link" $
    [ "--input"
    , "test/rtsapi/rtsapi.hs"
    , "--output-link-report"
    , "test/rtsapi/rtsapi.link.txt"
    , "--run"
    , "--extra-root-symbol=Main_printInt_closure"
    , "--extra-root-symbol=Main_fact_closure"
    , "--extra-root-symbol=base_GHCziBase_id_closure"
    ] <>
    [ mconcat
        [ "--asterius-instance-callback="
        , "i => {"
        , "const cap = i.staticsSymbolMap.MainCapability;"
        , "i.wasmInstance.exports.hs_init();"
        , "i.wasmInstance.exports.main();"
        , "i.wasmInstance.exports.rts_evalLazyIO(i.wasmInstance.exports.rts_apply(i.staticsSymbolMap.Main_printInt_closure, i.wasmInstance.exports.rts_apply(i.staticsSymbolMap.Main_fact_closure, i.wasmInstance.exports.rts_mkInt(5))));"
        , "const tid_p1 = i.wasmInstance.exports.rts_eval(i.wasmInstance.exports.rts_apply(i.staticsSymbolMap.Main_fact_closure, i.wasmInstance.exports.rts_mkInt(5)));"
        , "console.log(i.wasmInstance.exports.rts_getInt(i.wasmInstance.exports.getTSOret(tid_p1)));"
        , "const tid_p2 = i.wasmInstance.exports.rts_evalStableIO(i.wasmInstance.exports.getStablePtr(i.wasmInstance.exports.rts_apply(i.staticsSymbolMap.Main_printInt_closure, i.wasmInstance.exports.rts_apply(i.staticsSymbolMap.Main_fact_closure, i.wasmInstance.exports.rts_mkInt(5)))));"
        , "console.log((i.wasmInstance.exports.deRefStablePtr(i.wasmInstance.exports.getTSOret(tid_p2)) & (~ 7)) === i.staticsSymbolMap.ghczmprim_GHCziTuple_Z0T_closure);"
        , "console.log(i.wasmInstance.exports.rts_getBool(i.staticsSymbolMap.ghczmprim_GHCziTypes_False_closure));"
        , "console.log(i.wasmInstance.exports.rts_getBool(i.staticsSymbolMap.ghczmprim_GHCziTypes_True_closure));"
        , "console.log(i.wasmInstance.exports.rts_getBool(i.wasmInstance.exports.rts_mkBool(0)));"
        , "console.log(i.wasmInstance.exports.rts_getBool(i.wasmInstance.exports.rts_mkBool(42)));"
        , "const x0 = Math.random();"
        , "const tid_p3 = i.wasmInstance.exports.rts_eval(i.wasmInstance.exports.rts_apply(i.staticsSymbolMap.base_GHCziBase_id_closure, i.wasmInstance.exports.rts_mkDouble(x0)));"
        , "const x1 = i.wasmInstance.exports.rts_getDouble(i.wasmInstance.exports.getTSOret(tid_p3));"
        , "console.log([x0, x1, x0 === x1]);"
        , "}"
        ]
    ] <>
    args
