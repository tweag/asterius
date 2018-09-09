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
    ] <>
    [ mconcat
        [ "--asterius-instance-callback="
        , "i => {"
        , "const cap = i.staticsSymbolMap.MainCapability;"
        , "i.wasmInstance.exports.hs_init();"
        , "i.wasmInstance.exports.main();"
        , "i.wasmInstance.exports.rts_evalLazyIO(cap, i.wasmInstance.exports.rts_apply(cap, i.staticsSymbolMap.Main_printInt_closure, i.wasmInstance.exports.rts_apply(cap, i.staticsSymbolMap.Main_fact_closure, i.wasmInstance.exports.rts_mkInt(cap, 5))), 0);"
        , "const ret_p1 = i.wasmInstance.exports.allocate(cap, 1);"
        , "i.wasmInstance.exports.rts_eval(cap, i.wasmInstance.exports.rts_apply(cap, i.staticsSymbolMap.Main_fact_closure, i.wasmInstance.exports.rts_mkInt(cap, 5)), ret_p1);"
        , "console.log(i.wasmInstance.exports.rts_getInt(i.wasmInstance.exports.loadI64(ret_p1)));"
        , "const ret_p2 = i.wasmInstance.exports.allocate(cap, 1);"
        , "i.wasmInstance.exports.rts_evalStableIO(cap, i.wasmInstance.exports.getStablePtr(i.wasmInstance.exports.rts_apply(cap, i.staticsSymbolMap.Main_printInt_closure, i.wasmInstance.exports.rts_apply(cap, i.staticsSymbolMap.Main_fact_closure, i.wasmInstance.exports.rts_mkInt(cap, 5)))), ret_p2);"
        , "console.log((i.wasmInstance.exports.deRefStablePtr(i.wasmInstance.exports.loadI64(ret_p2)) & (~ 7)) === i.staticsSymbolMap.ghczmprim_GHCziTuple_Z0T_closure);"
        , "console.log(i.wasmInstance.exports.rts_getBool(i.staticsSymbolMap.ghczmprim_GHCziTypes_False_closure));"
        , "console.log(i.wasmInstance.exports.rts_getBool(i.staticsSymbolMap.ghczmprim_GHCziTypes_True_closure));"
        , "console.log(i.wasmInstance.exports.rts_getBool(i.wasmInstance.exports.rts_mkBool(cap, 0)));"
        , "console.log(i.wasmInstance.exports.rts_getBool(i.wasmInstance.exports.rts_mkBool(cap, 42)));"
        , "}"
        ]
    ] <>
    args
