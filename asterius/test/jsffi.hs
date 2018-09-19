import System.Environment
import System.Process

main :: IO ()
main = do
  args <- getArgs
  callProcess "ahc-link" $
    [ "--input"
    , "test/jsffi/jsffi.hs"
    , "--output-link-report"
    , "test/jsffi/jsffi.link.txt"
    , "--export-function=mult_hs_int"
    , "--export-function=mult_hs_double"
    , "--export-function=putchar"
    , "--run"
    ] <>
    [ mconcat
        [ "--asterius-instance-callback="
        , "i => {"
        , "i.wasmInstance.exports.hs_init();"
        , "i.wasmInstance.exports.main();"
        , "console.log(i.wasmInstance.exports.mult_hs_int(9, 9));"
        , "console.log(i.wasmInstance.exports.mult_hs_double(9, 9));"
        , "i.wasmInstance.exports.putchar(\"H\".codePointAt(0));"
        , "}"
        ]
    ] <>
    args
