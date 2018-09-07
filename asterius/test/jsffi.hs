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
    , "--export-function=mult_hs"
    , "--run"
    ] <>
    [ mconcat
        [ "--asterius-instance-callback="
        , "i => {"
        , "i.wasmInstance.exports.hs_init();"
        , "i.wasmInstance.exports.main();"
        , "console.log(i.wasmInstance.exports.mult_hs(9, 9));"
        , "}"
        ]
    ] <>
    args
