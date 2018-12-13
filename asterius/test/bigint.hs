import System.Environment
import System.Process

main :: IO ()
main = do
  args <- getArgs
  callProcess "ahc-link" $
    [ "--input"
    , "test/bigint/bigint.hs"
    , "--output-link-report"
    , "test/bigint/bigint.link.txt"
    , "--run"
    ] <>
    [ mconcat
        [ "--asterius-instance-callback="
        , "i => {"
        , "i.wasmInstance.exports.hs_init();"
        , "i.wasmInstance.exports.main();"
        , "console.log(i.stdio.stdout());"
        , "console.log(i.stdio.stderr());"
        , "}"
        ]
    ] <>
    args
