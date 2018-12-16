import System.Environment
import System.Process

main :: IO ()
main = do
  args <- getArgs
  callProcess "ahc-link" $
    [ "--input"
    , "test/vault/vault.hs"
    , "--output-link-report"
    , "test/vault/vault.link.txt"
    , "--run"
    ] <>
    [ mconcat
        [ "--asterius-instance-callback="
        , "async i => {"
        , "i.wasmInstance.exports.hs_init();"
        , "i.wasmInstance.exports.main();"
        , "i.vault = new Map([['key', 'Vault value set from js']]);"
        , "await new Promise(resolve => setTimeout(resolve, 1024));"
        , "i.wasmInstance.exports.main();"
        , "console.log(i.stdio.stdout());"
        , "}"
        ]
    ] <>
    args
