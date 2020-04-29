import System.Directory
import System.Environment
import System.Process

main :: IO ()
main = do
  args <- getArgs
  -- withCurrentDirectory "test/cloudflare" $ callCommand "npm install"
--  callProcess "cabal" $
--    ["new-run", "--project-file", "test.project", "--builddir", "dist-ahc", "asterius-test:exe:cloudflare", "--with-ghc=ahc", "--with-ghc-pkg=ahc-pkg"] <> args
  callProcess "ahc-link" $
    [ "--bundle",
      "--browser",
      "--input-hs",
      "asterius/test/cloudflare/cloudflare.hs",
      "--input-mjs",
      "asterius/test/cloudflare/cloudflare.mjs",
      "--export-function=handleFetch",
      "--no-main"
    ]
      <> args
  -- withCurrentDirectory "test/cloudflare" $ callProcess "npm" [ "run", "test" ]
