import System.Environment
import System.Directory
import System.Process

main :: IO ()
main = do
  args <- getArgs
  callProcess "cabal" $
    ["new-run", "--project-file", "test.project", "--builddir", "dist-ahc", "asterius-test:exe:array", "--with-ghc=ahc", "--with-ghc-pkg=ahc-pkg"]
      <> args
