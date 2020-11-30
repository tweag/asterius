import qualified Asterius.BuildInfo as A
import System.Directory
import System.Environment.Blank
import System.FilePath
import System.Process (callProcess)

main :: IO ()
main = do
  Just ghcPkg <- findExecutable "ghc-pkg-asterius"
  args <- getArgs
  callProcess ghcPkg $
    ( "--global-package-db="
        <> (A.dataDir </> ".boot" </> "asterius_lib" </> "package.conf.d")
    )
      : args
