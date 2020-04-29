import qualified Asterius.BuildInfo as A
import Data.Foldable
import Data.List
import System.Directory
import System.Environment.Blank
import System.FilePath
import System.Process (callProcess)

main :: IO ()
main = do
  Just ghcPkg <- findExecutable "ghc-pkg"
  env <- getEnvironment
  traverse_ unsetEnv
    $ filter (\k -> ("GHC_" `isPrefixOf` k) || "HASKELL_" `isPrefixOf` k)
    $ map fst env
  args <- getArgs
  callProcess ghcPkg $
    ( "--global-package-db="
        <> (rootBootDir </> ".boot" </> "asterius_lib" </> "package.conf.d")
    )
      : args
