import qualified Asterius.BuildInfo as A
import Data.Foldable
import Data.List
import System.Environment.Blank
import System.FilePath
import System.Process (callProcess)

main :: IO ()
main = do
  env <- getEnvironment
  traverse_ unsetEnv
    $ filter (\k -> ("GHC_" `isPrefixOf` k) || "HASKELL_" `isPrefixOf` k)
    $ map fst env
  args <- getArgs
  callProcess A.ghcPkg $
    ( "--global-package-db="
        <> (A.dataDir </> ".boot" </> "asterius_lib" </> "package.conf.d")
    )
      : args
