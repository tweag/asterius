import qualified Asterius.BuildInfo as A
import System.Environment.Blank
import System.FilePath
import System.Process
import Data.List (isPrefixOf)

main :: IO ()
main = do
  bootDir <- A.getBootDir
  args <- getArgs
  callProcess A.ghcPkg $
    ("--global-package-db=" <> bootDir </> ".boot" </> "asterius_lib" </> "package.conf.d")
    : args
