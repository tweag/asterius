import Asterius.BuildInfo
import Asterius.Internals.Temp
import Data.Foldable
import Data.List
import qualified Paths_asterius
import System.Environment.Blank
import System.FilePath
import System.Process (callProcess)

main :: IO ()
main = do
  args <- getArgs
  env <- getEnvironment
  traverse_ unsetEnv
    $ filter (\k -> ("GHC_" `isPrefixOf` k) || "HASKELL_" `isPrefixOf` k)
    $ map fst env
  unsetEnv "CABAL_CONFIG"
  withTempDir "ahc-cabal" $ \tmpdir -> do
    let new_cabal_config_path = tmpdir </> "config"
    conf <- readFile =<< Paths_asterius.getDataFileName ("cabal" </> "config")
    writeFile new_cabal_config_path $
      "program-locations\n  ar-location: "
        <> ahcAr
        <> "\n  ghc-location: "
        <> ahc
        <> "\n  ghc-pkg-location: "
        <> ahcPkg
        <> "\n"
        <> conf
    callProcess "cabal" (["--config-file", new_cabal_config_path] <> args)
