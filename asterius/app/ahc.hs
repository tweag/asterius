import qualified Asterius.BuildInfo as A
import qualified Asterius.FrontendPlugin as A
import Language.Haskell.GHC.Toolkit.FakeGHC
import System.Directory
import System.FilePath

main :: IO ()
main = do
  Just ghc <- findExecutable "ghc"
  fakeGHCMain $
    FakeGHCOptions
      ghc
      (A.dataDir </> ".boot" </> "asterius_lib")
      A.frontendPlugin
