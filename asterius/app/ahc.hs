import qualified Asterius.BuildInfo as A
import qualified Asterius.FrontendPlugin as A
import Language.Haskell.GHC.Toolkit.FakeGHC
import System.FilePath

main :: IO ()
main = do
  bootDir <- A.getBootDir
  fakeGHCMain $
    FakeGHCOptions
      A.ghc
      (bootDir </> ".boot" </> "asterius_lib")
      A.frontendPlugin
