import qualified Asterius.BuildInfo as A
import qualified Asterius.FrontendPlugin as A
import Language.Haskell.GHC.Toolkit.FakeGHC
import System.FilePath

main :: IO ()
main =
  fakeGHCMain $
    FakeGHCOptions
      A.ghc
      (A.dataDir </> ".boot" </> "asterius_lib")
      A.frontendPlugin
