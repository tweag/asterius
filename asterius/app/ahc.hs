import qualified Asterius.BuildInfo as A
import qualified Asterius.FrontendPlugin as A
import Language.Haskell.GHC.Toolkit.FakeGHC

main :: IO ()
main = fakeGHCMain $ FakeGHCOptions A.ghc A.ghcLibDir A.frontendPlugin
