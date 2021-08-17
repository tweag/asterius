import qualified Asterius.FrontendPlugin as A
import qualified GHC.Frontend.Ghc as GHC

main :: IO ()
main = GHC.main A.frontendPlugin
