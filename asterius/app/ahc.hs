import qualified Asterius.BuildInfo as BI
import Language.Haskell.GHC.Toolkit.FakeGHC

main :: IO ()
main =
  fakeMain
    FakeGHCOptions
      { ghc = BI.ghc
      , pluginModuleName = "Asterius.FrontendPlugin"
      , pluginPackageName = "asterius"
      , packageDBStack = BI.packageDBStack
      }
