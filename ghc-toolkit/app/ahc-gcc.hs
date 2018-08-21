import qualified Language.Haskell.GHC.Toolkit.BuildInfo as BI
import Language.Haskell.GHC.Toolkit.FakeGCC
import System.FilePath

main :: IO ()
main =
  fakeGCCMain
    FakeGCCOptions
      { gccPath = BI.gccPath
      , ghcLibDir = BI.ghcLibDir
      , prependIncludeDir = BI.dataDir </> "include"
      }
