import qualified Asterius.BuildInfo as A
import System.Environment.Blank
import System.FilePath
import System.Process

main :: IO ()
main = do
  args <- getArgs
  callProcess A.ghcPkg $ do
    arg <- args
    if arg == "--global"
      then
        [ "--global",
          "--global-package-db="
            <> (A.dataDir </> ".boot" </> "asterius_lib" </> "package.conf.d")
        ]
      else pure arg
