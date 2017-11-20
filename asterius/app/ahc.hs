import Asterius.BuildInfo
import Asterius.FrontendPlugin
import Data.Binary
import qualified Data.ByteString.Lazy as LBS
import Distribution.Simple.Compiler
import GHC
import System.Directory
import System.Environment.Blank
import System.IO
import System.Process
import UnliftIO.Exception

main :: IO ()
main = do
  ss <- getArgs
  let (nargs, moa) =
        foldr
          (\arg (nargs', moa') ->
             let fp =
                   [ "--frontend"
                   , "Asterius.FrontendPlugin"
                   , "-plugin-package"
                   , "asterius"
                   ] ++
                   case registrationPackageDB pkgDbStack of
                     GlobalPackageDB -> ["-global-package-db"]
                     UserPackageDB -> ["-user-package-db"]
                     SpecificPackageDB p -> ["-package-db", p]
              in case arg of
                   "--make" ->
                     ( fp ++ nargs'
                     , Just OriginalArgs {args = ss, mode = CompManager})
                   "-c" ->
                     ( fp ++ nargs'
                     , Just OriginalArgs {args = ss, mode = OneShot})
                   _ -> (arg : nargs', moa'))
          ([], Nothing)
          ss
  case moa of
    Just oa -> do
      tmpdir <- getTemporaryDirectory
      bracket
        (openBinaryTempFile tmpdir "asterius.tmp")
        (\(p, _) -> removeFile p) $ \(p, h) -> do
        LBS.hPut h $ encode oa
        hClose h
        setEnv "ASTERIUS_ORIGINAL_ARGS_PATH" p True
        callProcess ghc nargs
    _ -> callProcess ghc nargs
