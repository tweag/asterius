{-# LANGUAGE ViewPatterns #-}

import System.Directory
import System.Environment.Blank
import System.IO
import System.Process

main :: IO ()
main = do
  (reverse -> (('@':rsp_path):archive_path:_)) <- getArgs
  obj_paths <- lines <$> readFile rsp_path
  tmpdir <- getTemporaryDirectory
  (new_rsp_path, new_rsp_h) <- openTempFile tmpdir "ar.rsp"
  hPutStr new_rsp_h $ unlines obj_paths
  hClose new_rsp_h
  callProcess "ar" ["-r", "-c", archive_path, '@' : new_rsp_path]
  removeFile new_rsp_path
