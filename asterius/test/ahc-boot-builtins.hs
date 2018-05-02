{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

import Asterius.Boot
import Asterius.Internals
import Asterius.Store
import Prelude hiding (IO)
import System.FilePath

updateRTSAsteriusCmm :: BootArgs -> IO ()
updateRTSAsteriusCmm BootArgs {..} = do
  orig_store <- decodeFile (obj_topdir </> "asterius_store")
  encodeFile (obj_topdir </> "asterius_store") $
    builtinsStore builtinsOptions <> orig_store
  where
    obj_topdir = bootDir </> "asterius_lib"

main :: IO ()
main = getDefaultBootArgs >>= updateRTSAsteriusCmm
