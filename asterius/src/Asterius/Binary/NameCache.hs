module Asterius.Binary.NameCache
  ( newNameCacheUpdater,
  )
where

import Data.IORef
import qualified GhcPlugins as GHC
import qualified IfaceEnv as GHC
import qualified NameCache as GHC
import qualified PrelInfo as GHC

newNameCacheUpdater :: IO GHC.NameCacheUpdater
newNameCacheUpdater = do
  us <- GHC.mkSplitUniqSupply 'r'
  nc <- newIORef (GHC.initNameCache us GHC.knownKeyNames)
  pure $ GHC.NCU $ atomicModifyIORef' nc
