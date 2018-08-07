{-# LANGUAGE CPP, FlexibleContexts, DataKinds, MonoLocalBinds #-}

module Data.IntMap.Internal.DeprecatedDebug where
import Data.IntMap.Internal (IntMap)

import Utils.Containers.Internal.TypeError


-- | 'showTree' has moved to 'Data.IntMap.Internal.Debug.showTree'
showTree :: Whoops "Data.IntMap.showTree has moved to Data.IntMap.Internal.Debug.showTree"
         => IntMap a -> String
showTree _ = undefined

-- | 'showTreeWith' has moved to 'Data.IntMap.Internal.Debug.showTreeWith'
showTreeWith :: Whoops "Data.IntMap.showTreeWith has moved to Data.IntMap.Internal.Debug.showTreeWith"
             => Bool -> Bool -> IntMap a -> String
showTreeWith _ _ _ = undefined
