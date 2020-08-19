{-# LANGUAGE OverloadedStrings #-}

-- | Common definitions for pre- and post- processing.
module Ormolu.Processing.Common
  ( OrmoluState (..),
    startDisabling,
    endDisabling,
  )
where

import Data.String (IsString (..))

-- | Ormolu state.
data OrmoluState
  = -- | Enabled
    OrmoluEnabled
  | -- | Disabled
    OrmoluDisabled
  deriving (Eq, Show)

-- | Marker for the beginning of the region where Ormolu should be disabled.
startDisabling :: IsString s => s
startDisabling = "{- ORMOLU_DISABLE_START"

-- | Marker for the end of the region where Ormolu should be disabled.
endDisabling :: IsString s => s
endDisabling = "ORMOLU_DISABLE_END -}"
