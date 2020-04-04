{-# LANGUAGE OverloadedStrings #-}

module Asterius.CodeGen.Droppable
  ( ccallResultDroppable,
  )
where

import Asterius.Types

ccallResultDroppable :: EntitySymbol -> [ValueType]
ccallResultDroppable sym | sym == "memset" || sym == "memcpy" = [I64]
ccallResultDroppable _ = []
