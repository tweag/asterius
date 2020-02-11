{-# LANGUAGE OverloadedStrings #-}

module Asterius.CodeGen.Droppable
  ( ccallResultDroppable,
  )
where

import Asterius.Types

ccallResultDroppable :: AsteriusEntitySymbol -> [ValueType]
ccallResultDroppable sym | sym == "memset" || sym == "memcpy" = [I64]
ccallResultDroppable _ = []
