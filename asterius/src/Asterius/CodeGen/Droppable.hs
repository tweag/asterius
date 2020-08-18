{-# LANGUAGE OverloadedStrings #-}

module Asterius.CodeGen.Droppable
  ( ccallResultDroppable,
  )
where

import Asterius.Types

ccallResultDroppable :: EntitySymbol -> [ValueType]
ccallResultDroppable sym
  | sym `elem` ["memset", "memcpy", "clock_gettime", "clock_getres"] = [I64]
ccallResultDroppable _ = []
