{-# LANGUAGE ScopedTypeVariables #-}

module Asterius.Backends.Binaryen.CheckOverlapDataSegment
  ( checkOverlapDataSegment,
  )
where

import qualified Binaryen.Module as Binaryen
import Control.Monad
import Data.List
import Data.Traversable

checkOverlapDataSegment :: Binaryen.Module -> IO ()
checkOverlapDataSegment m = do
  (n :: Int) <- fromIntegral <$> Binaryen.getNumMemorySegments m
  segs <- fmap (sortOn fst) $
    for [0 .. n - 1] $ \_i -> do
      let i = fromIntegral _i
      (o :: Int) <- fromIntegral <$> Binaryen.getMemorySegmentByteOffset m i
      (l :: Int) <- fromIntegral <$> Binaryen.getMemorySegmentByteLength m i
      pure (o, l)
  let f = and [o0 + l0 <= o1 | ((o0, l0), (o1, _)) <- zip segs (tail segs)]
  unless f $ fail "OVERLAPPING DATA SEGMENT"
