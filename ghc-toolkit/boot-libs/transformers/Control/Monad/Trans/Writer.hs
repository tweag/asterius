{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Writer
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- The WriterT monad transformer.
-- This version builds its output lazily; for a constant-space version
-- with almost the same interface, see "Control.Monad.Trans.Writer.CPS".
-----------------------------------------------------------------------------

module Control.Monad.Trans.Writer (
    module Control.Monad.Trans.Writer.Lazy
  ) where

import Control.Monad.Trans.Writer.Lazy
