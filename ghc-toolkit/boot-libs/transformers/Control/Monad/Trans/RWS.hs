{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.RWS
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- A monad transformer that combines 'ReaderT', 'WriterT' and 'StateT'.
-- This version is lazy; for a constant-space version with almost the
-- same interface, see "Control.Monad.Trans.RWS.CPS".
-----------------------------------------------------------------------------

module Control.Monad.Trans.RWS (
    module Control.Monad.Trans.RWS.Lazy
  ) where

import Control.Monad.Trans.RWS.Lazy
