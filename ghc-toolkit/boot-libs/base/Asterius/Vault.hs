{-# LANGUAGE NoImplicitPrelude #-}

module Asterius.Vault
  ( vaultInsert
  , vaultLookup
  , vaultDelete
  ) where

import Asterius.Types
import GHC.Base

{-# INLINE vaultInsert #-}
vaultInsert :: JSArrayBuffer -> JSVal -> IO ()
vaultInsert = js_vaultInsert

{-# INLINE vaultLookup #-}
vaultLookup :: JSArrayBuffer -> IO (Maybe JSVal)
vaultLookup buf = do
  f <- js_vaultHas buf
  if f
    then do
      r <- js_vaultLookup buf
      pure (Just r)
    else pure Nothing

{-# INLINE vaultDelete #-}
vaultDelete :: JSArrayBuffer -> IO ()
vaultDelete = js_vaultDelete

foreign import javascript "__asterius_jsffi.vaultInsert(${1},${2})" js_vaultInsert
  :: JSArrayBuffer -> JSVal -> IO ()

foreign import javascript "__asterius_jsffi.vaultHas(${1})" js_vaultHas
  :: JSArrayBuffer -> IO Bool

foreign import javascript "__asterius_jsffi.vaultLookup(${1})" js_vaultLookup
  :: JSArrayBuffer -> IO JSVal

foreign import javascript "__asterius_jsffi.vaultDelete(${1})" js_vaultDelete
  :: JSArrayBuffer -> IO ()
