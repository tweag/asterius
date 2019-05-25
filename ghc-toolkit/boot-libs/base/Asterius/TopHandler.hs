{-# LANGUAGE ScopedTypeVariables #-}

module Asterius.TopHandler
  ( runMainIO
  ) where

import Control.Exception
import Foreign.C
import Prelude
import System.Exit
import System.IO

runMainIO :: IO a -> IO a
runMainIO = (`catch` topHandler)

topHandler :: SomeException -> IO a
topHandler err = catch (realHandler err) topHandler

realHandler :: SomeException -> IO a
realHandler err =
  case fromException err of
    Just (_ :: ExitCode) -> throwIO err
    _ -> do
      prog <- peekCString prog_name
      hPutStrLn stderr $ prog <> ": " <> show err
      throwIO err

foreign import ccall "&" prog_name :: CString
