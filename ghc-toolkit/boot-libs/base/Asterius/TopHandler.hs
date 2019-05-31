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
topHandler = throwExitCode realHandler

realHandler :: SomeException -> IO a
realHandler err = do
  prog <- peekCString prog_name
  hPutStrLn stderr $ prog <> ": " <> show err
  throwIO err

throwExitCode :: (SomeException -> IO a) -> SomeException -> IO a
throwExitCode h err =
  case fromException err of
    Just (_ :: ExitCode) -> throwIO err
    _ -> h err

foreign import ccall "&" prog_name :: CString
