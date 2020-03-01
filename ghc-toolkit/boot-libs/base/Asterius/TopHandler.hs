{-# LANGUAGE ScopedTypeVariables #-}

module Asterius.TopHandler
  ( runIO,
    runNonIO,
  )
where

import Control.Exception
import Foreign.C
import GHC.TopHandler (flushStdHandles)
import System.Exit
import System.IO
import Prelude

runIO :: IO a -> IO a
runIO = (`finally` flushStdHandles) . (`catch` topHandler)

runNonIO :: a -> IO a
runNonIO = runIO . evaluate

topHandler :: SomeException -> IO a
topHandler = throwExitCode realHandler

realHandler :: SomeException -> IO a
realHandler err = do
  prog <- peekCString prog_name
  hPutStrLn stderr $ prog <> ": " <> show err
  throwIO err

throwExitCode :: (SomeException -> IO a) -> SomeException -> IO a
throwExitCode h err = case fromException err of
  Just (_ :: ExitCode) -> throwIO err
  _ -> h err

foreign import ccall "&" prog_name :: CString
