{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Asterius.Iserv.Run
import Asterius.Iserv.State
import Asterius.Iserv.Trace
import Control.Exception
import Data.Binary
import Data.IORef
import GHCi.Message
import GHCi.Signals
import GHCi.Utils
import System.Environment.Blank

serv :: Bool -> Pipe -> (forall a. IO a -> IO a) -> IO ()
serv verbose pipe restore =
  withIservState $ \s -> do
    let loop = do
          trace verbose "reading pipe..."
          Msg msg <- readPipe pipe getMessage
          discardCtrlC
          trace verbose $ "msg: " <> show msg
          case msg of
            Shutdown -> pure ()
            _other -> run s pipe msg >>= reply
        reply ::
             forall a. (Binary a, Show a)
          => a
          -> IO ()
        reply r = do
          trace verbose $ "writing pipe: " <> show r
          writePipe pipe (put r)
          loop
  -- throw away any pending ^C exceptions while we're not running
  -- interpreted code.  GHC will also get the ^C, and either ignore it
  -- (if this is GHCi), or tell us to quit with a Shutdown message.
        discardCtrlC = do
          trace verbose "discardCtrlC"
          r <- try $ restore $ pure ()
          case r of
            Left UserInterrupt -> pure () *> discardCtrlC
            Left e -> throwIO e
            _ -> pure ()
    loop

main :: IO ()
main = do
  arg0:arg1:_ <- getArgs
  inh <- getGhcHandle $ read arg1
  outh <- getGhcHandle $ read arg0
  installSignalHandlers
  lo_ref <- newIORef Nothing
  uninterruptibleMask $
    serv True Pipe {pipeRead = inh, pipeWrite = outh, pipeLeftovers = lo_ref}
