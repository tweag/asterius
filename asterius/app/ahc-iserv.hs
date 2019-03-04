{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Asterius.Iserv.Run
import Asterius.Iserv.Trace
import Control.DeepSeq
import Control.Exception
import Data.Binary
import Data.IORef
import GHCi.Message
import GHCi.Signals
import GHCi.TH
import GHCi.Utils
import System.Environment.Blank

serv :: Bool -> Pipe -> (forall a. IO a -> IO a) -> IO ()
serv verbose pipe restore = loop
  where
    loop = do
      trace verbose "reading pipe..."
      Msg msg <- readPipe pipe getMessage
      discardCtrlC
      trace verbose $ "msg: " <> show msg
      case msg of
        Shutdown -> pure ()
        RunTH st q ty loc -> wrapRunTH $ runTH pipe st q ty loc
        RunModFinalizers st qrefs ->
          wrapRunTH $ runModFinalizerRefs pipe st qrefs
        _other -> run msg >>= reply
    reply ::
         forall a. (Binary a, Show a)
      => a
      -> IO ()
    reply r = do
      trace verbose $ "writing pipe: " <> show r
      writePipe pipe (put r)
      loop
  -- Run some TH code, which may interact with GHC by sending
  -- THMessage requests, and then finally send RunTHDone followed by a
  -- QResult.  For an overview of how TH works with Remote GHCi, see
  -- Note [Remote Template Haskell] in libraries/ghci/GHCi/TH.hs.
    wrapRunTH ::
         forall a. (Binary a, Show a)
      => IO a
      -> IO ()
    wrapRunTH io = do
      trace verbose "wrapRunTH..."
      r <- try io
      trace verbose "wrapRunTH done."
      trace verbose "writing RunTHDone."
      writePipe pipe $ putTHMessage RunTHDone
      case r of
        Left e
          | Just (GHCiQException _ err) <- fromException e -> do
            trace verbose $ "QFail " <> show err
            reply (QFail err :: QResult a)
          | otherwise -> do
            str <- showException e
            trace verbose $ "QException " <> str
            reply (QException str :: QResult a)
        Right a -> do
          trace verbose "QDone"
          reply $ QDone a
  -- carefully when showing an exception, there might be other exceptions
  -- lurking inside it.  If so, we return the inner exception instead.
    showException :: SomeException -> IO String
    showException e0 = do
      trace verbose "showException"
      r <- try $ evaluate $ force $ show (e0 :: SomeException)
      case r of
        Left e -> showException e
        Right str -> pure str
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

main :: IO ()
main = do
  arg0:arg1:_ <- getArgs
  inh <- getGhcHandle $ read arg1
  outh <- getGhcHandle $ read arg0
  installSignalHandlers
  lo_ref <- newIORef Nothing
  uninterruptibleMask $
    serv True Pipe {pipeRead = inh, pipeWrite = outh, pipeLeftovers = lo_ref}
