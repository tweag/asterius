{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Asterius.Iserv.Run
  ( run
  ) where

import Asterius.Internals.Temp
import Asterius.Iserv.State
import Asterius.JSRun.Main
import Asterius.JSRun.NonMain
import Asterius.Types
import Data.ByteString (ByteString)
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import Data.List
import GHCi.Message
import GHCi.RemoteTypes
import Language.JavaScript.Inline.Core
import System.FilePath
import Unsafe.Coerce

import Asterius.Iserv.Trace

startTH :: IO (RemoteRef (IORef QState))
startTH = pure $ unsafeCoerce $ RemotePtr 0

runTH ::
     IservState -> Pipe -> HValueRef -> THResultType -> IO (QResult ByteString)
runTH s@IservState {..} pipe hv rt = do
  let runner_sym =
        case rt of
          THExp -> "ghci_AsteriusziGHCi_asteriusRunQExp_closure"
          THPat -> "ghci_AsteriusziGHCi_asteriusRunQPat_closure"
          THType -> "ghci_AsteriusziGHCi_asteriusRunQType_closure"
          THDec -> "ghci_AsteriusziGHCi_asteriusRunQDec_closure"
          _ ->
            error $ "Asterius.Iserv.Run: unsupported TH result type " <> show rt
  (sym, m) <- loadSplice s hv
  withTempDir "ASDF" $ \p -> do
    i <-
      newAsteriusInstanceNonMain
        iservJSSession
        (p </> "ASDF")
        [runner_sym, sym]
        m
    trace True "pre hs_init"
    hsInit iservJSSession i
    trace True "post hs_init"
    let sym2code x = JSCode (shortByteString (entityName x))
        i_sym x = deRefJSVal i <> ".symbolTable." <> sym2code x
        i_func f xs =
          deRefJSVal i <> ".wasmInstance.exports." <> sym2code f <> "(" <>
          mconcat (intersperse "," xs) <>
          ")"
        i_runner_sym = i_sym runner_sym
        i_q_sym = i_sym sym
        buf_expr =
          deRefJSVal i <> ".getJSVal(" <>
          i_func
            "rts_getStablePtr"
            [ i_func
                "getTSOret"
                [ i_func
                    "rts_evalIO"
                    [i_func "rts_apply" [i_runner_sym, i_q_sym]]
                ]
            ] <>
          ")"
    buf <- eval iservJSSession buf_expr
    writePipe pipe $ putTHMessage RunTHDone
    pure $ QDone $ LBS.toStrict buf

run :: IservState -> Pipe -> Message a -> IO a
run s pipe msg =
  case msg of
    InitLinker -> pure ()
    LoadDLL _ -> pure Nothing
    LoadArchive p -> addArchive s p
    LoadObj p -> addObj s p
    AddLibrarySearchPath _ -> pure $ RemotePtr 0
    RemoveLibrarySearchPath _ -> pure True
    ResolveObjs -> pure True
    FindSystemLibrary _ -> pure $ Just ""
    CreateBCOs [sym_buf, m_buf] -> (: []) <$> createSplice s sym_buf m_buf
    StartTH -> startTH
    RunTH _ rhv ty _ -> runTH s pipe rhv ty
    _ -> fail $ "Asterius.Iserv.Run.run: unsupported message: " <> show msg
