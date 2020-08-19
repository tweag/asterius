{-# LANGUAGE BangPatterns #-}

-- | A formatter for Haskell source code.
module Ormolu
  ( ormolu,
    ormoluFile,
    ormoluStdin,
    Config (..),
    RegionIndices (..),
    defaultConfig,
    DynOption (..),
    OrmoluException (..),
    withPrettyOrmoluExceptions,
  )
where

import qualified CmdLineParser as GHC
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (MonadIO (..))
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace
import Ormolu.Config
import Ormolu.Diff
import Ormolu.Exception
import Ormolu.Parser
import Ormolu.Parser.Result
import Ormolu.Printer
import Ormolu.Utils (showOutputable)
import qualified SrcLoc as GHC

-- | Format a 'String', return formatted version as 'Text'.
--
-- The function
--
--     * Takes 'String' because that's what GHC parser accepts.
--     * Needs 'IO' because some functions from GHC that are necessary to
--       setup parsing context require 'IO'. There should be no visible
--       side-effects though.
--     * Takes file name just to use it in parse error messages.
--     * Throws 'OrmoluException'.
ormolu ::
  MonadIO m =>
  -- | Ormolu configuration
  Config RegionIndices ->
  -- | Location of source file
  FilePath ->
  -- | Input to format
  String ->
  m Text
ormolu cfgWithIndices path str = do
  let totalLines = length (lines str)
      cfg = regionIndicesToDeltas totalLines <$> cfgWithIndices
  (warnings, result0) <-
    parseModule' cfg OrmoluParsingFailed path str
  when (cfgDebug cfg) $ do
    traceM "warnings:\n"
    traceM (concatMap showWarn warnings)
    traceM (prettyPrintParseResult result0)
  -- We're forcing 'txt' here because otherwise errors (such as messages
  -- about not-yet-supported functionality) will be thrown later when we try
  -- to parse the rendered code back, inside of GHC monad wrapper which will
  -- lead to error messages presenting the exceptions as GHC bugs.
  let !txt = printModule result0
  when (not (cfgUnsafe cfg) || cfgCheckIdempotence cfg) $ do
    let pathRendered = path ++ "<rendered>"
    -- Parse the result of pretty-printing again and make sure that AST
    -- is the same as AST of original snippet module span positions.
    (_, result1) <-
      parseModule'
        cfg
        OrmoluOutputParsingFailed
        pathRendered
        (T.unpack txt)
    unless (cfgUnsafe cfg) $
      case diffParseResult result0 result1 of
        Same -> return ()
        Different ss -> liftIO $ throwIO (OrmoluASTDiffers path ss)
    -- Try re-formatting the formatted result to check if we get exactly
    -- the same output.
    when (cfgCheckIdempotence cfg) $
      let txt2 = printModule result1
       in case diffText txt txt2 pathRendered of
            Nothing -> return ()
            Just (loc, l, r) ->
              liftIO $
                throwIO (OrmoluNonIdempotentOutput loc l r)
  return txt

-- | Load a file and format it. The file stays intact and the rendered
-- version is returned as 'Text'.
--
-- > ormoluFile cfg path =
-- >   liftIO (readFile path) >>= ormolu cfg path
ormoluFile ::
  MonadIO m =>
  -- | Ormolu configuration
  Config RegionIndices ->
  -- | Location of source file
  FilePath ->
  -- | Resulting rendition
  m Text
ormoluFile cfg path =
  liftIO (readFile path) >>= ormolu cfg path

-- | Read input from stdin and format it.
--
-- > ormoluStdin cfg =
-- >   liftIO (hGetContents stdin) >>= ormolu cfg "<stdin>"
ormoluStdin ::
  MonadIO m =>
  -- | Ormolu configuration
  Config RegionIndices ->
  -- | Resulting rendition
  m Text
ormoluStdin cfg =
  liftIO getContents >>= ormolu cfg "<stdin>"

----------------------------------------------------------------------------
-- Helpers

-- | A wrapper around 'parseModule'.
parseModule' ::
  MonadIO m =>
  -- | Ormolu configuration
  Config RegionDeltas ->
  -- | How to obtain 'OrmoluException' to throw when parsing fails
  (GHC.SrcSpan -> String -> OrmoluException) ->
  -- | File name to use in errors
  FilePath ->
  -- | Actual input for the parser
  String ->
  m ([GHC.Warn], ParseResult)
parseModule' cfg mkException path str = do
  (warnings, r) <- parseModule cfg path str
  case r of
    Left (spn, err) -> liftIO $ throwIO (mkException spn err)
    Right x -> return (warnings, x)

-- | Pretty-print a 'GHC.Warn'.
showWarn :: GHC.Warn -> String
showWarn (GHC.Warn reason l) =
  unlines
    [ showOutputable reason,
      showOutputable l
    ]
