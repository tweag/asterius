{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Parser for Haskell source code.
module Ormolu.Parser
  ( parseModule,
    manualExts,
  )
where

import Bag (bagToList)
import qualified CmdLineParser as GHC
import Control.Exception
import Control.Monad.IO.Class
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import Data.Ord (Down (Down))
import qualified Data.Text as T
import DynFlags as GHC
import ErrUtils (Severity (..), errMsgSeverity, errMsgSpan)
import qualified FastString as GHC
import GHC hiding (IE, UnicodeSyntax)
import GHC.DynFlags (baseDynFlags)
import GHC.LanguageExtensions.Type (Extension (..))
import qualified HeaderInfo as GHC
import qualified HscTypes as GHC
import qualified Lexer as GHC
import Ormolu.Config
import Ormolu.Exception
import Ormolu.Parser.Anns
import Ormolu.Parser.CommentStream
import Ormolu.Parser.Result
import Ormolu.Processing.Preprocess (preprocess)
import Ormolu.Utils (incSpanLine, removeIndentation)
import qualified Panic as GHC
import qualified Parser as GHC
import qualified StringBuffer as GHC

-- | Parse a complete module from string.
parseModule ::
  MonadIO m =>
  -- | Ormolu configuration
  Config RegionDeltas ->
  -- | File name (only for source location annotations)
  FilePath ->
  -- | Input for parser
  String ->
  m
    ( [GHC.Warn],
      Either (SrcSpan, String) ParseResult
    )
parseModule Config {..} path rawInput = liftIO $ do
  let (literalPrefix, indentedInput, literalSuffix, extraComments) =
        preprocess path rawInput cfgRegion
      (input, indent) = removeIndentation indentedInput
  -- It's important that 'setDefaultExts' is done before
  -- 'parsePragmasIntoDynFlags', because otherwise we might enable an
  -- extension that was explicitly disabled in the file.
  let baseFlags =
        GHC.setGeneralFlag'
          GHC.Opt_Haddock
          (setDefaultExts baseDynFlags)
      extraOpts = dynOptionToLocatedStr <$> cfgDynOptions
  (warnings, dynFlags) <-
    parsePragmasIntoDynFlags baseFlags extraOpts path rawInput >>= \case
      Right res -> pure res
      Left err ->
        let loc =
              mkSrcSpan
                (mkSrcLoc (GHC.mkFastString path) 1 1)
                (mkSrcLoc (GHC.mkFastString path) 1 1)
         in throwIO (OrmoluParsingFailed loc err)
  let useRecordDot =
        "record-dot-preprocessor" == pgm_F dynFlags
          || any
            (("RecordDotPreprocessor" ==) . moduleNameString)
            (pluginModNames dynFlags)
      pStateErrors = \pstate ->
        let errs = bagToList $ GHC.getErrorMessages pstate dynFlags
            fixupErrSpan = incSpanLine (regionPrefixLength cfgRegion)
         in case L.sortOn (Down . SeverityOrd . errMsgSeverity) errs of
              [] -> Nothing
              err : _ ->
                -- Show instance returns a short error message
                Just (fixupErrSpan (errMsgSpan err), show err)
      r = case runParser GHC.parseModule dynFlags path input of
        GHC.PFailed pstate ->
          case pStateErrors pstate of
            Just err -> Left err
            Nothing -> error "PFailed does not have an error"
        GHC.POk pstate (L _ hsModule) ->
          case pStateErrors pstate of
            -- Some parse errors (pattern/arrow syntax in expr context)
            -- do not cause a parse error, but they are replaced with "_"
            -- by the parser and the modified AST is propagated to the
            -- later stages; but we fail in those cases.
            Just err -> Left err
            Nothing ->
              let (stackHeader, shebangs, pragmas, comments) =
                    mkCommentStream input extraComments pstate
               in Right
                    ParseResult
                      { prParsedSource = hsModule,
                        prAnns = mkAnns pstate,
                        prStackHeader = stackHeader,
                        prShebangs = shebangs,
                        prPragmas = pragmas,
                        prCommentStream = comments,
                        prUseRecordDot = useRecordDot,
                        prImportQualifiedPost =
                          GHC.xopt ImportQualifiedPost dynFlags,
                        prLiteralPrefix = T.pack literalPrefix,
                        prLiteralSuffix = T.pack literalSuffix,
                        prIndent = indent
                      }
  return (warnings, r)

-- | Enable all language extensions that we think should be enabled by
-- default for ease of use.
setDefaultExts :: DynFlags -> DynFlags
setDefaultExts flags = L.foldl' xopt_set flags autoExts
  where
    autoExts = allExts L.\\ manualExts
    allExts = [minBound .. maxBound]

-- | Extensions that are not enabled automatically and should be activated
-- by user.
manualExts :: [Extension]
manualExts =
  [ Arrows, -- steals proc
    Cpp, -- forbidden
    BangPatterns, -- makes certain patterns with ! fail
    PatternSynonyms, -- steals the pattern keyword
    RecursiveDo, -- steals the rec keyword
    StaticPointers, -- steals static keyword
    TransformListComp, -- steals the group keyword
    UnboxedTuples, -- breaks (#) lens operator
    MagicHash, -- screws {-# these things #-}
    TypeApplications, -- steals (@) operator on some cases
    AlternativeLayoutRule,
    AlternativeLayoutRuleTransitional,
    MonadComprehensions,
    UnboxedSums,
    UnicodeSyntax, -- gives special meanings to operators like (→)
    TemplateHaskellQuotes, -- enables TH subset of quasi-quotes, this
    -- apparently interferes with QuasiQuotes in
    -- weird ways
    ImportQualifiedPost -- affects how Ormolu renders imports, so the
    -- decision of enabling this style is left to the user
  ]

-- | Run a 'GHC.P' computation.
runParser ::
  -- | Computation to run
  GHC.P a ->
  -- | Dynamic flags
  GHC.DynFlags ->
  -- | Module path
  FilePath ->
  -- | Module contents
  String ->
  -- | Parse result
  GHC.ParseResult a
runParser parser flags filename input = GHC.unP parser parseState
  where
    location = GHC.mkRealSrcLoc (GHC.mkFastString filename) 1 1
    buffer = GHC.stringToStringBuffer input
    parseState = GHC.mkPState flags buffer location

-- | Wrap GHC's 'Severity' to add 'Ord' instance.
newtype SeverityOrd = SeverityOrd Severity

instance Eq SeverityOrd where
  s1 == s2 = compare s1 s2 == EQ

instance Ord SeverityOrd where
  compare (SeverityOrd s1) (SeverityOrd s2) =
    compare (f s1) (f s2)
    where
      f :: Severity -> Int
      f SevOutput = 1
      f SevFatal = 2
      f SevInteractive = 3
      f SevDump = 4
      f SevInfo = 5
      f SevWarning = 6
      f SevError = 7

----------------------------------------------------------------------------
-- Helpers taken from HLint

parsePragmasIntoDynFlags ::
  -- | Pre-set 'DynFlags'
  DynFlags ->
  -- | Extra options (provided by user)
  [Located String] ->
  -- | File name (only for source location annotations)
  FilePath ->
  -- | Input for parser
  String ->
  IO (Either String ([GHC.Warn], DynFlags))
parsePragmasIntoDynFlags flags extraOpts filepath str =
  catchErrors $ do
    let opts = GHC.getOptions flags (GHC.stringToStringBuffer str) filepath
    (flags', leftovers, warnings) <-
      parseDynamicFilePragma flags (opts <> extraOpts)
    case NE.nonEmpty leftovers of
      Nothing -> return ()
      Just unrecognizedOpts ->
        throwIO (OrmoluUnrecognizedOpts (unLoc <$> unrecognizedOpts))
    let flags'' = flags' `gopt_set` Opt_KeepRawTokenStream
    return $ Right (warnings, flags'')
  where
    catchErrors act =
      GHC.handleGhcException
        reportErr
        (GHC.handleSourceError reportErr act)
    reportErr e = return $ Left (show e)
