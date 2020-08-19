{-# LANGUAGE LambdaCase #-}

-- | 'OrmoluException' type and surrounding definitions.
module Ormolu.Exception
  ( OrmoluException (..),
    withPrettyOrmoluExceptions,
  )
where

import Control.Exception
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified GHC
import Ormolu.Utils (showOutputable)
import qualified Outputable as GHC
import System.Exit (ExitCode (..), exitWith)
import System.IO

-- | Ormolu exception representing all cases when Ormolu can fail.
data OrmoluException
  = -- | Parsing of original source code failed
    OrmoluParsingFailed GHC.SrcSpan String
  | -- | Parsing of formatted source code failed
    OrmoluOutputParsingFailed GHC.SrcSpan String
  | -- | Original and resulting ASTs differ
    OrmoluASTDiffers FilePath [GHC.SrcSpan]
  | -- | Formatted source code is not idempotent
    OrmoluNonIdempotentOutput GHC.RealSrcLoc Text Text
  | -- | Some GHC options were not recognized
    OrmoluUnrecognizedOpts (NonEmpty String)
  deriving (Eq, Show)

instance Exception OrmoluException where
  displayException = \case
    OrmoluParsingFailed s e ->
      showParsingErr "The GHC parser (in Haddock mode) failed:" s [e]
    OrmoluOutputParsingFailed s e ->
      showParsingErr "Parsing of formatted code failed:" s [e]
        ++ "Please, consider reporting the bug.\n"
    OrmoluASTDiffers path ss ->
      unlines $
        [ "AST of input and AST of formatted code differ."
        ]
          ++ fmap
            withIndent
            ( case fmap (\s -> "at " ++ showOutputable s) ss of
                [] -> ["in " ++ path]
                xs -> xs
            )
          ++ ["Please, consider reporting the bug."]
    OrmoluNonIdempotentOutput loc left right ->
      showParsingErr
        "Formatting is not idempotent:"
        loc
        ["before: " ++ show left, "after:  " ++ show right]
        ++ "Please, consider reporting the bug.\n"
    OrmoluUnrecognizedOpts opts ->
      unlines
        [ "The following GHC options were not recognized:",
          (withIndent . unwords . NE.toList) opts
        ]

-- | Inside this wrapper 'OrmoluException' will be caught and displayed
-- nicely using 'displayException'.
withPrettyOrmoluExceptions ::
  -- | Action that may throw the exception
  IO a ->
  IO a
withPrettyOrmoluExceptions m = m `catch` h
  where
    h :: OrmoluException -> IO a
    h e = do
      hPutStrLn stderr (displayException e)
      exitWith . ExitFailure $
        case e of
          -- Error code 1 is for 'error' or 'notImplemented'
          -- 2 used to be for erroring out on CPP
          OrmoluParsingFailed {} -> 3
          OrmoluOutputParsingFailed {} -> 4
          OrmoluASTDiffers {} -> 5
          OrmoluNonIdempotentOutput {} -> 6
          OrmoluUnrecognizedOpts {} -> 7

----------------------------------------------------------------------------
-- Helpers

-- | Show a parse error.
showParsingErr :: GHC.Outputable a => String -> a -> [String] -> String
showParsingErr msg spn err =
  unlines $
    [ msg,
      withIndent (showOutputable spn)
    ]
      ++ map withIndent err

-- | Indent with 2 spaces for readability.
withIndent :: String -> String
withIndent txt = "  " ++ txt
