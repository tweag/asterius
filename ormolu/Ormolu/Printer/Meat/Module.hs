{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Rendering of modules.
module Ormolu.Printer.Meat.Module
  ( p_hsModule,
  )
where

import Control.Monad
import qualified Data.Text as T
import GHC
import Ormolu.Imports (normalizeImports)
import Ormolu.Parser.CommentStream
import Ormolu.Parser.Pragma
import Ormolu.Parser.Shebang
import Ormolu.Printer.Combinators
import Ormolu.Printer.Comments
import Ormolu.Printer.Meat.Common
import Ormolu.Printer.Meat.Declaration
import Ormolu.Printer.Meat.Declaration.Warning
import Ormolu.Printer.Meat.ImportExport
import Ormolu.Printer.Meat.Pragma

-- | Render a module.
p_hsModule ::
  -- | Stack header
  Maybe (RealLocated Comment) ->
  -- | Shebangs
  [Shebang] ->
  -- | Pragmas and the associated comments
  [([RealLocated Comment], Pragma)] ->
  -- | Whether to use postfix qualified imports
  Bool ->
  -- | AST to print
  HsModule GhcPs ->
  R ()
p_hsModule mstackHeader shebangs pragmas qualifiedPost HsModule {..} = do
  let deprecSpan = maybe [] (\(L s _) -> [s]) hsmodDeprecMessage
      exportSpans = maybe [] (\(L s _) -> [s]) hsmodExports
  switchLayout (deprecSpan <> exportSpans) $ do
    forM_ shebangs $ \(Shebang x) ->
      located x $ \shebang -> do
        txt (T.pack shebang)
        newline
    forM_ mstackHeader $ \(L spn comment) -> do
      spitCommentNow spn comment
      newline
    newline
    p_pragmas pragmas
    newline
    case hsmodName of
      Nothing -> return ()
      Just hsmodName' -> do
        located hsmodName' $ \name -> do
          forM_ hsmodHaddockModHeader (p_hsDocString Pipe True)
          p_hsmodName name
        breakpoint
        forM_ hsmodDeprecMessage $ \w -> do
          located' p_moduleWarning w
          breakpoint
        case hsmodExports of
          Nothing -> return ()
          Just l -> do
            located l $ \exports -> do
              inci (p_hsmodExports exports)
            breakpoint
        txt "where"
        newline
    newline
    forM_ (normalizeImports hsmodImports) (located' (p_hsmodImport qualifiedPost))
    newline
    switchLayout (getLoc <$> hsmodDecls) $ do
      p_hsDecls Free hsmodDecls
      newline
      spitRemainingComments
