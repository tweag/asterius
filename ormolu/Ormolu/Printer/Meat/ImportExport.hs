{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Rendering of import and export lists.
module Ormolu.Printer.Meat.ImportExport
  ( p_hsmodExports,
    p_hsmodImport,
  )
where

import Control.Monad
import GHC
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common
import Ormolu.Utils (RelativePos (..), attachRelativePos)

p_hsmodExports :: [LIE GhcPs] -> R ()
p_hsmodExports [] = do
  txt "("
  breakpoint'
  txt ")"
p_hsmodExports xs =
  parens N $ do
    layout <- getLayout
    sep
      breakpoint
      (\(p, l) -> sitcc (located l (p_lie layout p)))
      (attachRelativePos xs)

p_hsmodImport :: Bool -> ImportDecl GhcPs -> R ()
p_hsmodImport useQualifiedPost ImportDecl {..} = do
  txt "import"
  space
  when ideclSource (txt "{-# SOURCE #-}")
  space
  when ideclSafe (txt "safe")
  space
  when
    (isImportDeclQualified ideclQualified && not useQualifiedPost)
    (txt "qualified")
  space
  case ideclPkgQual of
    Nothing -> return ()
    Just slit -> atom slit
  space
  inci $ do
    located ideclName atom
    when
      (isImportDeclQualified ideclQualified && useQualifiedPost)
      (space >> txt "qualified")
    case ideclAs of
      Nothing -> return ()
      Just l -> do
        space
        txt "as"
        space
        located l atom
    space
    case ideclHiding of
      Nothing -> return ()
      Just (hiding, _) ->
        when hiding (txt "hiding")
    case ideclHiding of
      Nothing -> return ()
      Just (_, L _ xs) -> do
        breakpoint
        parens N $ do
          layout <- getLayout
          sep
            breakpoint
            (\(p, l) -> sitcc (located l (p_lie layout p)))
            (attachRelativePos xs)
    newline
p_hsmodImport _ (XImportDecl x) = noExtCon x

p_lie :: Layout -> RelativePos -> IE GhcPs -> R ()
p_lie encLayout relativePos = \case
  IEVar NoExtField l1 -> do
    located l1 p_ieWrappedName
    p_comma
  IEThingAbs NoExtField l1 -> do
    located l1 p_ieWrappedName
    p_comma
  IEThingAll NoExtField l1 -> do
    located l1 p_ieWrappedName
    space
    txt "(..)"
    p_comma
  IEThingWith NoExtField l1 w xs _ -> sitcc $ do
    located l1 p_ieWrappedName
    breakpoint
    inci $ do
      let names :: [R ()]
          names = located' p_ieWrappedName <$> xs
      parens N . sep commaDel sitcc $
        case w of
          NoIEWildcard -> names
          IEWildcard n ->
            let (before, after) = splitAt n names
             in before ++ [txt ".."] ++ after
    p_comma
  IEModuleContents NoExtField l1 -> do
    located l1 p_hsmodName
    p_comma
  IEGroup NoExtField n str -> do
    case relativePos of
      SinglePos -> return ()
      FirstPos -> return ()
      MiddlePos -> newline
      LastPos -> newline
    p_hsDocString (Asterisk n) False (noLoc str)
  IEDoc NoExtField str ->
    p_hsDocString Pipe False (noLoc str)
  IEDocNamed NoExtField str -> p_hsDocName str
  XIE x -> noExtCon x
  where
    p_comma =
      case encLayout of
        SingleLine ->
          case relativePos of
            SinglePos -> return ()
            FirstPos -> comma
            MiddlePos -> comma
            LastPos -> return ()
        MultiLine -> comma
