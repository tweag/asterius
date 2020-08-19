{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- | Manipulations on import lists.
module Ormolu.Imports
  ( normalizeImports,
  )
where

import Data.Bifunctor
import Data.Char (isAlphaNum)
import Data.Function (on)
import Data.List (foldl', nubBy, sortBy, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import FastString (FastString)
import GHC hiding (GhcPs, IE)
import GHC.Hs.Extension
import GHC.Hs.ImpExp (IE (..))
import Ormolu.Utils (notImplemented, showOutputable)

-- | Sort and normalize imports.
normalizeImports :: [LImportDecl GhcPs] -> [LImportDecl GhcPs]
normalizeImports =
  fmap snd
    . M.toAscList
    . M.fromListWith combineImports
    . fmap (\x -> (importId x, g x))
  where
    g (L l ImportDecl {..}) =
      L
        l
        ImportDecl
          { ideclHiding = second (fmap normalizeLies) <$> ideclHiding,
            ..
          }
    g _ = notImplemented "XImportDecl"

-- | Combine two import declarations. It should be assumed that 'ImportId's
-- are equal.
combineImports ::
  LImportDecl GhcPs ->
  LImportDecl GhcPs ->
  LImportDecl GhcPs
combineImports (L lx ImportDecl {..}) (L _ y) =
  L
    lx
    ImportDecl
      { ideclHiding = case (ideclHiding, GHC.ideclHiding y) of
          (Just (hiding, L l' xs), Just (_, L _ ys)) ->
            Just (hiding, (L l' (normalizeLies (xs ++ ys))))
          _ -> Nothing,
        ..
      }
combineImports _ _ = notImplemented "XImportDecl"

-- | Import id, a collection of all things that justify having a separate
-- import entry. This is used for merging of imports. If two imports have
-- the same 'ImportId' they can be merged.
data ImportId = ImportId
  { importIsPrelude :: Bool,
    importIdName :: ModuleName,
    importPkgQual :: Maybe FastString,
    importSource :: Bool,
    importSafe :: Bool,
    importQualified :: Bool,
    importImplicit :: Bool,
    importAs :: Maybe ModuleName,
    importHiding :: Maybe Bool
  }
  deriving (Eq, Ord)

-- | Obtain an 'ImportId' for a given import.
importId :: LImportDecl GhcPs -> ImportId
importId (L _ ImportDecl {..}) =
  ImportId
    { importIsPrelude = isPrelude,
      importIdName = moduleName,
      importPkgQual = sl_fs <$> ideclPkgQual,
      importSource = ideclSource,
      importSafe = ideclSafe,
      importQualified = case ideclQualified of
        QualifiedPre -> True
        QualifiedPost -> True
        NotQualified -> False,
      importImplicit = ideclImplicit,
      importAs = unLoc <$> ideclAs,
      importHiding = fst <$> ideclHiding
    }
  where
    isPrelude = moduleNameString moduleName == "Prelude"
    moduleName = unLoc ideclName
importId _ = notImplemented "XImportDecl"

-- | Normalize a collection of import\/export items.
normalizeLies :: [LIE GhcPs] -> [LIE GhcPs]
normalizeLies = sortOn (getIewn . unLoc) . M.elems . foldl' combine M.empty
  where
    combine ::
      Map IEWrappedNameOrd (LIE GhcPs) ->
      LIE GhcPs ->
      Map IEWrappedNameOrd (LIE GhcPs)
    combine m (L new_l new) =
      let wname = getIewn new
          normalizeWNames =
            nubBy (\x y -> compareLIewn x y == EQ) . sortBy compareLIewn
          alter = \case
            Nothing -> Just . L new_l $
              case new of
                IEThingWith NoExtField n wildcard g flbl ->
                  IEThingWith NoExtField n wildcard (normalizeWNames g) flbl
                other -> other
            Just old ->
              let f = \case
                    IEVar NoExtField n -> IEVar NoExtField n
                    IEThingAbs NoExtField _ -> new
                    IEThingAll NoExtField n -> IEThingAll NoExtField n
                    IEThingWith NoExtField n wildcard g flbl ->
                      case new of
                        IEVar NoExtField _ ->
                          error "Ormolu.Imports broken presupposition"
                        IEThingAbs NoExtField _ ->
                          IEThingWith NoExtField n wildcard g flbl
                        IEThingAll NoExtField n' ->
                          IEThingAll NoExtField n'
                        IEThingWith NoExtField n' wildcard' g' flbl' ->
                          let combinedWildcard =
                                case (wildcard, wildcard') of
                                  (IEWildcard _, _) -> IEWildcard 0
                                  (_, IEWildcard _) -> IEWildcard 0
                                  _ -> NoIEWildcard
                           in IEThingWith
                                NoExtField
                                n'
                                combinedWildcard
                                (normalizeWNames (g <> g'))
                                flbl'
                        IEModuleContents NoExtField _ -> notImplemented "IEModuleContents"
                        IEGroup NoExtField _ _ -> notImplemented "IEGroup"
                        IEDoc NoExtField _ -> notImplemented "IEDoc"
                        IEDocNamed NoExtField _ -> notImplemented "IEDocNamed"
                        XIE x -> noExtCon x
                    IEModuleContents NoExtField _ -> notImplemented "IEModuleContents"
                    IEGroup NoExtField _ _ -> notImplemented "IEGroup"
                    IEDoc NoExtField _ -> notImplemented "IEDoc"
                    IEDocNamed NoExtField _ -> notImplemented "IEDocNamed"
                    XIE x -> noExtCon x
               in Just (f <$> old)
       in M.alter alter wname m

-- | A wrapper for @'IEWrappedName' 'RdrName'@ that allows us to define an
-- 'Ord' instance for it.
newtype IEWrappedNameOrd = IEWrappedNameOrd (IEWrappedName RdrName)
  deriving (Eq)

instance Ord IEWrappedNameOrd where
  compare (IEWrappedNameOrd x) (IEWrappedNameOrd y) = compareIewn x y

-- | Project @'IEWrappedName' 'RdrName'@ from @'IE' 'GhcPs'@.
getIewn :: IE GhcPs -> IEWrappedNameOrd
getIewn = \case
  IEVar NoExtField x -> IEWrappedNameOrd (unLoc x)
  IEThingAbs NoExtField x -> IEWrappedNameOrd (unLoc x)
  IEThingAll NoExtField x -> IEWrappedNameOrd (unLoc x)
  IEThingWith NoExtField x _ _ _ -> IEWrappedNameOrd (unLoc x)
  IEModuleContents NoExtField _ -> notImplemented "IEModuleContents"
  IEGroup NoExtField _ _ -> notImplemented "IEGroup"
  IEDoc NoExtField _ -> notImplemented "IEDoc"
  IEDocNamed NoExtField _ -> notImplemented "IEDocNamed"
  XIE x -> noExtCon x

-- | Like 'compareIewn' for located wrapped names.
compareLIewn :: LIEWrappedName RdrName -> LIEWrappedName RdrName -> Ordering
compareLIewn = compareIewn `on` unLoc

-- | Compare two @'IEWrapppedName' 'RdrName'@ things.
compareIewn :: IEWrappedName RdrName -> IEWrappedName RdrName -> Ordering
compareIewn (IEName x) (IEName y) = unLoc x `compareRdrName` unLoc y
compareIewn (IEName _) (IEPattern _) = LT
compareIewn (IEName _) (IEType _) = LT
compareIewn (IEPattern _) (IEName _) = GT
compareIewn (IEPattern x) (IEPattern y) = unLoc x `compareRdrName` unLoc y
compareIewn (IEPattern _) (IEType _) = LT
compareIewn (IEType _) (IEName _) = GT
compareIewn (IEType _) (IEPattern _) = GT
compareIewn (IEType x) (IEType y) = unLoc x `compareRdrName` unLoc y

compareRdrName :: RdrName -> RdrName -> Ordering
compareRdrName x y =
  case (getNameStr x, getNameStr y) of
    ([], []) -> EQ
    ((_ : _), []) -> GT
    ([], (_ : _)) -> LT
    ((x' : _), (y' : _)) ->
      case (isAlphaNum x', isAlphaNum y') of
        (False, False) -> x `compare` y
        (True, False) -> LT
        (False, True) -> GT
        (True, True) -> x `compare` y
  where
    getNameStr = showOutputable . rdrNameOcc
