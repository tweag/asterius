{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module helps handle operator chains composed of different
-- operators that may have different precedence and fixities.
module Ormolu.Printer.Operators
  ( OpTree (..),
    opTreeLoc,
    reassociateOpTree,
  )
where

import Data.Function (on)
import qualified Data.List as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import GHC
import OccName (occNameString)
import Ormolu.Utils (unSrcSpan)

-- | Intermediate representation of operator trees. It has two type
-- parameters: @ty@ is the type of sub-expressions, while @op@ is the type
-- of operators.
data OpTree ty op
  = OpNode ty
  | OpBranch
      (OpTree ty op)
      op
      (OpTree ty op)

-- | Return combined 'SrcSpan's of all elements in this 'OpTree'.
opTreeLoc :: OpTree (Located a) b -> SrcSpan
opTreeLoc (OpNode (L l _)) = l
opTreeLoc (OpBranch l _ r) = combineSrcSpans (opTreeLoc l) (opTreeLoc r)

-- | Re-associate an 'OpTree' taking into account automagically inferred
-- relative precedence of operators. Users are expected to first construct
-- an initial 'OpTree', then re-associate it using this function before
-- printing.
reassociateOpTree ::
  -- | How to get name of an operator
  (op -> Maybe RdrName) ->
  -- | Original 'OpTree'
  OpTree (Located ty) (Located op) ->
  -- | Re-associated 'OpTree'
  OpTree (Located ty) (Located op)
reassociateOpTree getOpName opTree =
  reassociateOpTreeWith
    (buildFixityMap getOpName normOpTree)
    (getOpName . unLoc)
    normOpTree
  where
    normOpTree = normalizeOpTree opTree

-- | Re-associate an 'OpTree' given the map with operator fixities.
reassociateOpTreeWith ::
  forall ty op.
  -- | Fixity map for operators
  Map String Fixity ->
  -- | How to get the name of an operator
  (op -> Maybe RdrName) ->
  -- | Original 'OpTree'
  OpTree ty op ->
  -- | Re-associated 'OpTree'
  OpTree ty op
reassociateOpTreeWith fixityMap getOpName = go
  where
    fixityOf :: op -> Fixity
    fixityOf op = fromMaybe defaultFixity $ do
      s <- occNameString . rdrNameOcc <$> getOpName op
      M.lookup s fixityMap
    -- Here, left branch is already associated and the root alongside with
    -- the right branch is right-associated. This function picks up one item
    -- from the right and inserts it correctly to the left.
    --
    -- Also, we are using the 'compareFixity' function which tells if the
    -- expression should associate to right.
    go :: OpTree ty op -> OpTree ty op
    -- base cases
    go t@(OpNode _) = t
    go t@(OpBranch (OpNode _) _ (OpNode _)) = t
    -- shift one operator to the left at the beginning
    go (OpBranch l@(OpNode _) op (OpBranch l' op' r')) =
      go (OpBranch (OpBranch l op l') op' r')
    -- at the last operator, place the operator and don't recurse
    go (OpBranch (OpBranch l op r) op' r'@(OpNode _)) =
      if snd $ compareFixity (fixityOf op) (fixityOf op')
        then OpBranch l op (go $ OpBranch r op' r')
        else OpBranch (OpBranch l op r) op' r'
    -- else, shift one operator to left and recurse.
    go (OpBranch (OpBranch l op r) op' (OpBranch l' op'' r')) =
      if snd $ compareFixity (fixityOf op) (fixityOf op')
        then go $ OpBranch (OpBranch l op (go $ OpBranch r op' l')) op'' r'
        else go $ OpBranch (OpBranch (OpBranch l op r) op' l') op'' r'

-- | A score assigned to an operator.
data Score
  = -- | The operator was placed at the beginning of a line
    AtBeginning Int
  | -- | The operator was placed at the end of a line
    AtEnd
  | -- | The operator was placed in between arguments on a single line
    InBetween
  deriving (Eq, Ord)

-- | Build a map of inferred 'Fixity's from an 'OpTree'.
buildFixityMap ::
  forall ty op.
  -- | How to get the name of an operator
  (op -> Maybe RdrName) ->
  -- | Operator tree
  OpTree (Located ty) (Located op) ->
  -- | Fixity map
  Map String Fixity
buildFixityMap getOpName opTree =
  addOverrides
    . M.fromList
    . concatMap (\(i, ns) -> map (\(n, _) -> (n, fixity i InfixL)) ns)
    . zip [2 ..]
    . L.groupBy ((==) `on` snd)
    . selectScores
    $ score opTree
  where
    addOverrides :: Map String Fixity -> Map String Fixity
    addOverrides m =
      M.fromList
        [ ("$", fixity 0 InfixR),
          (":", fixity 1 InfixR),
          (".", fixity 100 InfixL)
        ]
        `M.union` m
    fixity = Fixity NoSourceText
    score :: OpTree (Located ty) (Located op) -> [(String, Score)]
    score (OpNode _) = []
    score (OpBranch l o r) = fromMaybe (score r) $ do
      -- If we fail to get any of these, 'defaultFixity' will be used by
      -- 'reassociateOpTreeWith'.
      le <- srcSpanEndLine <$> unSrcSpan (opTreeLoc l) -- left end
      ob <- srcSpanStartLine <$> unSrcSpan (getLoc o) -- operator begin
      oe <- srcSpanEndLine <$> unSrcSpan (getLoc o) -- operator end
      rb <- srcSpanStartLine <$> unSrcSpan (opTreeLoc r) -- right begin
      oc <- srcSpanStartCol <$> unSrcSpan (getLoc o) -- operator column
      opName <- occNameString . rdrNameOcc <$> getOpName (unLoc o)
      let s
            | le < ob = AtBeginning oc
            | oe < rb = AtEnd
            | otherwise = InBetween
      return $ (opName, s) : score r
    selectScores :: [(String, Score)] -> [(String, Score)]
    selectScores =
      L.sortOn snd
        . mapMaybe
          ( \case
              [] -> Nothing
              xs@((n, _) : _) -> Just (n, selectScore $ map snd xs)
          )
        . L.groupBy ((==) `on` fst)
        . L.sort
    selectScore :: [Score] -> Score
    selectScore xs =
      case filter (/= InBetween) xs of
        [] -> InBetween
        xs' -> maximum xs'

----------------------------------------------------------------------------
-- Helpers

-- | Convert an 'OpTree' to with all operators having the same fixity and
-- associativity (left infix).
normalizeOpTree :: OpTree ty op -> OpTree ty op
normalizeOpTree (OpNode n) =
  OpNode n
normalizeOpTree (OpBranch (OpNode l) lop r) =
  OpBranch (OpNode l) lop (normalizeOpTree r)
normalizeOpTree (OpBranch (OpBranch l' lop' r') lop r) =
  normalizeOpTree (OpBranch l' lop' (OpBranch r' lop r))
