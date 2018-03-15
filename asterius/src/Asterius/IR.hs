{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Asterius.IR
  ( ALabel(..)
  , ALocalLabel(..)
  , ALit(..)
  , AStatic(..)
  , ADataSection(..)
  , AType(..)
  , AReg(..)
  , AUnOp(..)
  , ABinOp(..)
  , AExpr(..)
  , AInstr(..)
  , ABranchInstr(..)
  , ABasicBlock(..)
  , ACodeSection(..)
  , AModule(..)
  , toAModule
  ) where

import CLabel
import Cmm
import CmmSwitch
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Short as SBS
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import Data.Serialize
import Data.Serialize.Cereal.Orphans ()
import DynFlags
import Foreign
import Foreign.C
import GHC.Generics (Generic)
import Hoopl.Block
import Hoopl.Collections
import Hoopl.Graph
import Hoopl.Label
import Language.Haskell.GHC.Toolkit.IROrphans ()
import Outputable hiding ((<>))
import Unique

newtype ALabel = ALabelEncoded
  { unALabel :: SBS.ShortByteString
  } deriving (Eq, Hashable, Show, Generic, Serialize)

toALabel :: DynFlags -> CLabel -> ALabel
toALabel dflags =
  ALabelEncoded .
  SBS.toShort . BS.pack . showSDoc dflags . pprCode AsmStyle . ppr

newtype ALocalLabel = ALocalLabel
  { unALocalLabel :: Int
  } deriving (Eq, Hashable, Show, Generic, Serialize)

toALocalLabel :: Label -> ALocalLabel
toALocalLabel = ALocalLabel . getKey . getUnique

data ALit
  = AInt8 Int8
  | AInt16 Int16
  | AInt32 Int32
  | AInt64 Int64
  | AFloat32 CFloat
  | AFloat64 CDouble
  | ALabel ALabel
  | ALabelOff ALabel
              Int
  | ABlock ALocalLabel
  deriving (Show, Generic)

instance Serialize ALit

toALit :: DynFlags -> CmmLit -> ALit
toALit _ (CmmInt v W8) = AInt8 (fromIntegral v)
toALit _ (CmmInt v W16) = AInt16 (fromIntegral v)
toALit _ (CmmInt v W32) = AInt32 (fromIntegral v)
toALit _ (CmmInt v W64) = AInt64 (fromIntegral v)
toALit _ (CmmFloat v W32) = AFloat32 (fromRational v)
toALit _ (CmmFloat v W64) = AFloat64 (fromRational v)
toALit dflags (CmmLabel l) = ALabel (toALabel dflags l)
toALit dflags (CmmLabelOff l o) = ALabelOff (toALabel dflags l) o
toALit _ (CmmBlock i) = ABlock (toALocalLabel i)
toALit _ l = error $ "Invalid CmmLit variant: " ++ show l

data AStatic
  = AStaticLit ALit
  | AString SBS.ShortByteString
  deriving (Show, Generic)

instance Serialize AStatic

toAStatic :: DynFlags -> CmmStatic -> AStatic
toAStatic dflags (CmmStaticLit l) = AStaticLit (toALit dflags l)
toAStatic _ (CmmString s) = AString (SBS.pack s)
toAStatic _ s = error $ "Invalid CmmStatic variant: " ++ show s

data ADataSection = ADataSection
  { align :: Int
  , statics :: [AStatic]
  } deriving (Show, Generic)

instance Serialize ADataSection

data AType =
  ATypeStub
  deriving (Show, Generic)

instance Serialize AType

toAType :: CmmType -> AType
toAType _ = ATypeStub

data AReg =
  ARegStub
  deriving (Show, Generic)

instance Serialize AReg

toAReg :: CmmReg -> AReg
toAReg _ = ARegStub

data AUnOp
  = ANegInt32
  | ANegInt64
  | ANegFloat32
  | ANegFloat64
  | ANotInt32
  | ANotInt64
  | AConvInt32ToFloat32
  | AConvInt32ToFloat64
  | AConvInt64ToFloat32
  | AConvInt64ToFloat64
  | AConvFloat32ToInt32
  | AConvFloat32ToInt64
  | AConvFloat64ToInt32
  | AConvFloat64ToInt64
  | AConvSInt8ToSInt64
  | AConvSInt16ToSInt64
  | AConvSInt32ToSInt64
  | AConvSInt64ToSInt8
  | AConvSInt64ToSInt16
  | AConvSInt64ToSInt32
  | AConvUInt8ToUInt64
  | AConvUInt16ToUInt64
  | AConvUInt32ToUInt64
  | AConvUInt64ToUInt8
  | AConvUInt64ToUInt16
  | AConvUInt64ToUInt32
  | AConvFloat32ToFloat64
  | AConvFloat64ToFloat32
  deriving (Show, Generic)

instance Serialize AUnOp

data ABinOp
  = AAddInt32
  | AAddInt64
  | ASubInt32
  | ASubInt64
  | AEqInt8
  | AEqInt16
  | AEqInt32
  | AEqInt64
  | ANeInt32
  | ANeInt64
  | AMulInt32
  | AMulInt64
  | AMulSInt64MayOflo
  | ADivSInt32
  | ADivSInt64
  | ARemSInt32
  | ARemSInt64
  | AMulUInt64MayOflo
  | ADivUInt32
  | ADivUInt64
  | ARemUInt32
  | ARemUInt64
  | AGeSInt32
  | AGeSInt64
  | ALeSInt32
  | ALeSInt64
  | AGtSInt32
  | AGtSInt64
  | ALtSInt32
  | ALtSInt64
  | AGeUInt32
  | AGeUInt64
  | ALeUInt32
  | ALeUInt64
  | AGtUInt32
  | AGtUInt64
  | ALtUInt32
  | ALtUInt64
  | AAddFloat32
  | AAddFloat64
  | ASubFloat32
  | ASubFloat64
  | AMulFloat32
  | AMulFloat64
  | ADivFloat32
  | ADivFloat64
  | AEqFloat32
  | AEqFloat64
  | ANeFloat32
  | ANeFloat64
  | AGeFloat32
  | AGeFloat64
  | ALeFloat32
  | ALeFloat64
  | AGtFloat32
  | AGtFloat64
  | ALtFloat32
  | ALtFloat64
  | AAndInt32
  | AAndInt64
  | AOrInt32
  | AOrInt64
  | AXorInt32
  | AXorInt64
  | AShlInt32
  | AShlInt64
  | AShrUInt32
  | AShrUInt64
  | AShrSInt32
  | AShrSInt64
  deriving (Show, Generic)

instance Serialize ABinOp

data AExpr
  = ALit ALit
  | ALoad AExpr
          AType
  | AReg AReg
  | ARegOff AReg
            Int
  | AUnOp AUnOp
          AExpr
  | ABinOp ABinOp
           AExpr
           AExpr
  deriving (Show, Generic)

instance Serialize AExpr

toAExpr :: DynFlags -> CmmExpr -> AExpr
toAExpr dflags (CmmLit l) = ALit (toALit dflags l)
toAExpr dflags (CmmLoad e t) = ALoad (toAExpr dflags e) (toAType t)
toAExpr _ (CmmReg r) = AReg (toAReg r)
toAExpr _ (CmmRegOff r o) = ARegOff (toAReg r) o
toAExpr dflags (CmmMachOp op [x]) =
  AUnOp
    (case op of
       MO_S_Neg W32 -> ANegInt32
       MO_S_Neg W64 -> ANegInt64
       MO_F_Neg W32 -> ANegFloat32
       MO_F_Neg W64 -> ANegFloat64
       MO_Not W32 -> ANotInt32
       MO_Not W64 -> ANotInt64
       MO_SF_Conv W32 W32 -> AConvInt32ToFloat32
       MO_SF_Conv W32 W64 -> AConvInt32ToFloat64
       MO_SF_Conv W64 W32 -> AConvInt64ToFloat32
       MO_SF_Conv W64 W64 -> AConvInt64ToFloat64
       MO_FS_Conv W32 W32 -> AConvFloat32ToInt32
       MO_FS_Conv W32 W64 -> AConvFloat32ToInt64
       MO_FS_Conv W64 W32 -> AConvFloat64ToInt32
       MO_FS_Conv W64 W64 -> AConvFloat64ToInt64
       MO_SS_Conv W8 W64 -> AConvSInt8ToSInt64
       MO_SS_Conv W16 W64 -> AConvSInt16ToSInt64
       MO_SS_Conv W32 W64 -> AConvSInt32ToSInt64
       MO_SS_Conv W64 W8 -> AConvSInt64ToSInt8
       MO_SS_Conv W64 W16 -> AConvSInt64ToSInt16
       MO_SS_Conv W64 W32 -> AConvSInt64ToSInt32
       MO_UU_Conv W8 W64 -> AConvUInt8ToUInt64
       MO_UU_Conv W16 W64 -> AConvUInt16ToUInt64
       MO_UU_Conv W32 W64 -> AConvUInt32ToUInt64
       MO_UU_Conv W64 W8 -> AConvUInt64ToUInt8
       MO_UU_Conv W64 W16 -> AConvUInt64ToUInt16
       MO_UU_Conv W64 W32 -> AConvUInt64ToUInt32
       MO_FF_Conv W32 W64 -> AConvFloat32ToFloat64
       MO_FF_Conv W64 W32 -> AConvFloat64ToFloat32
       _ -> error $ "Invalid CmmExpr UnOp: " ++ show op)
    (toAExpr dflags x)
toAExpr dflags (CmmMachOp op [x, y]) =
  ABinOp
    (case op of
       MO_Add W32 -> AAddInt32
       MO_Add W64 -> AAddInt64
       MO_Sub W32 -> ASubInt32
       MO_Sub W64 -> ASubInt64
       MO_Eq W8 -> AEqInt8
       MO_Eq W16 -> AEqInt16
       MO_Eq W32 -> AEqInt32
       MO_Eq W64 -> AEqInt64
       MO_Ne W32 -> ANeInt32
       MO_Ne W64 -> ANeInt64
       MO_Mul W32 -> AMulInt32
       MO_Mul W64 -> AMulInt64
       MO_S_MulMayOflo W64 -> AMulSInt64MayOflo
       MO_S_Quot W32 -> ADivSInt32
       MO_S_Quot W64 -> ADivSInt64
       MO_S_Rem W32 -> ARemSInt32
       MO_S_Rem W64 -> ARemSInt64
       MO_U_MulMayOflo W64 -> AMulUInt64MayOflo
       MO_U_Quot W32 -> ADivUInt32
       MO_U_Quot W64 -> ADivUInt64
       MO_U_Rem W32 -> ARemUInt32
       MO_U_Rem W64 -> ARemUInt64
       MO_S_Ge W32 -> AGeSInt32
       MO_S_Ge W64 -> AGeSInt64
       MO_S_Le W32 -> ALeSInt32
       MO_S_Le W64 -> ALeSInt64
       MO_S_Gt W32 -> AGtSInt32
       MO_S_Gt W64 -> AGtSInt64
       MO_S_Lt W32 -> ALtSInt32
       MO_S_Lt W64 -> ALtSInt64
       MO_U_Ge W32 -> AGeUInt32
       MO_U_Ge W64 -> AGeUInt64
       MO_U_Le W32 -> ALeUInt32
       MO_U_Le W64 -> ALeUInt64
       MO_U_Gt W32 -> AGtUInt32
       MO_U_Gt W64 -> AGtUInt64
       MO_U_Lt W32 -> ALtUInt32
       MO_U_Lt W64 -> ALtUInt64
       MO_F_Add W32 -> AAddFloat32
       MO_F_Add W64 -> AAddFloat64
       MO_F_Sub W32 -> ASubFloat32
       MO_F_Sub W64 -> ASubFloat64
       MO_F_Mul W32 -> AMulFloat32
       MO_F_Mul W64 -> AMulFloat64
       MO_F_Quot W32 -> ADivFloat32
       MO_F_Quot W64 -> ADivFloat64
       MO_F_Eq W32 -> AEqFloat32
       MO_F_Eq W64 -> AEqFloat64
       MO_F_Ne W32 -> ANeFloat32
       MO_F_Ne W64 -> ANeFloat64
       MO_F_Ge W32 -> AGeFloat32
       MO_F_Ge W64 -> AGeFloat64
       MO_F_Le W32 -> ALeFloat32
       MO_F_Le W64 -> ALeFloat64
       MO_F_Gt W32 -> AGtFloat32
       MO_F_Gt W64 -> AGtFloat64
       MO_F_Lt W32 -> ALtFloat32
       MO_F_Lt W64 -> ALtFloat64
       MO_And W32 -> AAndInt32
       MO_And W64 -> AAndInt64
       MO_Or W32 -> AOrInt32
       MO_Or W64 -> AOrInt64
       MO_Xor W32 -> AXorInt32
       MO_Xor W64 -> AXorInt64
       MO_Shl W32 -> AShlInt32
       MO_Shl W64 -> AShlInt64
       MO_U_Shr W32 -> AShrUInt32
       MO_U_Shr W64 -> AShrUInt64
       MO_S_Shr W32 -> AShrSInt32
       MO_S_Shr W64 -> AShrSInt64
       _ -> error $ "Invalid CmmExpr BinOp: " ++ show op)
    (toAExpr dflags x)
    (toAExpr dflags y)
toAExpr _ e = error $ "Invalid CmmExpr variant: " ++ show e

data AInstr
  = AAssign AReg
            AExpr
  | AStore AExpr
           AExpr
  | AInstrStub
  deriving (Show, Generic)

instance Serialize AInstr

toAInstr :: DynFlags -> CmmNode O O -> AInstr
toAInstr dflags (CmmAssign r e) = AAssign (toAReg r) (toAExpr dflags e)
toAInstr dflags (CmmStore l e) = AStore (toAExpr dflags l) (toAExpr dflags e)
toAInstr _ _ = AInstrStub

data ABranchInstr
  = ABranch ALocalLabel
  | ACondBranch AExpr
                ALocalLabel
                ALocalLabel
  | ASwitch AExpr
            (HM.HashMap Int64 ALocalLabel)
            (Maybe ALocalLabel)
            Bool
  | ABranchInstrStub
  deriving (Show, Generic)

instance Serialize ABranchInstr

toABranchInstr :: DynFlags -> CmmNode O C -> ABranchInstr
toABranchInstr _ (CmmBranch l) = ABranch (toALocalLabel l)
toABranchInstr dflags (CmmCondBranch c t f _) =
  ACondBranch (toAExpr dflags c) (toALocalLabel t) (toALocalLabel f)
toABranchInstr dflags (CmmSwitch e m) =
  ASwitch
    (toAExpr dflags e)
    (HM.fromList
       [(fromIntegral k, toALocalLabel v) | (k, v) <- switchTargetsCases m])
    (fmap toALocalLabel (switchTargetsDefault m))
    (switchTargetsSigned m)
toABranchInstr _ _ = ABranchInstrStub

data ABasicBlock = ABasicBlock
  { instrs :: [AInstr]
  , branch :: ABranchInstr
  } deriving (Show, Generic)

instance Serialize ABasicBlock

data ACodeSection = ACodeSection
  { entryLocalLabel :: ALocalLabel
  , basicBlocks :: HM.HashMap ALocalLabel ABasicBlock
  } deriving (Show, Generic)

instance Serialize ACodeSection

data AModule = AModule
  { dataSections :: HM.HashMap ALabel ADataSection
  , codeSections :: HM.HashMap ALabel ACodeSection
  } deriving (Show, Generic)

instance Serialize AModule

instance Semigroup AModule where
  am0 <> am1 =
    AModule
      { dataSections = dataSections am0 <> dataSections am1
      , codeSections = codeSections am0 <> codeSections am1
      }

instance Monoid AModule where
  mempty = AModule {dataSections = mempty, codeSections = mempty}

toAModule :: DynFlags -> GenCmmDecl CmmStatics h CmmGraph -> AModule
toAModule dflags (CmmData (Section t l) (Statics _ ss)) =
  let (k, v) =
        ( toALabel dflags l
        , ADataSection
            (case t of
               CString -> 1
               _ -> 8)
            (map (toAStatic dflags) ss))
   in mempty {dataSections = HM.singleton k v}
toAModule dflags (CmmProc _ l _ CmmGraph {..}) =
  case g_graph of
    GMany NothingO m NothingO ->
      let lm =
            fmap
              (\b ->
                 case blockSplit b of
                   (_, b', e) -> (blockToList b', e))
              m
       in mempty
            { codeSections =
                HM.singleton
                  (toALabel dflags l)
                  ACodeSection
                    { entryLocalLabel = toALocalLabel g_entry
                    , basicBlocks =
                        HM.fromList
                          [ ( toALocalLabel k
                            , ABasicBlock
                                { instrs = map (toAInstr dflags) is
                                , branch = toABranchInstr dflags br
                                })
                          | (k, (is, br)) <- mapToList lm
                          ]
                    }
            }
