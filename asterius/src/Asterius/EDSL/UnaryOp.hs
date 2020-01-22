{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Asterius.EDSL.UnaryOp where

import Asterius.Types

clzInt32 = Unary ClzInt32

ctzInt32 = Unary CtzInt32

popcntInt32 = Unary PopcntInt32

negFloat32 = Unary NegFloat32

absFloat32 = Unary AbsFloat32

ceilFloat32 = Unary CeilFloat32

floorFloat32 = Unary FloorFloat32

truncFloat32 = Unary TruncFloat32

nearestFloat32 = Unary NearestFloat32

sqrtFloat32 = Unary SqrtFloat32

eqZInt32 = Unary EqZInt32

clzInt64 = Unary ClzInt64

ctzInt64 = Unary CtzInt64

popcntInt64 = Unary PopcntInt64

negFloat64 = Unary NegFloat64

absFloat64 = Unary AbsFloat64

ceilFloat64 = Unary CeilFloat64

floorFloat64 = Unary FloorFloat64

truncFloat64 = Unary TruncFloat64

nearestFloat64 = Unary NearestFloat64

sqrtFloat64 = Unary SqrtFloat64

eqZInt64 = Unary EqZInt64

extendSInt32 = Unary ExtendSInt32

extendUInt32 = Unary ExtendUInt32

wrapInt64 = Unary WrapInt64

truncSFloat32ToInt32 = Unary TruncSFloat32ToInt32

truncSFloat32ToInt64 = Unary TruncSFloat32ToInt64

truncUFloat32ToInt32 = Unary TruncUFloat32ToInt32

truncUFloat32ToInt64 = Unary TruncUFloat32ToInt64

truncSFloat64ToInt32 = Unary TruncSFloat64ToInt32

truncSFloat64ToInt64 = Unary TruncSFloat64ToInt64

truncUFloat64ToInt32 = Unary TruncUFloat64ToInt32

truncUFloat64ToInt64 = Unary TruncUFloat64ToInt64

reinterpretFloat32 = Unary ReinterpretFloat32

reinterpretFloat64 = Unary ReinterpretFloat64

convertSInt32ToFloat32 = Unary ConvertSInt32ToFloat32

convertSInt32ToFloat64 = Unary ConvertSInt32ToFloat64

convertUInt32ToFloat32 = Unary ConvertUInt32ToFloat32

convertUInt32ToFloat64 = Unary ConvertUInt32ToFloat64

convertSInt64ToFloat32 = Unary ConvertSInt64ToFloat32

convertSInt64ToFloat64 = Unary ConvertSInt64ToFloat64

convertUInt64ToFloat32 = Unary ConvertUInt64ToFloat32

convertUInt64ToFloat64 = Unary ConvertUInt64ToFloat64

promoteFloat32 = Unary PromoteFloat32

demoteFloat64 = Unary DemoteFloat64

reinterpretInt32 = Unary ReinterpretInt32

reinterpretInt64 = Unary ReinterpretInt64
