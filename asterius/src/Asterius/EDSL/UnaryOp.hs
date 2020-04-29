
module Asterius.EDSL.UnaryOp where

import Asterius.Types

clzInt32 :: Expression -> Expression
clzInt32 = Unary ClzInt32

ctzInt32 :: Expression -> Expression
ctzInt32 = Unary CtzInt32

popcntInt32 :: Expression -> Expression
popcntInt32 = Unary PopcntInt32

negFloat32 :: Expression -> Expression
negFloat32 = Unary NegFloat32

absFloat32 :: Expression -> Expression
absFloat32 = Unary AbsFloat32

ceilFloat32 :: Expression -> Expression
ceilFloat32 = Unary CeilFloat32

floorFloat32 :: Expression -> Expression
floorFloat32 = Unary FloorFloat32

truncFloat32 :: Expression -> Expression
truncFloat32 = Unary TruncFloat32

nearestFloat32 :: Expression -> Expression
nearestFloat32 = Unary NearestFloat32

sqrtFloat32 :: Expression -> Expression
sqrtFloat32 = Unary SqrtFloat32

eqZInt32 :: Expression -> Expression
eqZInt32 = Unary EqZInt32

clzInt64 :: Expression -> Expression
clzInt64 = Unary ClzInt64

ctzInt64 :: Expression -> Expression
ctzInt64 = Unary CtzInt64

popcntInt64 :: Expression -> Expression
popcntInt64 = Unary PopcntInt64

negFloat64 :: Expression -> Expression
negFloat64 = Unary NegFloat64

absFloat64 :: Expression -> Expression
absFloat64 = Unary AbsFloat64

ceilFloat64 :: Expression -> Expression
ceilFloat64 = Unary CeilFloat64

floorFloat64 :: Expression -> Expression
floorFloat64 = Unary FloorFloat64

truncFloat64 :: Expression -> Expression
truncFloat64 = Unary TruncFloat64

nearestFloat64 :: Expression -> Expression
nearestFloat64 = Unary NearestFloat64

sqrtFloat64 :: Expression -> Expression
sqrtFloat64 = Unary SqrtFloat64

eqZInt64 :: Expression -> Expression
eqZInt64 = Unary EqZInt64

extendSInt32 :: Expression -> Expression
extendSInt32 = Unary ExtendSInt32

extendUInt32 :: Expression -> Expression
extendUInt32 = Unary ExtendUInt32

wrapInt64 :: Expression -> Expression
wrapInt64 = Unary WrapInt64

truncSFloat32ToInt32 :: Expression -> Expression
truncSFloat32ToInt32 = Unary TruncSFloat32ToInt32

truncSFloat32ToInt64 :: Expression -> Expression
truncSFloat32ToInt64 = Unary TruncSFloat32ToInt64

truncUFloat32ToInt32 :: Expression -> Expression
truncUFloat32ToInt32 = Unary TruncUFloat32ToInt32

truncUFloat32ToInt64 :: Expression -> Expression
truncUFloat32ToInt64 = Unary TruncUFloat32ToInt64

truncSFloat64ToInt32 :: Expression -> Expression
truncSFloat64ToInt32 = Unary TruncSFloat64ToInt32

truncSFloat64ToInt64 :: Expression -> Expression
truncSFloat64ToInt64 = Unary TruncSFloat64ToInt64

truncUFloat64ToInt32 :: Expression -> Expression
truncUFloat64ToInt32 = Unary TruncUFloat64ToInt32

truncUFloat64ToInt64 :: Expression -> Expression
truncUFloat64ToInt64 = Unary TruncUFloat64ToInt64

reinterpretFloat32 :: Expression -> Expression
reinterpretFloat32 = Unary ReinterpretFloat32

reinterpretFloat64 :: Expression -> Expression
reinterpretFloat64 = Unary ReinterpretFloat64

convertSInt32ToFloat32 :: Expression -> Expression
convertSInt32ToFloat32 = Unary ConvertSInt32ToFloat32

convertSInt32ToFloat64 :: Expression -> Expression
convertSInt32ToFloat64 = Unary ConvertSInt32ToFloat64

convertUInt32ToFloat32 :: Expression -> Expression
convertUInt32ToFloat32 = Unary ConvertUInt32ToFloat32

convertUInt32ToFloat64 :: Expression -> Expression
convertUInt32ToFloat64 = Unary ConvertUInt32ToFloat64

convertSInt64ToFloat32 :: Expression -> Expression
convertSInt64ToFloat32 = Unary ConvertSInt64ToFloat32

convertSInt64ToFloat64 :: Expression -> Expression
convertSInt64ToFloat64 = Unary ConvertSInt64ToFloat64

convertUInt64ToFloat32 :: Expression -> Expression
convertUInt64ToFloat32 = Unary ConvertUInt64ToFloat32

convertUInt64ToFloat64 :: Expression -> Expression
convertUInt64ToFloat64 = Unary ConvertUInt64ToFloat64

promoteFloat32 :: Expression -> Expression
promoteFloat32 = Unary PromoteFloat32

demoteFloat64 :: Expression -> Expression
demoteFloat64 = Unary DemoteFloat64

reinterpretInt32 :: Expression -> Expression
reinterpretInt32 = Unary ReinterpretInt32

reinterpretInt64 :: Expression -> Expression
reinterpretInt64 = Unary ReinterpretInt64
