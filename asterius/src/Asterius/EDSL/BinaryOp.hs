
module Asterius.EDSL.BinaryOp where

import Asterius.Types

addInt32 :: Expression -> Expression -> Expression
addInt32 = Binary AddInt32

subInt32 :: Expression -> Expression -> Expression
subInt32 = Binary SubInt32

mulInt32 :: Expression -> Expression -> Expression
mulInt32 = Binary MulInt32

divSInt32 :: Expression -> Expression -> Expression
divSInt32 = Binary DivSInt32

divUInt32 :: Expression -> Expression -> Expression
divUInt32 = Binary DivUInt32

remSInt32 :: Expression -> Expression -> Expression
remSInt32 = Binary RemSInt32

remUInt32 :: Expression -> Expression -> Expression
remUInt32 = Binary RemUInt32

andInt32 :: Expression -> Expression -> Expression
andInt32 = Binary AndInt32

orInt32 :: Expression -> Expression -> Expression
orInt32 = Binary OrInt32

xorInt32 :: Expression -> Expression -> Expression
xorInt32 = Binary XorInt32

shlInt32 :: Expression -> Expression -> Expression
shlInt32 = Binary ShlInt32

shrUInt32 :: Expression -> Expression -> Expression
shrUInt32 = Binary ShrUInt32

shrSInt32 :: Expression -> Expression -> Expression
shrSInt32 = Binary ShrSInt32

rotLInt32 :: Expression -> Expression -> Expression
rotLInt32 = Binary RotLInt32

rotRInt32 :: Expression -> Expression -> Expression
rotRInt32 = Binary RotRInt32

eqInt32 :: Expression -> Expression -> Expression
eqInt32 = Binary EqInt32

neInt32 :: Expression -> Expression -> Expression
neInt32 = Binary NeInt32

ltSInt32 :: Expression -> Expression -> Expression
ltSInt32 = Binary LtSInt32

ltUInt32 :: Expression -> Expression -> Expression
ltUInt32 = Binary LtUInt32

leSInt32 :: Expression -> Expression -> Expression
leSInt32 = Binary LeSInt32

leUInt32 :: Expression -> Expression -> Expression
leUInt32 = Binary LeUInt32

gtSInt32 :: Expression -> Expression -> Expression
gtSInt32 = Binary GtSInt32

gtUInt32 :: Expression -> Expression -> Expression
gtUInt32 = Binary GtUInt32

geSInt32 :: Expression -> Expression -> Expression
geSInt32 = Binary GeSInt32

geUInt32 :: Expression -> Expression -> Expression
geUInt32 = Binary GeUInt32

addInt64 :: Expression -> Expression -> Expression
addInt64 = Binary AddInt64

subInt64 :: Expression -> Expression -> Expression
subInt64 = Binary SubInt64

mulInt64 :: Expression -> Expression -> Expression
mulInt64 = Binary MulInt64

divSInt64 :: Expression -> Expression -> Expression
divSInt64 = Binary DivSInt64

divUInt64 :: Expression -> Expression -> Expression
divUInt64 = Binary DivUInt64

remSInt64 :: Expression -> Expression -> Expression
remSInt64 = Binary RemSInt64

remUInt64 :: Expression -> Expression -> Expression
remUInt64 = Binary RemUInt64

andInt64 :: Expression -> Expression -> Expression
andInt64 = Binary AndInt64

orInt64 :: Expression -> Expression -> Expression
orInt64 = Binary OrInt64

xorInt64 :: Expression -> Expression -> Expression
xorInt64 = Binary XorInt64

shlInt64 :: Expression -> Expression -> Expression
shlInt64 = Binary ShlInt64

shrUInt64 :: Expression -> Expression -> Expression
shrUInt64 = Binary ShrUInt64

shrSInt64 :: Expression -> Expression -> Expression
shrSInt64 = Binary ShrSInt64

rotLInt64 :: Expression -> Expression -> Expression
rotLInt64 = Binary RotLInt64

rotRInt64 :: Expression -> Expression -> Expression
rotRInt64 = Binary RotRInt64

eqInt64 :: Expression -> Expression -> Expression
eqInt64 = Binary EqInt64

neInt64 :: Expression -> Expression -> Expression
neInt64 = Binary NeInt64

ltSInt64 :: Expression -> Expression -> Expression
ltSInt64 = Binary LtSInt64

ltUInt64 :: Expression -> Expression -> Expression
ltUInt64 = Binary LtUInt64

leSInt64 :: Expression -> Expression -> Expression
leSInt64 = Binary LeSInt64

leUInt64 :: Expression -> Expression -> Expression
leUInt64 = Binary LeUInt64

gtSInt64 :: Expression -> Expression -> Expression
gtSInt64 = Binary GtSInt64

gtUInt64 :: Expression -> Expression -> Expression
gtUInt64 = Binary GtUInt64

geSInt64 :: Expression -> Expression -> Expression
geSInt64 = Binary GeSInt64

geUInt64 :: Expression -> Expression -> Expression
geUInt64 = Binary GeUInt64

addFloat32 :: Expression -> Expression -> Expression
addFloat32 = Binary AddFloat32

subFloat32 :: Expression -> Expression -> Expression
subFloat32 = Binary SubFloat32

mulFloat32 :: Expression -> Expression -> Expression
mulFloat32 = Binary MulFloat32

divFloat32 :: Expression -> Expression -> Expression
divFloat32 = Binary DivFloat32

copySignFloat32 :: Expression -> Expression -> Expression
copySignFloat32 = Binary CopySignFloat32

minFloat32 :: Expression -> Expression -> Expression
minFloat32 = Binary MinFloat32

maxFloat32 :: Expression -> Expression -> Expression
maxFloat32 = Binary MaxFloat32

eqFloat32 :: Expression -> Expression -> Expression
eqFloat32 = Binary EqFloat32

neFloat32 :: Expression -> Expression -> Expression
neFloat32 = Binary NeFloat32

ltFloat32 :: Expression -> Expression -> Expression
ltFloat32 = Binary LtFloat32

leFloat32 :: Expression -> Expression -> Expression
leFloat32 = Binary LeFloat32

gtFloat32 :: Expression -> Expression -> Expression
gtFloat32 = Binary GtFloat32

geFloat32 :: Expression -> Expression -> Expression
geFloat32 = Binary GeFloat32

addFloat64 :: Expression -> Expression -> Expression
addFloat64 = Binary AddFloat64

subFloat64 :: Expression -> Expression -> Expression
subFloat64 = Binary SubFloat64

mulFloat64 :: Expression -> Expression -> Expression
mulFloat64 = Binary MulFloat64

divFloat64 :: Expression -> Expression -> Expression
divFloat64 = Binary DivFloat64

copySignFloat64 :: Expression -> Expression -> Expression
copySignFloat64 = Binary CopySignFloat64

minFloat64 :: Expression -> Expression -> Expression
minFloat64 = Binary MinFloat64

maxFloat64 :: Expression -> Expression -> Expression
maxFloat64 = Binary MaxFloat64

eqFloat64 :: Expression -> Expression -> Expression
eqFloat64 = Binary EqFloat64

neFloat64 :: Expression -> Expression -> Expression
neFloat64 = Binary NeFloat64

ltFloat64 :: Expression -> Expression -> Expression
ltFloat64 = Binary LtFloat64

leFloat64 :: Expression -> Expression -> Expression
leFloat64 = Binary LeFloat64

gtFloat64 :: Expression -> Expression -> Expression
gtFloat64 = Binary GtFloat64

geFloat64 :: Expression -> Expression -> Expression
geFloat64 = Binary GeFloat64
