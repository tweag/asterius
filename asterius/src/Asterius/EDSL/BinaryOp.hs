{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Asterius.EDSL.BinaryOp where

import Asterius.Types

addInt32 = Binary AddInt32

subInt32 = Binary SubInt32

mulInt32 = Binary MulInt32

divSInt32 = Binary DivSInt32

divUInt32 = Binary DivUInt32

remSInt32 = Binary RemSInt32

remUInt32 = Binary RemUInt32

andInt32 = Binary AndInt32

orInt32 = Binary OrInt32

xorInt32 = Binary XorInt32

shlInt32 = Binary ShlInt32

shrUInt32 = Binary ShrUInt32

shrSInt32 = Binary ShrSInt32

rotLInt32 = Binary RotLInt32

rotRInt32 = Binary RotRInt32

eqInt32 = Binary EqInt32

neInt32 = Binary NeInt32

ltSInt32 = Binary LtSInt32

ltUInt32 = Binary LtUInt32

leSInt32 = Binary LeSInt32

leUInt32 = Binary LeUInt32

gtSInt32 = Binary GtSInt32

gtUInt32 = Binary GtUInt32

geSInt32 = Binary GeSInt32

geUInt32 = Binary GeUInt32

addInt64 = Binary AddInt64

subInt64 = Binary SubInt64

mulInt64 = Binary MulInt64

divSInt64 = Binary DivSInt64

divUInt64 = Binary DivUInt64

remSInt64 = Binary RemSInt64

remUInt64 = Binary RemUInt64

andInt64 = Binary AndInt64

orInt64 = Binary OrInt64

xorInt64 = Binary XorInt64

shlInt64 = Binary ShlInt64

shrUInt64 = Binary ShrUInt64

shrSInt64 = Binary ShrSInt64

rotLInt64 = Binary RotLInt64

rotRInt64 = Binary RotRInt64

eqInt64 = Binary EqInt64

neInt64 = Binary NeInt64

ltSInt64 = Binary LtSInt64

ltUInt64 = Binary LtUInt64

leSInt64 = Binary LeSInt64

leUInt64 = Binary LeUInt64

gtSInt64 = Binary GtSInt64

gtUInt64 = Binary GtUInt64

geSInt64 = Binary GeSInt64

geUInt64 = Binary GeUInt64

addFloat32 = Binary AddFloat32

subFloat32 = Binary SubFloat32

mulFloat32 = Binary MulFloat32

divFloat32 = Binary DivFloat32

copySignFloat32 = Binary CopySignFloat32

minFloat32 = Binary MinFloat32

maxFloat32 = Binary MaxFloat32

eqFloat32 = Binary EqFloat32

neFloat32 = Binary NeFloat32

ltFloat32 = Binary LtFloat32

leFloat32 = Binary LeFloat32

gtFloat32 = Binary GtFloat32

geFloat32 = Binary GeFloat32

addFloat64 = Binary AddFloat64

subFloat64 = Binary SubFloat64

mulFloat64 = Binary MulFloat64

divFloat64 = Binary DivFloat64

copySignFloat64 = Binary CopySignFloat64

minFloat64 = Binary MinFloat64

maxFloat64 = Binary MaxFloat64

eqFloat64 = Binary EqFloat64

neFloat64 = Binary NeFloat64

ltFloat64 = Binary LtFloat64

leFloat64 = Binary LeFloat64

gtFloat64 = Binary GtFloat64

geFloat64 = Binary GeFloat64
