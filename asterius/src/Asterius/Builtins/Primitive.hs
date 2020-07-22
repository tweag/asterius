{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Webassembly implementations for primitive operations (@memcpy@, @memmove@,
-- @memcmp@, and @memset@ variants). Each binding corresponds to a definition
-- in the cbits of the @primitive@ package:
--
-- https://github.com/haskell/primitive/blob/c07823669e542399b7af11ffbf924d7106e3f145/cbits/primitive-memops.h#L1-L23
module Asterius.Builtins.Primitive
  ( primitiveImports,
    primitiveCBits,
  )
where

import Asterius.EDSL
import Asterius.Types
import qualified Data.ByteString as BS

-- GEORGE: Maybe use this function in all Asterius.Builtin.* modules to save us
-- from duplication and copy-paste errors?

-- | Create a function import with internal name of the form
-- @__asterius_<EXTERNAL-MODULE-NAME>_<EXTERNAL-BASE-NAME>@.
mkImport ::
  -- | External module name
  BS.ByteString ->
  -- | External base name
  BS.ByteString ->
  -- | Function type.
  FunctionType ->
  FunctionImport
mkImport ext_mod_name ext_base_name fn_type =
  FunctionImport
    { internalName = "__asterius_" <> ext_mod_name <> "_" <> ext_base_name,
      externalModuleName = ext_mod_name,
      externalBaseName = ext_base_name,
      functionType = fn_type
    }
{-# INLINE mkImport #-}

-- -------------------------------------------------------------------------

-- GEORGE: Note that Asterius uses 64 bits everywhere, but WebAssembly at the
-- moment does not support I64. Yet, it DOES support F64. Hence, we convert
-- everything to F64 in all the signatures. It would be nice to remove this
-- hack eventually, but that I guess is not in our hands at the moment.

-- | Wasm import of the JavaScript implementation of @memcpy@, @memmove@,
-- @memset@, and @memcmp@ (see implementations in rts/rts.memory.mjs). Notice
-- that for some of them we ignore their result type; the hsprimitive_*
-- functions do not use them.
primitiveImports :: [FunctionImport]
primitiveImports =
  [ mkImport "Memory" "memcpy" $
      FunctionType {paramTypes = [I64, I64, I64], returnTypes = []},
    mkImport "Memory" "memmove" $
      FunctionType {paramTypes = [I64, I64, I64], returnTypes = []},
    mkImport "Memory" "memset" $
      FunctionType {paramTypes = [I64, I64, I64, I64], returnTypes = []},
    mkImport "Memory" "memsetFloat32" $
      FunctionType {paramTypes = [F64, F32, F64], returnTypes = []},
    mkImport "Memory" "memsetFloat64" $
      FunctionType {paramTypes = [F64, F64, F64], returnTypes = []},
    mkImport "Memory" "memcmp" $
      FunctionType {paramTypes = [I64, I64, I64], returnTypes = [I64]}
  ]

-- -------------------------------------------------------------------------

primitiveCBits :: AsteriusModule
primitiveCBits =
  primitiveMemcpy
    <> primitiveMemmove
    <> primitiveMemcmp
    <> primitiveMemsetWord8
    <> primitiveMemsetWord16
    <> primitiveMemsetWord32
    <> primitiveMemsetWord64
    <> primitiveMemsetWord
    <> primitiveMemsetPtr
    <> primitiveMemsetFloat
    <> primitiveMemsetDouble
    <> primitiveMemsetChar

-- -------------------------------------------------------------------------

-- | @void hsprimitive_memcpy(void *dst, ptrdiff_t doff, void *src, ptrdiff_t soff, size_t len)@
primitiveMemcpy :: AsteriusModule
primitiveMemcpy = runEDSL "hsprimitive_memcpy" $ do
  [dst, doff, src, soff, len] <- params [I64, I64, I64, I64, I64]
  let arg1 = dst `addInt64` doff
      arg2 = src `addInt64` soff
  callImport "__asterius_Memory_memcpy" [arg1, arg2, len]

-- | @void hsprimitive_memmove(void *dst, ptrdiff_t doff, void *src, ptrdiff_t soff, size_t len)@
primitiveMemmove :: AsteriusModule
primitiveMemmove = runEDSL "hsprimitive_memmove" $ do
  [dst, doff, src, soff, len] <- params [I64, I64, I64, I64, I64]
  let arg1 = dst `addInt64` doff
      arg2 = src `addInt64` soff
  callImport "__asterius_Memory_memmove" [arg1, arg2, len]

-- | @int hsprimitive_memcmp(HsWord8 *s1, HsWord8 *s2, size_t n)@
primitiveMemcmp :: AsteriusModule
primitiveMemcmp = runEDSL "hsprimitive_memcmp" $ do
  setReturnTypes [I64]
  args <- params [I64, I64, I64]
  callImport' "__asterius_Memory_memcmp" args I64
    >>= emit

-- | @void hsprimitive_memset_XXX (XXX *p, ptrdiff_t off, size_t n, XXX x)@
mkPrimitiveMemsetUInt ::
  -- | Size (in bytes) of the type
  Int ->
  -- | String representation of the type
  EntitySymbol ->
  AsteriusModule
mkPrimitiveMemsetUInt size typerep = runEDSL hsname $ do
  [p, off, n, x] <- params [I64, I64, I64, I64]
  callImport "__asterius_Memory_memset" [p `addInt64` off, x, n, constI64 size]
  where
    hsname = "hsprimitive_memset_" <> typerep

-- -------------------------------------------------------------------------

-- | @void hsprimitive_memset_Word8 (HsWord8 *p, ptrdiff_t off, size_t n, HsWord x)@
primitiveMemsetWord8 :: AsteriusModule
primitiveMemsetWord8 = mkPrimitiveMemsetUInt 1 "Word8"

-- void hsprimitive_memset_Word16 (HsWord16 *, ptrdiff_t, size_t, HsWord);
primitiveMemsetWord16 :: AsteriusModule
primitiveMemsetWord16 = mkPrimitiveMemsetUInt 2 "Word16"

-- void hsprimitive_memset_Word32 (HsWord32 *, ptrdiff_t, size_t, HsWord);
primitiveMemsetWord32 :: AsteriusModule
primitiveMemsetWord32 = mkPrimitiveMemsetUInt 4 "Word32"

-- void hsprimitive_memset_Word64 (HsWord64 *, ptrdiff_t, size_t, HsWord64);
primitiveMemsetWord64 :: AsteriusModule
primitiveMemsetWord64 = mkPrimitiveMemsetUInt 8 "Word64"

-- void hsprimitive_memset_Word (HsWord *, ptrdiff_t, size_t, HsWord);
primitiveMemsetWord :: AsteriusModule
primitiveMemsetWord = mkPrimitiveMemsetUInt 8 "Word"

-- void hsprimitive_memset_Ptr (HsPtr *, ptrdiff_t, size_t, HsPtr);
primitiveMemsetPtr :: AsteriusModule
primitiveMemsetPtr = mkPrimitiveMemsetUInt 8 "Ptr"

-- void hsprimitive_memset_Float (HsFloat *, ptrdiff_t, size_t, HsFloat);
primitiveMemsetFloat :: AsteriusModule
primitiveMemsetFloat = runEDSL "hsprimitive_memset_Float" $ do
  [p, off, n, x] <- params [I64, I64, I64, F32]
  callImport "__asterius_Memory_memsetFloat32" $
    [ convertUInt64ToFloat64 $ p `addInt64` off,
      x,
      convertUInt64ToFloat64 n
    ]

-- void hsprimitive_memset_Double (HsDouble *, ptrdiff_t, size_t, HsDouble);
primitiveMemsetDouble :: AsteriusModule
primitiveMemsetDouble = runEDSL "hsprimitive_memset_Double" $ do
  [p, off, n, x] <- params [I64, I64, I64, F64]
  callImport "__asterius_Memory_memsetFloat64" $
    [ convertUInt64ToFloat64 $ p `addInt64` off,
      x,
      convertUInt64ToFloat64 n
    ]

-- void hsprimitive_memset_Char (HsChar *, ptrdiff_t, size_t, HsChar);
primitiveMemsetChar :: AsteriusModule
primitiveMemsetChar = mkPrimitiveMemsetUInt 4 "Char"
