{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Webassembly implementations for primitive operations (@memcpy@, @memmove@,
-- @memcmp@, and @memset@ variants).
module Asterius.Builtins.Primitive
  ( primitiveImports,
    primitiveCBits,
  )
where

import Asterius.EDSL
import Asterius.Types
import qualified Data.ByteString.Short as SBS


-- GEORGE: I do not like that the cbits are disassociated from their imports
-- (this is the case in all Asterius.Builtins.* files). It is very easy to
-- forget one. Maybe we can find a better way to do this in the future.

-- -------------------------------------------------------------------------

-- GEORGE: Maybe use this function in all Asterius.Builtin.* modules to save us
-- from duplication and copy-paste errors?

mkImport ::
  -- | External module name
  SBS.ShortByteString ->
  -- | External base name
  SBS.ShortByteString ->
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
--
-- General Guidelines:
--   * Pointers --> F64
--   * I64 --> F64
--
-- What about the rest of the types? Is there a strategy for converting the types?
--
-- GEORGE thinks:
--   * size_t --> F64

-- See the JS implementations in @./asterius/asterius/rts/rts.memory.mjs@:
--
-- > memcpy(_dst, _src, n) {
-- >   this.i8View.copyWithin(
-- >     Memory.unTag(_dst),
-- >     Memory.unTag(_src),
-- >     Memory.unTag(_src) + n
-- >   );
-- > }
-- >
-- > memmove(_dst, _src, n) {
-- >   return this.memcpy(_dst, _src, n);
-- > }
-- >
-- > memset(_dst, c, n) {
-- >   this.i8View.fill(c, Memory.unTag(_dst), Memory.unTag(_dst) + n);
-- > }
-- >
-- > memcmp(_ptr1, _ptr2, n) {
-- >   for (let i = 0; i < n; ++i) {
-- >     const sgn = Math.sign(
-- >       this.i8View[Memory.unTag(_ptr1) + i] -
-- >         this.i8View[Memory.unTag(_ptr2) + i]
-- >     );
-- >     if (sgn) return sgn;
-- >   }
-- >   return 0;
-- > }
primitiveImports :: [FunctionImport]
primitiveImports =
  [ mkImport "primitive" "memcpy" $ -- void * memcpy ( void * destination, const void * source, size_t num );
      FunctionType {paramTypes = [F64, F64, F64], returnTypes = [F64]},
    mkImport "primitive" "memmove" $ -- void * memmove ( void * destination, const void * source, size_t num );
      FunctionType {paramTypes = [F64, F64, F64], returnTypes = [F64]},
    mkImport "primitive" "memcmp" $ -- int memcmp ( const void * ptr1, const void * ptr2, size_t num );
      FunctionType {paramTypes = [F64, F64, F64], returnTypes = [F64]}
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

-- void hsprimitive_memcpy( void *dst, ptrdiff_t doff, void *src, ptrdiff_t soff, size_t len )
-- {
--   memcpy( (char *)dst + doff, (char *)src + soff, len );
-- }
primitiveMemcpy :: AsteriusModule
primitiveMemcpy = runEDSL "hsprimitive_memcpy" $ do
  setReturnTypes []
  [dst,doff,src,soff,len] <- params [I64,I64,I64,I64,I64]
  let arg1 = dst `addInt64` doff
      arg2 = src `addInt64` soff
  callImport -- TODO: Convert result to I64 / drop it
    "__asterius_primitive_memcpy"
    [arg1, arg2, len] -- TODO: Convert arguments to F64
    -- GEORGE: Strictly speaking, it returns a pointer, but we wish to drop it. Is this approach valid?
  -- TODO: Add the conversions from and to F64

-- -------------------------------------------------------------------------

-- void hsprimitive_memmove( void *dst, ptrdiff_t doff, void *src, ptrdiff_t soff, size_t len )
-- {
--   memmove( (char *)dst + doff, (char *)src + soff, len );
-- }
primitiveMemmove :: AsteriusModule
primitiveMemmove = runEDSL "hsprimitive_memmove" $ do
  setReturnTypes []
  [dst,doff,src,soff,len] <- params [I64,I64,I64,I64,I64]
  let arg1 = dst `addInt64` doff
      arg2 = src `addInt64` soff
  callImport -- TODO: Convert result to I64 / drop it
    "__asterius_primitive_memmove"
    [arg1, arg2, len] -- TODO: Convert arguments to F64
    -- GEORGE: Strictly speaking, it returns a pointer, but we wish to drop it. Is this approach valid?

-- -------------------------------------------------------------------------

-- int hsprimitive_memcmp( HsWord8 *s1, HsWord8 *s2, size_t n )
-- {
--   return memcmp( s1, s2, n );
-- }
primitiveMemcmp :: AsteriusModule
primitiveMemcmp = runEDSL "hsprimitive_memcmp" $ do
  setReturnTypes [I64]
  args <- params [I64,I64,I64]
  truncSFloat64ToInt64 <$>
    callImport'
      "__asterius_primitive_memcmp"
      (map convertSInt64ToFloat64 args)
      F64
    >>= emit

-- -------------------------------------------------------------------------

-- void hsprimitive_memset_Word8 (HsWord8 *p, ptrdiff_t off, size_t n, HsWord x)
-- {
--   memset( (char *)(p+off), x, n );
-- }
primitiveMemsetWord8 :: AsteriusModule
primitiveMemsetWord8 = runEDSL "hsprimitive_memcmp" $ do
  setReturnTypes []
  [p, off, n, x] <- params [I64,I64,I64,I64]
  let arg1 = p `addInt64` off
  callImport -- TODO: Convert result to I64 / drop it
    "__asterius_primitive_memset"
    [arg1, x, n] -- TODO: Convert arguments to F64
    -- GEORGE: Strictly speaking, it returns a pointer, but we wish to drop it. Is this approach valid?

-- -------------------------------------------------------------------------

-- # #define MEMSET(TYPE, ATYPE)                                                  \
-- # void hsprimitive_memset_ ## TYPE (Hs ## TYPE *p, ptrdiff_t off, size_t n, ATYPE x) \
-- # {                                                                            \
-- #   p += off;                                                                  \
-- #   if (x == 0)                                                                \
-- #     memset(p, 0, n * sizeof(Hs ## TYPE));                                    \
-- #   else if (sizeof(Hs ## TYPE) == sizeof(int)*2) {                            \
-- #     int *q = (int *)p;                                                       \
-- #     const int *r = (const int *)(void *)&x;                                  \
-- #     while (n>0) {                                                            \
-- #       q[0] = r[0];                                                           \
-- #       q[1] = r[1];                                                           \
-- #       q += 2;                                                                \
-- #       --n;                                                                   \
-- #     }                                                                        \
-- #   }                                                                          \
-- #   else {                                                                     \
-- #     while (n>0) {                                                            \
-- #       *p++ = x;                                                              \
-- #       --n;                                                                   \
-- #     }                                                                        \
-- #   }                                                                          \
-- # }
-- #
-- # /* MEMSET(HsWord8, HsWord) */
-- # MEMSET(Word16, HsWord)
-- # MEMSET(Word32, HsWord)
-- # MEMSET(Word64, HsWord64)
-- # MEMSET(Word, HsWord)
-- # MEMSET(Ptr, HsPtr)
-- # MEMSET(Float, HsFloat)
-- # MEMSET(Double, HsDouble)
-- # MEMSET(Char, HsChar)

-- TODO: Do some sharing once you have created a couple of the memset functions.


-- void hsprimitive_memset_Word16 (HsWord16 *, ptrdiff_t, size_t, HsWord);
primitiveMemsetWord16 :: AsteriusModule
primitiveMemsetWord16 = runEDSL "hsprimitive_memset_Word16" $ do
  error "TODO"

-- void hsprimitive_memset_Word32 (HsWord32 *, ptrdiff_t, size_t, HsWord);
primitiveMemsetWord32 :: AsteriusModule
primitiveMemsetWord32 = runEDSL "hsprimitive_memset_Word32" $ do
  error "TODO"

-- void hsprimitive_memset_Word64 (HsWord64 *, ptrdiff_t, size_t, HsWord64);
primitiveMemsetWord64 :: AsteriusModule
primitiveMemsetWord64 = runEDSL "hsprimitive_memset_Word64" $ do
  error "TODO"

-- void hsprimitive_memset_Word (HsWord *, ptrdiff_t, size_t, HsWord);
primitiveMemsetWord :: AsteriusModule
primitiveMemsetWord = runEDSL "hsprimitive_memset_Word" $ do
  error "TODO"

-- void hsprimitive_memset_Ptr (HsPtr *, ptrdiff_t, size_t, HsPtr);
primitiveMemsetPtr :: AsteriusModule
primitiveMemsetPtr = runEDSL "hsprimitive_memset_Ptr" $ do
  error "TODO"

-- void hsprimitive_memset_Float (HsFloat *, ptrdiff_t, size_t, HsFloat);
primitiveMemsetFloat :: AsteriusModule
primitiveMemsetFloat = runEDSL "hsprimitive_memset_Float" $ do
  error "TODO"

-- void hsprimitive_memset_Double (HsDouble *, ptrdiff_t, size_t, HsDouble);
primitiveMemsetDouble :: AsteriusModule
primitiveMemsetDouble = runEDSL "hsprimitive_memset_Double" $ do
  error "TODO"

-- void hsprimitive_memset_Char (HsChar *, ptrdiff_t, size_t, HsChar);
primitiveMemsetChar :: AsteriusModule
primitiveMemsetChar = runEDSL "hsprimitive_memset_Char" $ do
  error "TODO"



