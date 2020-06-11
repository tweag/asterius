{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Asterius.Builtins.Math
  ( mathCBits,
  )
where

-- All the relevant Math imports can be found in Asterius.Builtins.rtsFunctionImports.

import Asterius.EDSL
import Asterius.Types

mathCBits :: AsteriusModule
mathCBits =
  mathSin
    <> mathCos
    <> mathTan
    <> mathSinh
    <> mathCosh
    <> mathTanh
    <> mathAsin
    <> mathAcos
    <> mathAtan
    <> mathLog
    <> mathExp

mathSin :: AsteriusModule
mathSin = runEDSL "sin" $ do
  x <- param F64
  callImport' "__asterius_sin_F64" [x] F64 >>= emit

mathCos :: AsteriusModule
mathCos = runEDSL "cos" $ do
  x <- param F64
  callImport' "__asterius_cos_F64" [x] F64 >>= emit

mathTan :: AsteriusModule
mathTan = runEDSL "tan" $ do
  x <- param F64
  callImport' "__asterius_tan_F64" [x] F64 >>= emit

mathSinh :: AsteriusModule
mathSinh = runEDSL "sinh" $ do
  x <- param F64
  callImport' "__asterius_sinh_F64" [x] F64 >>= emit

mathCosh :: AsteriusModule
mathCosh = runEDSL "cosh" $ do
  x <- param F64
  callImport' "__asterius_cosh_F64" [x] F64 >>= emit

mathTanh :: AsteriusModule
mathTanh = runEDSL "tanh" $ do
  x <- param F64
  callImport' "__asterius_tanh_F64" [x] F64 >>= emit

mathAsin :: AsteriusModule
mathAsin = runEDSL "asin" $ do
  x <- param F64
  callImport' "__asterius_asin_F64" [x] F64 >>= emit

mathAcos :: AsteriusModule
mathAcos = runEDSL "acos" $ do
  x <- param F64
  callImport' "__asterius_acos_F64" [x] F64 >>= emit

mathAtan :: AsteriusModule
mathAtan = runEDSL "atan" $ do
  x <- param F64
  callImport' "__asterius_atan_F64" [x] F64 >>= emit

mathLog :: AsteriusModule
mathLog = runEDSL "log" $ do
  x <- param F64
  callImport' "__asterius_log_F64" [x] F64 >>= emit

mathExp :: AsteriusModule
mathExp = runEDSL "exp" $ do
  x <- param F64
  callImport' "__asterius_exp_F64" [x] F64 >>= emit
