{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

import Fib
import Language.Haskell.TH.Syntax

main :: IO ()
main = print $(lift $ fib 5)
