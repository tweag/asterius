{-# LANGUAGE TemplateHaskell #-}

import Fib
import Language.Haskell.TH.Syntax

main :: IO ()
main =
  putStrLn
    $( do
         x <- lift $ fib 10
         y <- newName "x"
         exts <- extsEnabled
         liftString $ show x <> ", " <> show y <> ", " <> show exts
     )
