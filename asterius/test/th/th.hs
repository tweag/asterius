{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

import Fib

main :: IO ()
main = print $([|fib 5|])
