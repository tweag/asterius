{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

main :: IO ()
main = print $([|6 * 7 :: Int|])
