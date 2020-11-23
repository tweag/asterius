module Asterius.FixEnv
  ( fixEnv,
  )
where

import Data.Foldable
import Data.List
import System.Environment.Blank

fixEnv :: IO ()
fixEnv = do
  ks <-
    filter (\k -> ("GHC_" `isPrefixOf` k) || "HASKELL_" `isPrefixOf` k)
      . map fst
      <$> getEnvironment
  for_ ks unsetEnv
