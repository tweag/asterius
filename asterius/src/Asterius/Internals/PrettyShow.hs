module Asterius.Internals.PrettyShow
  ( prettyShow,
  )
where

import GHC.IO (catchAny)
import System.Process

prettyShow :: Show a => a -> IO String
prettyShow a = do
  let s = show a
  catchAny (readProcess "ppsh" [] s) (\_ -> pure s)
