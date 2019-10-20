module Asterius.Internals.Containers
  ( find,
  )
where

import Control.Applicative
import qualified Data.Map.Internal as M

find :: (k -> a -> Maybe b) -> M.Map k a -> Maybe b
find f = w
  where
    w (M.Bin _ k a l r) = w l <|> f k a <|> w r
    w M.Tip = Nothing
