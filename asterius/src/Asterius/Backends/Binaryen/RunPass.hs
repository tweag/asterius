module Asterius.Backends.Binaryen.RunPass
  ( runPass,
  )
where

import qualified Asterius.Internals.Arena as A
import Asterius.Internals.Marshal
import qualified Binaryen.Module as Binaryen
import qualified Data.ByteString as BS
import Data.Traversable

runPass :: Binaryen.Module -> [BS.ByteString] -> IO ()
runPass m ps = A.with $ \a -> do
  ps' <- for ps $ marshalBS a
  (ps'', l) <- marshalV a ps'
  Binaryen.runPasses m ps'' (fromIntegral l)
