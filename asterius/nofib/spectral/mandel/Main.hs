module Main where

import Mandel
import PortablePixmap
import Control.Monad
import System.IO
import System.Environment

class NFData a where
  rnf :: a -> ()

instance NFData a => NFData [a] where
  rnf = foldr (\a b -> rnf a `seq` b) ()

instance (NFData a, NFData b, NFData c) => NFData (a, b, c) where
  rnf (a, b, c) = rnf a `seq` rnf b `seq` rnf c

instance NFData Int where
  rnf n = n `seq` ()

instance NFData Integer where
  rnf n = n `seq` ()

instance NFData PixMap where
  rnf (Pixmap a b c d) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d

main = replicateM_ 100 $ do
  hSetBinaryMode stdout True
  [minx,miny,maxx,maxy,_,_,_]     <- map read <$> getArgs
  [_,_,_,_,screenX,screenY,limit] <- map read <$> getArgs
  [_,_,_,_,_,_,limit]             <- map read <$> getArgs
  -- print (mandelset minx miny maxx maxy screenX screenY limit)
  rnf (mandelset minx miny maxx maxy screenX screenY limit) `seq` return ()
