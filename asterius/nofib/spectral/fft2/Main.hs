
module Main(main) -- floating point benchmark - Fourier transforms
where             --     Rex Page (rpage@trc.amoco.com)
import Fourier    --     Amoco Production Research, Sep 1992
import Complex_Vectors
import Data.Complex
import System.Environment
import Data.Maybe
import Control.Monad (forM_)

main = forM_ [1..100] $ \i -> do
  (n:args) <- getArgs
  let withPrint = elem "print" args
  let m = read n + i / 15 :: Double
  let results = ("result1 = " ++ show (result1 m) ++ "\n" ++

  if withPrint
    then putStr results
    else length (show results) `seq` return ()

result1 m =
        tstfft(rmwC  m)

result2 m =
        tstdft(rmwC  m)

result3 m =
        tstsct(rampWave  m)

tstfft zs  = distance zs (fftinv(fft zs))
valfft zs  = distance (fft zs) (sft zs)

tstdft zs  = distance zs (dftinv(dft zs))
valdft zs  = distance (dft zs) (sft zs)

tstsct = sum.sct

squareWave n = take n (repeat 1)
sqwC = (map (:+0)).squareWave

rampWave n = [0 .. n-1]
rmwC = (map (:+0)).rampWave

sineWave = (map sin).thetas
snwC = (map (:+0)).sineWave
