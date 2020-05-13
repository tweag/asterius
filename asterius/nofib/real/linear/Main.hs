
import Matrix  -- matrix implementation
import Input   -- read gcomp data files
import Misc    -- for timing function and takeuntil


import AbsCg (solve_iters, Cg_state (..), show_state)
import Absmatlib

import Control.Monad
import System.Environment
import NofibUtils



conv1 (Cg_stateC x r p q c) =
    (norm r) < 0.000001

conv2 (Cg_stateC x r p q c) =
    sqrt (norm r) < 0.0000004


main = replicateM_ 200 $ do
  (n:_) <- getArgs
  let result = test bilu test_data (read n) conv2
  print (hash result)

test_data = hard_data

noscale a b = (a,b)
noprecond a b = b

test process data' set conv =
    run (test' process data' set conv,
         process ++ "/" ++ data' ++ "/" ++ (show set))

test' process data' set conv
   = header ++ output ++ "\n"
     where
        output =
              concat (map (show_state soln) iterations)
        iterations =
            takeuntil conv (take maxiters all_iterations)
        all_iterations =
            solve_iters scale precond a b
        (scale,precond)
           = case process of
              "bilu" -> (doscale,doprecond numwells)
              "none" -> (noscale,noprecond)
              _ ->  error usage
        (a,soln,b,numwells)
           = case data' of
              "easy_data" -> (a_easy set, x1 set, mvmult a soln, 0)
              "hard_data" -> (a_hard set, x1 set, mvmult a soln, 0)
              "gcomp_data" -> (gmat set, soln_vect set, rhside set, wells set)
              _      -> error usage
        maxiters = 10
        usage =
           "Usage: test (bilu|none) (test_data|gcomp_data)" ++
           " num (conv1|conv2)"


header :: [Char]
header =
    "\nIteration   norm (x-soln)   norm r  \n" ++
    "=========   =============   ======= \n"



easy_data = "easy_data"
hard_data = "hard_data"
gcomp_data   = "gcomp_data"
bilu      = "bilu"
none      = "none"

