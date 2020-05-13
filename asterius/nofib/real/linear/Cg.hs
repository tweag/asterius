

module Cg(cgiters,Cg_state (..),showcg_state)
        where

import Matrix (Vector , Matrix ,
               Block_list , Col_pos , Row_pos ,
               vsub,vdot,svmult,vadd,mvmult,norm)

import AbsDensematrix

import Utils



data Cg_state = Cg_stateC Vector Vector Vector Vector Int
type Precond_function = Matrix -> Vector -> Vector
type Scale_function = Matrix -> Vector -> (Matrix,Vector)



cgiters :: Scale_function -> Precond_function -> Matrix -> Vector -> [Cg_state]
cgiters scale precond a0 b0
   = iterate cgiter (Cg_stateC x0 r0 p0 q0 0)
     where
        x0 = b0 `vsub` b0
        (a,b) = scale a0 b0
        r0 = b
        p0 = precondition r0
        q0 = a `mvmult` p0
        precondition = precond a
        cgiter (Cg_stateC x r p q cnt)
	     = (Cg_stateC x' r' p' q' cnt')
               where
                  qq = q `vdot` q
                  cnt' = cnt + 1
                  x' = x `vadd` (alpha `svmult` p)
                  r' = r `vsub` (alpha `svmult` q)
                  alpha = rq / qq
                  rq = r `vdot` q
                  p'' = precondition r'
                  q'' = a `mvmult` p''
                  beta  = qp / qq
                  qp = q `vdot` q''
                  p' = p'' `vsub` (beta `svmult` p)
                  q' = q'' `vsub` (beta `svmult` q)



showcg_state :: Vector -> Cg_state -> [Char]
showcg_state soln (Cg_stateC x r p q cnt)
   = rjustify  5 (show cnt) ++ (spaces 4) ++
     rjustify 25 (show (norm err)) ++ (spaces 3) ++
     rjustify 25 (show (norm r)) ++ "\n"
     where
        err = vsub x soln
        alpha = if qq /= 0 then rq / qq
               else  0
        rq = vdot r q
        qq = vdot q q

