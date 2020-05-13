Main program for lambda lifter

> module Main where


> import LambdaLift
> import Utilities
> import Print
> import Test
>
> import Control.Monad
> import System.Environment
> import NofibUtils

> main = do
>   (n:_) <- getArgs
>   replicateM_ (read n) $ do
>     inputExpr <- salt program
>     let plain = pprintSCs (lambdaLift inputExpr)
>     let lazy = pprintSCs (fullyLazyLift inputExpr)
>     print (hash ("\nInput expression " ++ pprintExpr inputExpr ++
>       "\nResult\n" ++ plain ++
>       "\nFully lazy \n" ++ lazy ++
>       "\n"))

> {- OLD: 1.2
> main ~(Str argString : ~(Str input : rest)) =

--> 	GetArgs :
--> 	ReadFile file :

>	AppendChan stdout ( -- "Args " ++ argString ++

>		-- "\nInput file " ++ file ++
>		-- "\nInput data " ++ input ++

>		"\nInput expression " ++ pprintExpr inputExpr ++
>		"\nResult\n" ++ output ++
>		"\nFully lazy \n" ++ pprintSCs (fullyLazyLift inputExpr) ++
>		"\n") :
>	[]
>
>     where
>     args = my_splitAt '\n' argString
>     file = args !! 0
>     inputExpr = program
>     output = pprintSCs (lambdaLift inputExpr)
>-}


|my_splitAt| splits a list into a list of lists, starting a new list
whenever a given element occurs.  The result always contains at least
one list, possibly empty.

> my_splitAt x [] = [[]]
> my_splitAt x (y:ys) | y == x    =  [] : my_splitAt x ys
>                  | otherwise =  (y:ys1) : yss where (ys1:yss) = my_splitAt x ys
