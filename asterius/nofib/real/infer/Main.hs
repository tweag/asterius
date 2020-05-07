module Main where

import Parse
import Shows
import Term
import Type
import Environment
import InferMonad
import Substitution	( Sub )
import MaybeM		( Maybe )
import Infer

import Control.Monad
import System.Environment
import NofibUtils

main = do
  input <- getContents
  replicateM_ 200 $ do
    input' <- salt input
    let inferred = concat (map readInferShow (lines input'))
    print (hash (showsString (show testEnv ++ prompt) inferred))

readInferShow :: String -> String
readInferShow =  useP ("Failed to parse" ++ prompt)   (
                 lexactlyP reads                      `eachP` (\t ->
                 useI ("Failed to type" ++ prompt)    (
                 inferTerm testEnv t                  `eachI` (\tt ->
                 show t ++ " : " ++ show tt ++ prompt))))
testEnv       :: Env
testEnv       =  read
                   (   "[ unit   : x -> List x,"
                   ++  "  append : List x -> List x -> List x,"
                   ++  "  fix    : (x -> x) -> x ]"
                   )
prompt        :: String
prompt        =  "\n? "
