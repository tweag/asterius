
module Utils (
    cmap, rep, print_str, split_str, finish
    ) where

import Data.Char
import NofibUtils
import Config
import Types
import Env

cmap :: [(Xcont x) -> Cont] -> (Xscont x) -> Cont
cmap [] xsc        =  xsc []
cmap (xc:xcs) xsc  =  xc (cmap xcs . (.) xsc . (:))

rep :: Int -> x -> [x]
rep n x  =  take n (repeat x)

print_str :: String -> Cont -> Cont
-- print_str s c  =  get_output (\ op e -> op s >> c e)
print_str s c  =  get_output (\ op e -> op (show (hash s) ++ "\n") >> c e)

split_str :: Int -> String -> String
split_str n ""  =  ""
split_str n s   =  start ++ next ++ "\n"
                   ++ split_str n (dropWhile isSpace rest)
                   where
                   (start, rest1)         =  split_num n s
                   (next,  rest)          =  span (not . isSpace) rest1
                   split_num 0 s          =  ("", s)
                   split_num _ ""         =  ("", "")
                   split_num _ ('\n':cs)  =  ("", ' ':cs)
                   split_num m (c:cs)     =  (c:cs', cs'')
                                             where
                                             (cs',cs'') = split_num (m-one) cs

finish :: Cont
finish e  = return ()
