
module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (replicateM_, void)
import System.Environment (getArgs)
import NofibUtils (hash)

main :: IO ()
main = do
 (n:_) <- getArgs
 replicateM_ (read n) $ do
   (_:argv) <- getArgs
   parse_args defaultEnv (unlines argv)

parse_args :: Env -> String -> IO ()
parse_args (MkEnv x1 x2 x3) ('-':'l':rest)
    =  readbool (parse_args (MkEnv True x2 x3)) rest
parse_args (MkEnv x1 x2 x3) ('-':'m':rest)
    =  readstring (\str -> parse_args (MkEnv x1 str x3)) rest
parse_args (MkEnv x1 x2 x3) ('-':'o':rest)
    =  readstring (\str -> parse_args (MkEnv x1 x2 (File str))) rest
parse_args e rest  =  next (pairs (lines rest)) e

pairs :: [String] -> [(String, String)]
pairs []         =  []
pairs (s:s':ss)  =  (s,s') : pairs ss
pairs [s]        =  [("",s)]

next :: [(String, String)] -> Cont
next sps e | check sps  =  mkprog sps e
           | otherwise  =  usage

check :: [(String, String)] -> Bool
check sps  =  flags_ok sps && no_dups [flag | (_:flag:_,_) <- sps]
              where
              flags_ok  =  and . map f
              f (['-',_],_)       =  True
              f (['+',_],"Bool")  =  True
              f _                 =  False
              no_dups []      =  True
              no_dups (c:cs)  =  not (c `elem` cs) && no_dups cs

usage :: IO ()
usage =
    putStr
     "Usage: mkhprog [-l] [-m module_name] [-o file_name] [[-flag type] ...]"

mkprog :: [(String, String)] -> Cont
mkprog sps
    =  (do_header . nl . do_default types . nl . do_usage sps . nl
        . do_datadecl types . nl . do_parse_args sps . nl . do_auxfns)
       stop
       where
       types  =  map snd sps

do_header :: Cont -> Cont
do_header c
    =  modname (\s -> writeln ("module " ++ s ++ " (main) where\n\nimport System (getArgs)") c)

do_default :: [String] -> Cont -> Cont
do_default ts
    =  writeln "defaultArgs :: Args"
       . writeln ("defaultArgs  =  MkArgs" ++ concat (map f ts))
       where
       f "String"  =  " \"\""
       f "Bool"    =  " False"
       f _         =  " ??"

do_usage :: [(String, String)] -> Cont -> Cont
do_usage sps
    =  writeln "usage :: IO ()"
       . writeln ("usage  =  hPutStr stderr \"Usage: prog"
                  ++ concat (map f sps) ++ "\"")
       where
       f (['-',c], "Bool")  =  " [-" ++ [c] ++ "]"
       f (['+',c], "Bool")  =  " [(+|-)" ++ [c] ++ "]"
       f (flag, typ)        =  " [" ++ flag ++ " " ++ typ ++ "]"

do_datadecl :: [String] -> Cont -> Cont
do_datadecl ts
    =  writeln ("data Args  =  MkArgs" ++ concat (map ((:) ' ') ts)
                ++ " deriving ()")

do_parse_args :: [(String, String)] -> Cont -> Cont
do_parse_args sps
    =  writeln "parse_args :: Args -> String -> IO ()"
       . foldr (.) end
       (zipWith3 do_one_flag (repeat n) [1..] sps)
       where
       n  =  length sps
       end  =  writeln (phead n ++ wildmatch '-' ++ "usage")
               . writeln (phead n ++ " rest  =  prog" ++ args 1 n
                          ++ " (lines rest)")


do_one_flag :: Int -> Int -> (String, String) -> Cont -> Cont
do_one_flag n r (['+',flag], _)
    =  writeln (phead n ++ match '-' flag ++ do_readbool n r False)
       . writeln (phead n ++ match '+' flag ++ do_readbool n r True)
do_one_flag n r (['-',flag], "Bool")
    =  writeln (phead n ++ match '-' flag ++ do_readbool n r True)
do_one_flag n r (['-',flag], "String")
    =  writeln (phead n ++ match '-' flag ++ do_readstring n r)
do_one_flag n r (['-',flag], _)
    =  writeln (phead n ++ match '-' flag ++ do_readval n r)

do_auxfns :: Cont -> Cont
do_auxfns
    =    writeln "main :: IO ()\n\
                 \main  =  getArgs >>= (parse_args defaultArgs . unlines)"
       . nl
       . writeln "readbool :: (String -> IO ()) -> String -> IO ()\n\
                 \readbool f \"\"         =  f \"\"\n\
                 \readbool f ('\\n':cs)  =  f cs\n\
                 \readbool f _          =  usage"
       . nl
       . writeln "readstring :: (String -> String -> IO ()) -> String \
                               \-> IO ()\n\
                 \readstring f \"\"  =  f \"\" \"\"\n\
                 \readstring f cs@(c:cs')\n\
                 \    =  f s t\n\
                 \       where\n\
                 \       st      =  if c == '\\n' then cs' else cs\n\
                 \       (s,t1)  =  span ((/=) '\\n') st\n\
                 \       t       =  if t1 == \"\" then t1 else (tail t1)"
       . nl
       . writeln "readval :: (Read a) => ReadS a \
                            \-> (a -> String -> IO ()) -> String\n\
                 \                       -> IO ()\n\
                 \readval readsfn f str\n\
                 \    =  case thing of\n\
                 \           []    -> usage\n\
                 \           (_:_) -> f val (if str' == \"\" \
                                            \then str' else (tail str'))\n\
                 \       where\n\
                 \       thing        =  readsfn str\n\
                 \       (val, str')  =  head thing"

phead :: Int -> String
phead n  =  "parse_args (MkArgs" ++ args 1 n ++ ") "

args :: Int -> Int -> String
args a b  =  concat [" x" ++ show r | r <- [a..b]]

match :: Char -> Char -> String
match mp c  =  "('" ++ [mp] ++ "':'" ++ [c] ++ "':rest)\n    =  "

wildmatch :: Char -> String
wildmatch mp  =  "('" ++ [mp] ++ "': _ :rest)\n    =  "

do_readval, do_readstring :: Int -> Int -> String
do_readbool   :: Int -> Int -> Bool -> String
do_readval n r     =  "readval reads (\\val -> " ++ bits n r "val"
do_readstring n r  =  "readstring (\\str -> " ++ bits n r "str"
do_readbool n r b  =  "readbool (" ++ bits n r (show b)

bits :: Int -> Int -> String -> String
bits n r str
    =  "parse_args (MkArgs" ++ args 1 (r-1) ++ " " ++ str ++ args (r+1) n
       ++ ")) rest"

writeln :: String -> Cont -> Cont
writeln str c
    =  islit (\b ->
       output (\f env ->
           let
             str'   =  (if b then (lit str) else str) ++ "\n"
             lit s  =  "> " ++ foldr ((++) . conv) "" s
             conv '\n'  =  "\n> "
             conv c     =  [c]

nl :: Cont -> Cont
nl c = output (\f env -> f "\n" >> c env)

readbool :: (String -> IO ()) -> String -> IO ()
readbool f ""         =  f ""
readbool f ('\n':cs)  =  f cs
readbool f _          =  usage

readstring :: (String -> String -> IO ()) -> String -> IO ()
readstring f ""  =  f "" ""
readstring f cs@(c:cs')
      =  f s t
         where
         st      =  if c == '\n' then cs' else cs
         (s,t1)  =  span ((/=) '\n') st
         t       =  if t1 == "" then t1 else (tail t1)

type Cont  =  Env -> IO ()

stop :: Cont
stop e  =  return ()

data Env  =  MkEnv Bool String Output

data Output  =  Stdout | File String

defaultEnv :: Env
defaultEnv  =  MkEnv False "Main" Stdout

islit :: (Bool -> Cont) -> Cont
islit bc e@(MkEnv l _ _)  =  bc l e

modname :: (String -> Cont) -> Cont
modname sc e@(MkEnv _ m _)  =  sc m e

output :: ((String -> IO ()) -> Cont) -> Cont
-- output oc e@(MkEnv _ _ Stdout)    =  oc (putStr) e
output oc e@(MkEnv _ _ Stdout)    =  oc (void . evaluate . hash) e
output oc e@(MkEnv _ _ (File f))  =  oc (appendFile f) e
