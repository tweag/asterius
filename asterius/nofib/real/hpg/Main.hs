
module Main (
    main
    ) where

import Config
import Types
import Env
import Utils
import GenType
import GenVal
import GenExp
import Control.Monad
import System.Environment
import System.IO

main :: IO ()
main =  replicateM_ 100 $ do
  argv <- getArgs
  parse_args defaultArgs (unlines argv)

data Args  =  MkArgs (Int,Int,Int) Int Int Int Int Int String Output
              deriving ()

defaultArgs :: Args
defaultArgs  =  MkArgs (9807,65,32975) 4 4 4 4 4 "Main" default_output

parse_args :: Args -> String -> IO ()
parse_args (MkArgs x1 x2 x3 x4 x5 x6 x7 x8) ('-':'s':rest)
    =  readval reads
       (\val -> parse_args (MkArgs val x2 x3 x4 x5 x6 x7 x8)) rest
parse_args (MkArgs x1 x2 x3 x4 x5 x6 x7 x8) ('-':'n':'t':rest)
    =  readval reads
       (\val -> parse_args (MkArgs x1 val x3 x4 x5 x6 x7 x8)) rest
parse_args (MkArgs x1 x2 x3 x4 x5 x6 x7 x8) ('-':'d':'t':rest)
    =  readval reads
       (\val -> parse_args (MkArgs x1 x2 val x4 x5 x6 x7 x8)) rest
parse_args (MkArgs x1 x2 x3 x4 x5 x6 x7 x8) ('-':'n':'v':rest)
    =  readval reads
       (\val -> parse_args (MkArgs x1 x2 x3 val x5 x6 x7 x8)) rest
parse_args (MkArgs x1 x2 x3 x4 x5 x6 x7 x8) ('-':'d':'v':rest)
    =  readval reads
       (\val -> parse_args (MkArgs x1 x2 x3 x4 val x6 x7 x8)) rest
parse_args (MkArgs x1 x2 x3 x4 x5 x6 x7 x8) ('-':'d':'e':rest)
    =  readval reads
       (\val -> parse_args (MkArgs x1 x2 x3 x4 x5 val x7 x8)) rest
parse_args (MkArgs x1 x2 x3 x4 x5 x6 x7 x8) ('-':'m':rest)
    =  readstring (\str -> parse_args (MkArgs x1 x2 x3 x4 x5 x6 str x8)) rest
parse_args (MkArgs x1 x2 x3 x4 x5 x6 x7 x8) ('-':'o':rest)
    =  readstring (\str -> parse_args
                        (MkArgs x1 x2 x3 x4 x5 x6 x7 (set_output str))) rest
parse_args (MkArgs x1 x2 x3 x4 x5 x6 x7 x8) ""
    =  hpg x1 x2 x3 x4 x5 x6 x7 x8
parse_args (MkArgs x1 x2 x3 x4 x5 x6 x7 x8) _
    =  usage defaultArgs

hpg :: (Int, Int, Int) -> Int -> Int -> Int -> Int -> Int -> String
       -> Output -> Answer
hpg s nt dt nv dv de mn op
    =  print_str (head "")
                 (gen_types max_vnts max_flds nt dt c1) (make_Env s op)
       where
       c1     =  gen_vals nv dv c2
       c2     =  gen_exps de print_program
       head   =  showString "-- HPG version " . version
                 . newline
                 . showString "-- Output from hpg "
                 . sep_list space id
                            [shows s, shows nt, shows dt, shows nv, shows dv,
                             shows de]
                 . newline
                 . sep_list newline id [shead, thead, vhead, ehead]
                 . newline . newline . mhead . newline . newline
       shead  =  showString "-- Random number generator seeds: " . shows s
       thead  =  showString "-- " . shows nt . showString " types, of depth "
                 . shows dt
       vhead  =  showString "-- " . shows nv . showString " values, of depth "
                 . shows dv
       ehead  =  showString "-- Expression depth: " . shows de
       mhead  =  mod_name . space . showString mn . space . lbrack
                 . main_name . rbrack . space . where_name . space . lbrace

print_program :: Xscont (Val_name, Expression)
print_program vnes
    =  get_all_type_decls (\tds -> get_all_val_decls (\vds ->
           print_str (split_str line_len
               ((sep_list dsep id (map showsType_decl tds
                                   ++ map showsVal_decl vds)
                . dsep ) ""))
               (print_test vnes)))
       where
       dsep  =  decl_sep . newline

print_test :: Xscont (Val_name, Expression)
print_test vnes
    =  print_str (split_str line_len
                            ((main_name . val_def . showString print_name
                              . space . lsq
                              . sep_list list_separator showspair vnes . rsq
                              . newline . rbrace)
                             "")) finish
       where
       showspair (vn, e)  =  showString vn . space . showString eq_name
                             . space . shows e

readstring :: (String -> String -> IO ()) -> String -> IO ()
readstring f ""  =  f "" ""
readstring f cs@(c:cs')
    =  f s t
       where
       st      =  if c == '\n' then cs' else cs
       (s,t1)  =  span ((/=) '\n') st
       t       =  if t1 == "" then t1 else (tail t1)

readval :: (Read a) => ReadS a -> (a -> String -> IO ()) -> String
                       -> IO ()
readval readsfn f str
    =  case thing of
           []    -> usage defaultArgs
           (_:_) -> f val (if str' == "" then str' else (tail str'))
       where
       thing        =  readsfn str
       (val, str')  =  head thing

usage :: Args -> IO ()
usage (MkArgs s nt dt nv dv de mn _)
    =  hPutStr stderr
       ("Usage: hpg [-s (Int,Int,Int)] [-nt Int] [-dt Int] \
                   \[-nv Int] [-dv Int] [-de Int] [-m String] [-o String]\n\
        \    -s   random number generator seeds (default " ++ show s ++ ")\n\
        \    -nt  number of types to be generated (" ++ show nt ++ ")\n\
        \    -dt  depth of generated types (" ++ show dt ++ ")\n\
        \    -nv  number of values to be generated (" ++ show nv ++ ")\n\
        \    -dv  depth of values to be generated (" ++ show dv ++ ")\n\
        \    -de  depth of expressions to be generated (" ++ show de ++ ")\n\
        \    -m   output module name (" ++ mn ++ ")\n\
        \    -o   output file name (stdout)\n")
