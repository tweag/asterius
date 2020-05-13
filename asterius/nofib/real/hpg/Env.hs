
module Env (
    Cont, Ncont, Econt, Vcont, Xcont, Xscont,
    Answer,
    Env, make_Env,
    upto, choose, choosew,
    Output, default_output,
    get_constructors, get_val_names, get_type_names, get_all_type_names,
    get_all_type_decls, get_all_val_decls, get_all_lambdas, get_type,
    get_output,
    push_lambda, pop_lambda, extend_type_env, extend_val_env, set_output
    ) where

import Config
import Types
import System.IO

type Cont      =  Env -> Answer

type Xcont x   =  x -> Cont
type Xscont x  =  [x] -> Cont

type Ncont     =  Xcont Int
type Econt     =  Xcont Expression
type Vcont     =  Xcont Value

type Answer    =  IO () --Dialogue

random_numbers :: (Int, Int, Int) -> [Float]
random_numbers (s1,s2,s3)
    =  map (snd . properFraction . combine) (iterate f (s1,s2,s3))
       where
       combine :: (Int,Int,Int) -> Float
       combine (a,b,c)  =
            fromIntegral(a)/30269 + fromIntegral(b)/30307
            + fromIntegral(c)/30323
       f (a,b,c)  =
           ((171*a) `mod` 30269, (172*b) `mod` 30307, (170*c) `mod` 30323)

upto :: Int -> Ncont -> Cont
upto n nc (MkEnv (r:rs) cs ts vs te ve le op)
    =  seq x $ nc x (MkEnv rs cs ts vs te ve le op) -- Disabled printing for perf measurement.
--  =  hPutStr stderr (show x ++ " ") >> nc x (MkEnv rs cs ts vs te ve le op)
--  =  appendChan stderr (show x ++ " ") exit ((nc x) (MkEnv rs cs ts vs te ve le op))
       where
       x :: Int
       x  =  fromIntegral (fst (properFraction (r * (fromIntegral (n+one)))))

choose :: [x] -> (Xcont x) -> Cont
choose xs xc  =  upto (length xs - one) (\n -> xc (xs !! n))

choosew :: [(Int,x)] -> (Xcont x) -> Cont
choosew ps xc
    =  upto (sum [i | (i,x) <- ps] - one) (f ps)
       where
       f ((i,x):ps') n | n < i  =  xc x
                       | True   =  f ps' (n-i)

idnum :: String -> [String]
idnum id  =  [id ++ show n | n <- [one..]]

constructors :: [Constructor]
constructors  =  idnum constructor_name

type_names :: [Type_name]
type_names    =  idnum type_name

val_names :: [Val_name]
val_names     =  idnum val_name

type Type_env  =  [Type_decl]

initial_type_env :: Type_env
initial_type_env  =  []

type Val_env  =  [Val_decl]

initial_val_env :: Val_env
initial_val_env  =  []

type Lambda_env  =  [(Val_name, Value)]

initial_lambda_env :: Lambda_env
initial_lambda_env  =  []

--type Output  =  String -> FailCont -> SuccCont -> Dialogue
type Output  =  String -> IO ()

default_output :: Output
default_output  str =  putStr str

set_output :: String -> Output
set_output str  =  appendFile str

data Env  =  MkEnv [Float] [Constructor] [Type_name] [Val_name]
                   Type_env Val_env Lambda_env Output

make_Env :: (Int,Int,Int) -> Output -> Env
make_Env (s1,s2,s3) op  =  MkEnv (random_numbers (s1,s2,s3))
                              constructors type_names val_names
                              initial_type_env initial_val_env
                              initial_lambda_env op

get_constructors :: Int -> (Xscont Constructor) -> Cont
get_constructors n csc (MkEnv rs cs tns vns te ve le op)
    =  csc (take n cs) (MkEnv rs (drop n cs) tns vns te ve le op)

get_val_names :: Int -> (Xscont Val_name) -> Cont
get_val_names n vnsc (MkEnv rs cs tns vns te ve le op)
    =  vnsc (take n vns) (MkEnv rs cs tns (drop n vns) te ve le op)

get_type_names :: Int -> (Xscont Type_name) -> Cont
get_type_names n tnsc (MkEnv rs cs tns vns te ve le op)
    =  tnsc (take n tns) (MkEnv rs cs (drop n tns) vns te ve le op)

get_all_type_names :: (Xscont Type_name) -> Cont
get_all_type_names tnsc e@(MkEnv _ _ _ _ te _ _ _)
    =  tnsc [tn | (tn,t) <- te] e

get_all_type_decls :: (Xscont Type_decl) -> Cont
get_all_type_decls tdsc e@(MkEnv _ _ _ _ te _ _ _)
    = tdsc te e

get_all_val_decls :: (Xscont Val_decl) -> Cont
get_all_val_decls vdsc e@(MkEnv _ _ _ _ _ ve _ _)
    = vdsc ve e

get_all_lambdas :: (Xcont Lambda_env) -> Cont
get_all_lambdas lec e@(MkEnv _ _ _ _ _ _ le _)
    =  lec le e

get_type :: Type_name -> (Xcont Htype) -> Cont
get_type tn tc e@(MkEnv _ _ _ _ te _ _ _)
    =  tc (head [t | (tn',t) <- te , tn == tn']) e

get_output :: (Xcont Output) -> Cont
get_output oc e@(MkEnv _ _ _ _ _ _ _ op)  =  oc op e

extend_type_env :: [Type_decl] -> Cont -> Cont
extend_type_env tds c (MkEnv rs cs tns vns te ve le op)
    =  c (MkEnv rs cs tns vns (tds++te) ve le op)

extend_val_env :: [Val_decl] -> Cont -> Cont
extend_val_env vds c (MkEnv rs cs tns vns te ve le op)
    =  c (MkEnv rs cs tns vns te (vds++ve) le op)

push_lambda :: (Val_name, Value) -> Cont -> Cont
push_lambda (vn,v) c (MkEnv rs cs tns vns te ve le op)
    =  c (MkEnv rs cs tns vns te ve ((vn,v):le) op)

pop_lambda :: Xcont (Val_name,Value) -> Cont
pop_lambda lc (MkEnv rs cs tns vns te ve ((vn,v):le) op)
    =  lc (vn,v) (MkEnv rs cs tns vns te ve le op)
