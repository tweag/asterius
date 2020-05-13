
module Print where

import LambdaLift
import Utilities

pprint :: (binder -> Iseq) -> Expr binder -> [Char]
pprint pb e = i_mkstr (ppr pb e)

pprintExpr = pprint i_str

pprintLevel :: (Expr ([Char], Integer)) -> [Char]

pprintLevel = pprint (\(name,level) -> i_concat [ i_str name, i_str "{",

pprintSCs scs = i_mkstr (i_concat (map ppsc scs))
ppsc (name, args, rhs) =

ppr pb (EAp e1 e2) = i_concat [ppr pb e1, i_space, ppr_atomic pb e2]
ppr pb (ELet isrec defs e) =
  i_concat [	i_str keyword, i_newline,

  ]
  where
  keyword | isrec     = "letrec"

ppr pb (ELam args body) =
  i_concat [ i_str "\\[",


ppr pb e = ppr_atomic pb e

ppr_atomic pb (EConst (CNum n)) 	= i_num n
ppr_atomic pb (EConst (CFun name))	= i_str name
ppr_atomic pb (EConst (CBool b))	= i_str (show b)
ppr_atomic pb (EVar v) 		= i_str v
ppr_atomic pb e = i_concat [i_str "(", ppr pb e, i_str ")"]

ppr_def pb (binder, (ELam args body)) =
  i_concat [	pb binder, i_space,

  ]
ppr_def pb (binder, rhs) = i_concat [pb binder, i_str " = ", ppr pb rhs]


type Iseq = Oseq -> Oseq

i_concat :: [Iseq] -> Iseq
i_concat = foldr i_append i_nil

i_interleave :: Iseq -> [Iseq] -> Iseq
i_interleave is []  = i_nil
i_interleave is iss = foldr1 glue iss

                            foldr1 f [x] = x

i_num :: (Show a, Num a) => a -> Iseq
i_num = i_str . show

i_newline 	= i_str "\n"
i_space 	= i_str " "

type Oseq = Int -> Bool -> [Char]

o_empty :: Oseq              -- An empty oseq
o_empty indent npend = []

o_mkstr :: Oseq -> [Char]
o_mkstr oseq = oseq 0 False

i_nil x = x
i_append = (.)
i_str = foldr (i_append . i_char) i_nil
i_mkstr iseq = o_mkstr (iseq o_empty)

i_char :: Char -> Iseq
i_char '\n' rest indent npend = '\n' : rest indent True
i_char c    rest indent False	= c    : rest indent False
i_char c    rest indent True	= pspaces indent (c : rest indent False)

i_indent n iseq oseq indent npend =
  iseq oseq' (indent+n) npend
  where
  oseq' indent' npend' = oseq indent npend'
  -- Ignore the indent passed along to oseq;
  -- use the original indent instead.

pspaces 0 cs = cs
pspaces n cs = ' ' : pspaces (n-1) cs
