
module Types (
    Type_decl, Htype, Argtype(..), Base_type(..), Num_kind(..),
    Constructor, Type_name, Val_decl, Value(..), Val_name,
    Expression(..),
    showsType_decl, showsVal_decl, vrange,
    int_val,
    sep_list
    ) where

import Data.Char (isAlpha) -- 1.3
import Config

type Type_decl    =  (Type_name, Htype)

type Htype        =  [(Constructor, [Argtype])]

data Argtype      =  Name_type  Type_name
                  |  Basic_type Base_type
                  |  List_type  Argtype
                  |  Tuple_type [Argtype]
                  |  Array_type Argtype Argtype
     deriving (Eq)

data Base_type    =  Num_type Num_kind | Bool_type | Char_type
     deriving (Eq)

data Num_kind     =  Int_type | Integer_type | Float_type | Double_type
     deriving (Eq)

type Constructor  =  String
type Type_name    =  String

instance Show Argtype where
    showsPrec _ (Name_type tn)    =  showString tn
    showsPrec _ (Basic_type bt)   =  shows bt
    showsPrec _ (List_type at)    =  lsq . shows at . rsq
    showsPrec _ (Tuple_type ats)
        =  lbrack . sep_list list_separator shows ats . rbrack
    showsPrec d (Array_type at at')
        =  showParen (d >= apply_prec)
           (array_type_name . space . showsPrec apply_prec at
            . space . showsPrec apply_prec at')

instance Show Base_type where
    showsPrec _ (Num_type k)  =  shows k
    showsPrec _ Bool_type     =  bool_type_name
    showsPrec _ Char_type     =  char_type_name

instance Show Num_kind where
    showsPrec _ Int_type      =  int_type_name
    showsPrec _ Integer_type  =  integer_type_name
    showsPrec _ Float_type    =  float_type_name
    showsPrec _ Double_type   =  double_type_name

showsType_decl :: Type_decl -> ShowS
showsType_decl (tn, ht)
    =  data_name . space . showString tn . type_def
       . sep_list union showsVnt ht . space . derive_eq
       where
       showsVnt (c, ats)  =  showString c . field_separator
                             . sep_list field_separator
                                        (showsPrec apply_prec) ats

type Val_decl  =  (Val_name, Value)

data Value     =  Num_val    Num_kind Int
               |  Bool_val   Bool
               |  Char_val   Char
               |  List_val   [Value]
               |  Tuple_val  [Value]
               |  Tagged_val Constructor [Value]
               |  Array_val  (Value,Value) [(Value, Value)]
     deriving (Eq)

type Val_name  =  String

instance Show Value where
    showsPrec d (Num_val Int_type n)      =  showsPrec d n
    showsPrec d (Num_val Integer_type n)  =  showsPrec d n
    showsPrec d (Num_val Float_type n)    =  showsPrec d (int_to_float n)
    showsPrec d (Num_val Double_type n)   =  showsPrec d (int_to_double n)
    showsPrec d (Bool_val b)              =  showsPrec d b
    showsPrec d (Char_val c)              =  showsPrec d c
    showsPrec _ (List_val vs)
        =  lsq . sep_list list_separator shows vs . rsq
    showsPrec _ (Tuple_val vs)
        =  lbrack . sep_list list_separator shows vs . rbrack
    showsPrec d (Tagged_val c vs)
        =  showParen (d >= apply_prec)
           (showString c . field_separator
           . sep_list field_separator (showsPrec apply_prec) vs)
    showsPrec d (Array_val (v,v') avs)
        =  showParen (d >= apply_prec)
           (array_name . space . (showsPrec apply_prec (Tuple_val [v,v']))
            . space . showsPrec apply_prec avs)

showsVal_decl :: Val_decl -> ShowS
showsVal_decl (vn, v)  =  showString vn . val_def . shows v

vrange :: (Value,Value) -> [Value]
vrange (Num_val Int_type n, Num_val Int_type n')
    =  [int_val r | r <- [n..n']]
vrange (Num_val Integer_type n, Num_val Integer_type n')
    =  [Num_val Integer_type r | r <- [n..n']]
vrange (Bool_val b, Bool_val b')      =  [Bool_val r | r <- [b..b']]
vrange (Char_val c, Char_val c')      =  [Char_val r | r <- [c..c']]
vrange (Tuple_val vs, Tuple_val vs')  =
    [Tuple_val vs'' | vs'' <- f (map vrange (zip vs vs'))]
    where
    f []        =  [[]]
    f (vs:vss)  =  [(i:is) | i <- vs, is <- f vss]
vrange x                              =  error ("Error: vrange called on "
                                                ++ show x)

int_to_float :: Int -> Float
int_to_float n  =  fromIntegral n

int_to_double :: Int -> Double
int_to_double n  =  fromIntegral n

int_val :: Int -> Value
int_val n  =  Num_val Int_type n

data Expression  =  Apply_exp  Expression Expression
                 |  Id_exp     Val_name
                 |  Val_exp    Value
                 |  Tagged_exp Constructor [Expression]
                 |  List_exp   [Expression]
                 |  Tuple_exp  [Expression]
                 |  Lambda_exp Val_name Expression
                 |  Array_exp  Expression Expression
     deriving (Eq)

instance Show Expression where
    showsPrec d (Apply_exp e1 e2)
        =  showParen (d > apply_prec)
           (showsPrec apply_prec e1 . space . showsPrec (apply_prec+one) e2)
    showsPrec _ (Id_exp vn)
        =  showParen (is_op vn) (showString vn)
           where
           is_op (c:cs)  =  not (isAlpha c)
    showsPrec d (Val_exp v)        =  showsPrec d v
    showsPrec d (Tagged_exp c es)
        =  showParen (d > apply_prec)
           (showString c . field_separator
            . sep_list field_separator (showsPrec (apply_prec+one)) es)
    showsPrec _ (List_exp es)
        =  lsq . sep_list list_separator shows es . rsq
    showsPrec _ (Tuple_exp es)
        =  lbrack . sep_list list_separator shows es . rbrack
    showsPrec _ (Lambda_exp vn e)
        =  lbrack . lambda_name . showString vn . space . map_name . space
           . shows e . rbrack
    showsPrec d (Array_exp e e')
        =  showParen (d > apply_prec)
           (array_name . space . showsPrec (apply_prec+one) e . space
            . showsPrec (apply_prec+one) e')

sep_list :: ShowS -> (a -> ShowS) -> [a] -> ShowS
sep_list s f []  =  empty_string
sep_list s f xs  =  foldr1 g (map f xs)
                    where
                    g x y  =  x . s . y
