
module GenExp (
    gen_exps
    ) where

import Config
import Types
import Env
import Utils
import GenVal

gen_exps :: Int -> Xscont (Val_name, Expression) -> Cont
gen_exps dp vnesc
    =  get_all_val_decls
       (\vds -> cmap [gen_exp dp v . c1 vn | (vn,v) <- vds] vnesc)
       where
       c1 vn vnec e  =  vnec (vn,e)

type Genfn x   =  Int -> x -> Econt -> Cont
type Fnlist x  =  [(Int, (Genfn x, x -> Bool))]

gen_exp :: Genfn Value
gen_exp dp v ec | dp <= one  =  ec (Val_exp v)
                | otherwise  =  pick_exp general_fns dp v ec

general_fns :: Fnlist Value
general_fns  =  [(1, (gen_exp',ok)), (1, (gen_lambda_exp,ok))]

gen_exp' :: Genfn Value
gen_exp' dp n@(Num_val Int_type _) ec      =  pick_exp intfns dp n ec
gen_exp' dp n@(Num_val Integer_type _) ec  =  pick_exp integerfns dp n ec
gen_exp' dp n@(Num_val Float_type _) ec    =  pick_exp floatfns dp n ec
gen_exp' dp n@(Num_val Double_type _) ec   =  pick_exp doublefns dp n ec
gen_exp' dp (Bool_val b) ec       =  pick_exp boolfns dp b ec
gen_exp' dp (Char_val c) ec       =  pick_exp charfns dp c ec
gen_exp' dp (List_val vs) ec      =  pick_exp listfns dp vs ec
gen_exp' dp (Tuple_val vs) ec     =  pick_exp tuplefns dp vs ec
gen_exp' dp (Tagged_val c vs) ec  =  pick_exp taggedfns dp (c,vs) ec
gen_exp' dp (Array_val vp avs) ec =  pick_exp arrayfns dp (vp,avs) ec

gen_plus_exp :: Genfn Value
gen_plus_exp dp (Num_val k n)
    =  binary_exp plus_name (dp-one) (Num_val k 1) (Num_val k (n-one))

gen_minus_exp :: Genfn Value
gen_minus_exp dp (Num_val k n)
    =  binary_exp minus_name (dp-one) (Num_val k (n+one)) (Num_val k 1)

gen_mult_exp :: Genfn Value
gen_mult_exp dp n'@(Num_val k n)
    =  binary_exp mult_name (dp-one) n' (Num_val k 1)

gen_div_exp :: Genfn Value
gen_div_exp dp n'@(Num_val k n)
    =  binary_exp div_name (dp-one) n' (Num_val k 1)

div_filter :: Value -> Bool
div_filter (Num_val _ n)  =  n /= 0

gen_neg_exp :: Genfn Value
gen_neg_exp dp (Num_val k n)
    =  unary_exp negate_name (dp-one) (Num_val k (negate n))

gen_int_id_exp :: Genfn Value
gen_int_id_exp dp n'@(Num_val k n) ec
    =  get_all_lambdas (\vvs ->
       case vvs of
           []    -> gen_plus_exp dp n' ec
           (_:_) -> choose vvs (\(vn, Num_val Int_type n'') ->
                    gen_exp (dp-one) (int_val (n-n'')) (\e ->
                    ec (Apply_exp (Apply_exp (Id_exp plus_name) (Id_exp vn))
                                  e))))

intfns :: Fnlist Value
intfns  =  [(1, (gen_plus_exp,ok)), (1, (gen_minus_exp,ok)),
            (1, (gen_mult_exp,ok)), (1, (gen_div_exp,div_filter)),
            (1, (gen_neg_exp,ok)),  (1, (gen_int_id_exp,ok))]

integerfns :: Fnlist Value
integerfns  =  intfns

floatfns :: Fnlist Value
floatfns  =  [(1, (gen_plus_exp,ok)), (1, (gen_minus_exp,ok)),
              (1, (gen_mult_exp,ok)), (1, (gen_neg_exp,ok)),
              (1, (gen_int_id_exp,ok))]

doublefns :: Fnlist Value
doublefns  =  floatfns

gen_not_exp :: Genfn Bool
gen_not_exp dp b  =  unary_exp not_name (dp-one) (Bool_val (not b))

gen_and_exp :: Genfn Bool
gen_and_exp dp b   =  bool_binary_exp and_name (dp-one) True b

gen_or_exp :: Genfn Bool
gen_or_exp dp b   =  bool_binary_exp or_name (dp-one) False b

gen_less_exp :: Genfn Bool
gen_less_exp dp True   =  int_binary_exp less_name (dp-one) 0 1
gen_less_exp dp False  =  int_binary_exp less_name (dp-one) 1 1

boolfns :: Fnlist Bool
boolfns  =  [(1, (gen_not_exp,ok)), (1, (gen_and_exp,ok)),
             (1, (gen_or_exp,ok)),  (1, (gen_less_exp,ok))]

gen_decode_exp :: Genfn Char
gen_decode_exp dp c  =  unary_exp int_to_char_name (dp-one) (int_val (fromEnum c))

charfns :: Fnlist Char
charfns  =  [(1, (gen_decode_exp,ok))]

gen_list_exp :: Genfn [Value]
gen_list_exp dp vs ec  =  cmap (map (gen_exp (dp-one)) vs)
                          (\es -> ec (List_exp es))

gen_drop_exp :: Genfn [Value]
gen_drop_exp dp vs
    =  binary_exp drop_name (dp-one) (int_val (length vs)) (List_val (vs++vs))

gen_take_exp :: Genfn [Value]
gen_take_exp dp vs
    =  binary_exp take_name (dp-one) (int_val (length vs)) (List_val (vs++vs))

listfns :: Fnlist [Value]
listfns  =  [(8, (gen_list_exp,ok)), (1, (gen_drop_exp,ok)),
             (1, (gen_take_exp,ok))]

gen_tuple_exp :: Genfn [Value]
gen_tuple_exp dp vs ec  =  cmap (map (gen_exp dp) vs)
                           (\es -> ec (Tuple_exp es))

tuplefns :: Fnlist [Value]
tuplefns  =  [(1, (gen_tuple_exp,ok))]

gen_tagged_exp :: Genfn (Constructor, [Value])
gen_tagged_exp dp (c,vs) ec  =  cmap (map (gen_exp dp) vs)
                                (\es -> ec (Tagged_exp c es))

taggedfns :: Fnlist (Constructor, [Value])
taggedfns  =  [(1, (gen_tagged_exp,ok))]

type Assoc a b = (a,b)

gen_array_exp :: Genfn ((Value,Value), [Assoc Value Value])
gen_array_exp dp ((v,v'), avs) ec
    =  gen_exp (dp-one) (Tuple_val [v,v'])
       (\e -> cmap [binary_exp assoc_name (dp-one) v1 v2
                        | (v1, v2) <- avs]
       (\es -> ec (Array_exp e (List_exp es))))

arrayfns :: Fnlist ((Value,Value), [Assoc Value Value])
arrayfns  =  [(1, (gen_array_exp,ok))]

gen_lambda_exp :: Genfn Value
gen_lambda_exp dp v ec
    = get_val_names one (\[vn] ->
      gen_id_val (dp-one) (Basic_type (Num_type Int_type)) (\v' ->
      push_lambda (vn, v') (
      gen_exp (dp-one) v (\e ->
      pop_lambda (\ _ ->
      choose [extend_val_env [(vn,v')] (ec (Apply_exp (Lambda_exp vn e)
                                                      (Id_exp vn))),
              gen_exp (dp-one) v'
                      (\e' -> ec (Apply_exp (Lambda_exp vn e) e'))]
             id)))))

pick_exp :: [(Int, (Genfn x, x -> Bool))] -> Genfn x
pick_exp fs dp x ec
    =  choosew fs (\(f,p) ->
           if p x then f dp x ec else pick_exp fs dp x ec)

unary_exp :: Val_name -> Genfn Value
unary_exp vn dp v ec  =  gen_exp dp v (\e -> ec (Apply_exp (Id_exp vn) e))

binary_exp :: Val_name -> Int -> Value -> Value -> Econt -> Cont
binary_exp vn dp v v' ec
    =  unary_exp vn dp v
       (\e -> gen_exp dp v'
       (\e' -> ec (Apply_exp e e')))

int_binary_exp :: Val_name -> Int -> Int -> Int -> Econt -> Cont
int_binary_exp vn dp n n'   =  binary_exp vn dp (int_val n) (int_val n')

bool_binary_exp :: Val_name -> Int -> Bool -> Bool -> Econt -> Cont
bool_binary_exp vn dp b b'  =  binary_exp vn dp (Bool_val b) (Bool_val b')

ok :: x -> Bool
ok x  =  True
