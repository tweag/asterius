
module GenVal (
    gen_vals, gen_id_val
    ) where

import Utils
import Config
import Types
import Env

gen_vals :: Int -> Int -> Cont -> Cont
gen_vals n dp c
    =  get_all_type_names
       (\tns -> cmap (rep n (upto (length tns - one)
                . (\vc r -> gen_val (tns !! r) dp vc)))
       (\vs -> get_val_names n
       (\vns -> extend_val_env (zip vns vs) c)))

gen_val :: Type_name -> Int -> Vcont -> Cont
gen_val tn dp vc
    | dp <= one =  get_type tn (\t -> vc (Tagged_val (fst (head t)) []))
    | otherwise =  get_type tn tc
                   where
                   tc t  =  upto (length t - one) nc
                            where
                            nc r  =  cmap (map (gen_id_val (dp - one)) ids)
                                     (\vs -> vc (Tagged_val c' vs))
                                     where
                                     (c', ids)  =  t !! r

gen_id_val :: Int -> Argtype -> Vcont -> Cont
gen_id_val dp (Name_type tn) vc  =  gen_val tn dp vc
gen_id_val dp (Basic_type bt) vc  =  gen_basic_val bt vc
gen_id_val dp (List_type id) vc
    =  upto max_list_len
       (\r -> cmap (rep r (gen_id_val (dp - one) id))
       (\vs -> vc (List_val vs)))
gen_id_val dp (Tuple_type ids) vc
    =  cmap (map (gen_id_val (dp - one)) ids)
       (\vs -> vc (Tuple_val vs))
gen_id_val dp (Array_type id id') vc
    =  let vsc [v,v']
               =  upto (max_array_len-one)
                  (\r -> let vsc' vs  =  vc (Array_val (v,v'')
                                                (zipWith (,) vrng vs))
                             vrng     =  take (r+one) (vrange (v,v'))
                             v''      =  case vrng of
                                             []    -> v'
                                             (_:_) -> last vrng
                         in
                             cmap (rep (length vrng)
                                  (gen_id_val (dp-one) id')) vsc')
       in
           cmap (rep 2 (gen_id_val (dp - one) id)) vsc

gen_basic_val :: Base_type -> Vcont -> Cont
gen_basic_val (Num_type k) vc
    =  upto maxint (\r -> vc (Num_val k r))
       where
       maxint  =  256 :: Int
gen_basic_val Bool_type vc
    =  choose [True, False] (\b -> vc (Bool_val b))
gen_basic_val Char_type vc
    =  upto maxchar (\r -> vc (Char_val (toEnum r)))
