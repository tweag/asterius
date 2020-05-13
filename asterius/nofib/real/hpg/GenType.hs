
module GenType (
    gen_types
    ) where

import Utils
import Config
import Types
import Env

gen_types :: Int -> Int -> Int -> Int -> Cont -> Cont
gen_types vnts flds n dp c
    =  get_type_names n
       (\tns -> get_all_type_names
       (\tns' -> cmap (rep n (gen_type vnts flds (tns'++tns) dp))
       (\ts -> extend_type_env (zip tns ts) c)))

gen_type :: Int -> Int -> [Type_name] -> Int -> Xcont Htype -> Cont
gen_type vnts flds tns dp tc
    =  upto (vnts-one)
       (\n -> get_constructors (n + one)
       (\cs -> cmap (rep n (gen_argtypes flds tns dp))
       (\as -> tc (zip cs ([]:as)))))

gen_argtypes :: Int -> [Type_name] -> Int -> Xscont Argtype -> Cont
gen_argtypes flds tns dp asc
    =  upto flds (\n -> cmap (rep n (gen_argtype tns dp)) asc)

gen_argtype :: [Type_name] -> Int -> Xcont Argtype -> Cont
gen_argtype tns dp ac | dp <= one  =  gen_base ac
gen_argtype tns dp ac
    =  choose [upto (length tns - one) (\n -> ac (Name_type (tns !! n))),
               gen_base ac,
               gen_argtype tns (dp - one) (\a -> ac (List_type a)),
               upto max_tuple_len
                 (\r -> cmap (rep r (gen_argtype tns (dp - one)))
                 (\as -> ac (Tuple_type as))),
               gen_ix_type tns (dp - one)
                 (\a -> gen_argtype tns (dp - one)
                 (\a' -> ac (Array_type a a')))] id


gen_base :: Xcont Argtype -> Cont
gen_base ac
    =  choose [choose [Int_type, Integer_type, Float_type, Double_type]
                      (\k -> ac (Basic_type (Num_type k))),
               ac (Basic_type Bool_type),
               ac (Basic_type Char_type)] id

gen_ix_type :: [Type_name] -> Int -> Xcont Argtype -> Cont
gen_ix_type tns dp ac | dp <= one  =  gen_ix_base ac
gen_ix_type tns dp ac
    =  choose [gen_ix_base ac,
               upto max_tuple_len
                 (\r -> cmap (rep r (gen_ix_type tns (dp-one)))
                 (\as -> ac (Tuple_type as)))] id

gen_ix_base :: Xcont Argtype -> Cont
gen_ix_base ac
    =  choose [choose [Int_type, Integer_type]
                      (\k -> ac (Basic_type (Num_type k))),
               ac (Basic_type Bool_type),
               ac (Basic_type Char_type)] id
