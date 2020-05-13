
module Build_Tm where

import Unparse

import Kernel

import Sub_Core1

import Sub_Core2

import Sub_Core3

import Sub_Core4

import Type_defs

import Core_datatype

import Vtslib

build_trm :: Sgn -> Flagged_ITrm -> Trm

build_trm sg ( Opnd ( Itrm itm ))

build_trm ( SG sg ) oth

build_trm' sg ( Sym i j tyL attL )
	= add_l ( symbol sg i j ) tyL attL

build_trm' sg ( App itm1 itm2 tyL attL )

build_trm' sg ( Pair itm1 itm2 itm3 tyL attL )

build_trm' sg ( Binder bdr idc itm tyL attL )

build_trm' sg ( Constant cst tyL attL )

build_trm' sg ( Binary' b_op itm1 itm2 tyL attL )

build_trm' sg ( Unary Not itm tyL attL )

build_trm' sg ( Cond idc itm1 itm2 tyL attL )

build_trm' sg ( Const i j k tyL attL )
	= add_l ( constructor sg i j k ) tyL attL

build_trm' sg ( Recurse itmL itm tyL attL )

build_trm' sg ( Tagid ( str , _ , cnv_fnL ) argL )
      = case fetch_fn cnv_fnL of
              Ok cnv_fn -> cnv_fn argL
              Bad mesg  -> TM_Err mesg
        where

        fetch_fn ( Trm_Fn fn : _ ) = Ok fn
        fetch_fn ( _ : oth )       = fetch_fn oth
        fetch_fn []                = Bad ( "cannot convert tag " ++ str ++ " to term" )

build_trm' ( SG isg ) oth

build_dc = gen_dc

gen_dc sg ( Symbol_dec itm attL )

gen_dc sg ( Axiom_dec itm attL )

gen_dc sg ( Decpair idc1 idc2 attL )

gen_dc _ _ = error "Only symbol_dec so far implemented"

add_l ( TM_Err mesg ) _ _ = TM_Err mesg

add_l tm tyL@( srt : _ ) attL

add_l tm [] attL

build_sg :: ISgn -> Sgn

build_sg ( Empty _ ) = empty

build_sg ( Extend idc isg _ )

build_sg _ = error "unimplemented signature constructor"
