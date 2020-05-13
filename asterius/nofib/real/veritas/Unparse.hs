
module Unparse(unparse_tm , unparse_th , unparse , unparse' , disp_tk , unparse_sg , unparse_sg' , unparse_nm , unparse_trm , unparse_Trm , unparse_sgn , unparse_Sgn , unparse_Dec , unparse_dec , unparse_Thm ) where

import Kernel

import Type_defs

import Core_datatype

import Attributes

unparse_trm ( SG isg ) _ = unparse' isg

unparse_Trm _ _ ( TM itm _ isg ) = unparse' isg itm

unparse_sgn _ _ isg  = unparse_sg' isg

unparse_Sgn _ _ ( SG isg )  = unparse_sg' isg

unparse_dec ( SG isg ) _ idc = unparse_dc isg idc

unparse_Dec _ _ ( DC idc isg ) = unparse_dc isg idc

unparse_Thm _ _ ( TH itm isg ) = unparse' isg itm

unparse_th ( TH tm sg )

unparse_th ( TH_Err mesg )

unparse_tm ( TM tm1 tm2 sg )

unparse_tm ( TM_Err mesg ) = "\nTerm formation error\n" ++ mesg ++ "\n"

unparse sg ( Opnd ( Itrm tm ))

unparse sg ( Opnd ( Idec dc ))

unparse _ ( Prs_Err mess ) = mess ++ "\n"

unparse sg ( Opr ( OpItrm tm ) tpe prc )

unparse _ ( Opr (Spl "") _ _ ) = "unparse operator"

unparse sg ( Opnd ( TypApp tm )) = "Unparse TypApp: " ++ unparse sg tm

unparse sg ( Opr ( Spl "typed" ) _ _ ) = "Unparse 'typed'"

unparse sg oth = "unparse unknown"

unparse' sg ( Sym n1 n2 _ att )

unparse' sg ( Const i j k _ _ )
	= lookUp sg i j k

unparse' sg ( App tm1 tm2 _ _ )

unparse' sg ( Pair tm1 tm2 tm3 _ _ )

unparse' sg ( Binder Subtype dc tm _ _ )

unparse' sg ( Binder Imp dc tm _ _ )

unparse' sg ( Binder con dc tm _ _ )

unparse' sg ( Constant con _ _ )
	= unparse_constant con

unparse' sg ( Unary con tm _ _ )

unparse' sg ( Binary' con tm1 tm2 _ _ )

unparse' sg ( Cond dc tm1 tm2 _ _ )

unparse' sg ( Recurse tml srt _ _ )

unparse' sg ( Tagid ( nm , _ , _ ) argL )

unparse' sg ( ITrm_Err mesg ) = mesg

unparse_bdr Lambda = "\236"

unparse_bdr Forall = "\177"

unparse_bdr Exists = "\178"

unparse_bdr Pi = "\208"

unparse_bdr Sigma = "\211"

unparse_bdr Choose = "\229"

unparse_bdr Delta = "\196"

unparse_dc sg ( Symbol_dec tm attL )

unparse_dc sg ( Axiom_dec tm attL )

unparse_dc sg ( Decpair dc1 dc2 _ )

unparse_dc sg ( Data dcl ctrs attL )

unparse_dc sg ( Def tm srt attL )

unparse_dc sg _

unparse_fmls sg ( dc : dcl )

unparse_fmls _ [] = ""

unparse_ctrs sg ( nm : nml ) ( ctr : ctrl )

unparse_ctrs _ [] [] = ""

unparse_ctr sg ( arg : argl )

unparse_ctr _ [] = ""

unparse_nm ( Name nm )

unparse_nm ( Operator' op prc tpe )

show_tpe Pre = "Pre"

show_tpe BinL = "BinL"

show_tpe BinR = "BinR"

show_tpe Post = "Post"

unparse_bcon :: Binary_conn -> String

unparse_bcon Or = "\180"

unparse_bcon And = "\179"

unparse_bcon Eq' = "="

unparse_bcon Issubtype = "\172"

unparse_constant T = "True"

unparse_constant F = "False"

unparse_constant Bool' = "Bool"

unparse_constant ( Univ i ) = "U" ++ show i

lookUp ( Extend dc sg _ ) 0 i2 i3

lookUp ( Extend dc sg _ ) i1 i2 i3 | i1 > 0

lookUp _ _ _ _ = "symbol not found "

lookup_dc ( Symbol_dec tm attL ) _ 0 _

lookup_dc ( Axiom_dec tm attL ) _ 0 _

lookup_dc ( Decpair dc1 dc2 attL ) dcl i k | i > 0

lookup_dc ( Data _ _ attL ) _ 0 k

lookup_dc ( Def _ _ attL ) _ 0 _

lookup_dc _ ( dc : dcl ) i k | i > 0

lookup_dc _ _ _ _ = " symbol not found "

lookup_ctr ( nm : nml ) k | k > 0

lookup_ctr ( nm : nml ) 0

lookup_ctr [] _

disp_tk :: Token -> String

disp_tk ( Rvd str ) = str

disp_tk ( Clr str ) = str

disp_tk ( Bdr bdr ) = unparse_bdr bdr

disp_tk ( IfxBdr str ) = str

disp_tk ( IfxOp str ) = str

disp_tk ( Scan_Err str ) = "Invalid token: " ++ str -- have to change this?

unparse_sg :: Flagged_ITrm -> String

unparse_sg ( Opnd ( Isgn sg )) = unparse_sg' sg

unparse_sg ( Prs_Err mesg ) = mesg

unparse_sg' ( Empty _ ) = ""

unparse_sg' ( Extend dc sg _ )
