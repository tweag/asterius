
module Parse where

import Data.Char(isDigit)--1.3
import Token

import Unparse

import Build_Tm

import Build_itrm

import Core_datatype

import Type_defs

import Attributes

import Kernel

import Sub_Core1

import Sub_Core2

import Sub_Core3

import Sub_Core4

import Vtslib

type Parse_State = ( [ Tag ] , Sgn )

parse_trm sg tgL s
       = case parse_itm ( tgL , sg ) s of
             ( Opnd ( Itrm itm )) -> Ok itm
             Prs_Err mesg         -> Bad mesg

parse_Thm_M sg ps str

parse_Thm sg ps = parse_deriv (ps, sg) . parse_itm (ps, sg)

parse_Trm sg ps = parse_tm ( ps , sg )

trm_to_Trm = build_trm'

dec_to_Dec = build_dc

sgn_to_Sgn = build_sg

parse_tm (tgL , sg ) str

parse_itm sg str

drive_parse :: Parse_State -> String -> ( Flagged_ITrm , [Token] )

drive_parse sg str

term :: Parse_State -> [Token] -> [Token] -> (Flagged_ITrm , [Token])

term sg tmnL tkL

term' :: Parse_State -> [Token] -> [Token] -> ([Flagged_ITrm] , [Token])

term' sg tmnL ( Rvd "if" : tkL )
	= ( [ cond_tm ] , tkL4 )

term' sg tmnL ( Rvd "let" : tkL )

term' sg tmnL ( Rvd "recurse" : tkL )

term' sg tmnl ( Rvd "fn" : tkl )

term' sg tmnL ( Rvd "\185" : tkL )

term' sg tmnL ( Bdr nm : tkL )

term' sg tmnL ( Rvd "," : tkL )

term' sg tmnL ( Rvd "typed" : tkL )

term' sg tmnL ( Rvd "\168": Clr no1 : Rvd ",": Clr no2 : Rvd "\169": tkL )

term' sg tmnL ( Rvd "\168" : Clr no1 : Rvd "," : Clr no2 : Rvd "," :

	= ( const' no1 no2 no3 : tmL , tkL2 )

term' sg tmnL	 ( Rvd "{" : tkL )

term' sg tmnL ( Rvd "[" : tkL )

term' sg tmnL ( IfxBdr bdr : tkL )

term' sg tmnL ( IfxOp op : tkL )

term' sg tmnL ( Rvd "\181" : tkL )

term' sg tmnL ( Rvd "(" : tkL )

term' sg tmnL ( Rvd "@" : tkL )
	= ( [ Prs_Err "@ not implemented " ] , dmy )

term' sg tmnL ( Clr ( 'U' : arg@(_:_) ) : tkL ) | and ( map isDigit arg )

term' sg tmnL ( Clr nm : tkL ) | nm == "true" || nm == "false" || nm == "bool"

term' pst@( tgL , sg ) tmnL ( Clr nm : tkL )

term' pst@( _ , sg)  tmnL ( Clr nm : tkL )

term' sg tmnL tkL@( tk : _ ) | tk `elem` tmnL

term' sg tmnl ( Rvd str : tkl )

term' sg tmnl [] |  Rvd ""  `elem` tmnl

term' sg tmnl []

 	= ( [ Prs_Err mesg ] , dmy )

 	  mesg = " expecting '" ++ concat ( map disp_tk tmnl ) ++ "'"

term' sg _ ( Scan_Err mesg : _ )

aterm sg tmnl ( Rvd "\168": Clr no1 : Rvd ",": Clr no2 : Rvd "\169": tkl )

aterm sg tmnl ( Rvd "\168" : Clr no1 : Rvd "," : Clr no2 : Rvd "," :

	= ( const' no1 no2 no3 , tkl )

aterm sg tmnl ( Rvd "{" : tkl )

aterm sg tmnl ( Clr ( 'U' : arg@(_:_) ) : tkl ) | and ( map isDigit arg )

aterm sg tmnl ( Clr nm : tkl ) | nm == "true" || nm == "false" || nm == "bool"

aterm sg tmnl ( Rvd "(" : tkl )

aterm ( _ , sg ) tmnl ( Clr nm : tkl )

aterm sg tmnl ( tk : tkl )

hyp :: Parse_State -> [Token] -> [Token] -> ( Flagged_ITrm , [Token] )

hyp sg tmnL ( Rvd "[" : tkL )

              nxt : _ -> ( Prs_Err mesg1 , dmy )

hyp sg tmnL tkL

bdec :: Parse_State -> [Token] -> [Token] -> ( Flagged_ITrm , [Token] )

bdec sg tmnL tkL

	  where

bdec' :: Parse_State -> [Token] -> [Token] -> ( Flagged_ITrm , [Token] )

bdec' sg tmnL ( Rvd "(" : tkL )

        ( nxt_dc , _ : tkL2 ) = bdec sg [ Rvd ")" ] tkL

bdec' sg tmnL tkL

bdec_name :: Parse_State -> [Token] -> [Name'] -> [Token]

bdec_name sg tmnL nmL tkL

        switch inm ( Rvd "," : tkL2' ) = bdec_name sg tmnL ( inm : nmL ) tkL2'

make_dc :: [Name'] -> Flagged_ITrm -> Attribute -> Flagged_ITrm

make_dc nmL ( Opnd ( Itrm srt )) tped

make_dc nmL ( Prs_Err mesg ) _

make_dc nmL _ _ = error "unexpected term in make_dc"

abdec :: Parse_State -> [Token] -> [Token] -> ( Flagged_ITrm , [Token] )

abdec sg tmnL ( Rvd "(" : tkL )

        ( nxt_dc , _ : tkL2 ) = bdec sg [ Rvd ")" ] tkL

abdec sg tmnL tkL

name ( Rvd "{" : Clr id : tkL )

name ( Clr id : tkL )

name ( oth : tkL )

name [] = ( Bad "empty identifier" , dmy )

optyp :: [Token] -> ( Oprtype , [Token] )

optyp ( Rvd "Pre" : tkL )

optyp ( Rvd "BinL" : tkL )

optyp ( Rvd "BinR" : tkL )

optyp ( Rvd "Post" : tkL )

optyp tkL

opprc :: [Token] -> ( Int , [Token] )

opprc ( Clr prcid : tkL ) | and ( map isDigit prcid )

opprc tkL

recurse_cls :: Parse_State -> [Flagged_ITrm] -> [Token] -> ( [Flagged_ITrm] , [Token])

recurse_cls sg fnL tkL

fn_clauses

match sg ctrL ( tpe_nm : nmL ) tp_id tkl

match_cls ctr_argL nm

make_rhs

  	  where

find_recursive ( arg : argL ) tp_id

ident_type sg ( Clr nm : tkl )

ident_type sg ( Rvd "\167" : _ )

ident_type sg ( Rvd tk : _ )	

prioritise :: [Flagged_ITrm] -> [Flagged_ITrm]

prioritise tmL = case ptse [] [] False tmL of

type Flag_I = Flagged_ITrm -- shorthand

ptse :: [Flag_I] -> [Flag_I] -> Bool -> [Flag_I] -> [Flag_I]

ptse opnds oprs _ ( Prs_Err tm : opL )

ptse opnds oprs False ( Opnd tm : opL )

ptse opnds oprs True opL@( Opnd _ : _ )

ptse opnds oprs _ ( op@( Opr _ Pre prc ) : opL )

ptse opnds oprs _ ( op@( Opr _ BinL prc ) : opL )

ptse opnds oprs _ ( op@( Opr op_nm BinR prc ) : opL )

ptse opnds oprs fl ( op@( Opr _ Post prc ) : opL )

ptse opnds oprs _ []

null_op :: a -> Bool

null_op _ = False

swap_op :: Flagged_ITrm -> [Flagged_ITrm] -> [Flagged_ITrm]

swap_op op ( top_op : opnds )

swap_op op [] = [ op ]

flush :: [Flagged_ITrm] -> [Flagged_ITrm] -> Int

flush opnds oprs@( op@(Opr op_nm op_tpe prc) : opL ) pprc cmp

flush opnds [] _ _ = ( opnds , [] )

add_op :: Operator -> Oprtype -> [Flagged_ITrm] -> [Flagged_ITrm]

add_op op Pre ( arg : fn : opnds )

add_op op BinL ( arg : fn : opnds )

add_op op BinR ( arg : fn : opnds )

add_op op Post opnds

add_op op _ _

make_prebdr :: String -> Flagged_ITrm

make_prebdr bdr

make_iop :: String -> Flagged_ITrm

make_iop ":"

make_iop op

lookup_name sg nm

lookUp :: String -> Sgn -> Flagged_ITrm

lookUp nm sg

lookup' :: ISgn -> String -> Int -> Flagged_ITrm

lookup' ( Extend dc sg _ ) nm i

lookup' ( Empty _ ) nm i

lookup_dc :: IDec -> [IDec] -> String -> Int -> Int -> ( Bool , Flagged_ITrm )

lookup_dc dc dcL nm  i j

lookup_dc' :: IDec -> [IDec] -> String -> Int -> Int -> (Bool , Flagged_ITrm)

lookup_dc' ( Decpair dc1 dc2 _ ) dcL nm  i j

lookup_dc' ( Symbol_dec _ ( ( _ , Symbol_Name nm' ) : _ ) ) _ nm i j

lookup_dc' ( Axiom_dec _ ( ( _ , Symbol_Name nm' ) : _ ) ) _ nm i j

lookup_dc' ( Data _ _ [ ( _ , Datatype_Name nmL ) ] ) _  nm i j

lookup_dc' ( Def _ _ [ ( _ , Symbol_Name nm' )] ) _ nm i j

lookup_nm :: Name' -> String -> Int -> Int -> ( Bool , Flagged_ITrm )

lookup_nm ( Name nm' ) nm i j
	| nm == nm' = ( True , ( Opnd . Itrm ) ( Sym i j [] [sym_nmd] ) )

lookup_nm ( Operator' nm' prc opt ) nm i j
	| nm == nm' = ( True , Opr ( OpItrm ( Sym i j [] [sym_nmd] )) opt prc )

lookup_nml :: [Name'] -> String -> Int -> Int -> Int -> (Bool , Flagged_ITrm)

lookup_nml ( Name nm' : nml ) nm i j k
	| nm == nm' = ( True , ( Opnd . Itrm ) ( Const i j k [] [sym_nmd] ) )

lookup_nml ( Operator' nm' prc opt : nml ) nm i j k
	| nm == nm' = ( True , Opr

lookup_nml [] _ _ _ _

dmy :: [Token]

dmy = [ Rvd "" ]

pst_extend :: Flagged_ITrm -> Parse_State -> Parse_State

pst_extend ( Opnd ( Idec idc )) ( tgL , sg )

fetch_tg tg_nm1 ( tg@( tg_nm2 , _ , _ ) : tgL )

fetch_tg _ [] = ( False , ( "" , [] , [] ) )

parse_deriv :: Parse_State -> Flagged_ITrm -> Thm

parse_deriv pst ( Opnd ( Itrm itm ))

parse_deriv _ _

deriv :: Parse_State -> ITrm -> Thm

deriv pst@( tgL , sg ) ( Tagid ( str , _ , cnv_fnL ) argL )

deriv pst@( tgL , sg ) ( Binder Delta idc itm _ _ )

deriv pst@( _ , sg ) ( App itm1 itm2 _ _ )

deriv pst@( _ , sg ) ( Pair itm1 itm2 _ _ _ )

deriv ( _ , sg ) ( Sym i j _ _ )

deriv pst@( _ , SG isg ) itm

deriv _ _ = error "deriv error"

parse_iL :: [Token] -> ( Tag_Arg , [Token] )

parse_iL ( Rvd "\168" : tkL )

parse_iL _ = ( Tg_Trm ( TM_Err "Malformed integer list" ) , dmy )

parse_iL' iL ( Clr str : tkL ) | and ( map isDigit str )

parse_iL' iL ( Rvd "\169" : tkL )

parse_iL' iL ( tk : _ )

parse_iL' iL []

parse_iL'' iL ( Rvd "\169" : tkL )

parse_iL'' iL ( Rvd "," : tkL )

parse_iL'' iL _
