
module Build_itrm where

import Data.Char(isDigit)--1.3
import Core_datatype

import Type_defs

import Attributes

import Unparse  -- for debugging purposes only

import Kernel

import Sub_Core1

import Sub_Core2

import Sub_Core3

import Sub_Core4

import Vtslib

sym' :: String -> String -> Flagged_ITrm

sym' str1 str2

const' :: String -> String -> String -> Flagged_ITrm

const' str1 str2 str3

binder' bnd_con ( Opnd ( Idec dc )) ( Opnd ( Itrm tm ))

binder' _ ( Prs_Err x ) _ = Prs_Err x

binder' _ _ ( Prs_Err x ) = Prs_Err x

cond' :: Flagged_ITrm -> Flagged_ITrm -> Flagged_ITrm -> Flagged_ITrm

cond' ( Prs_Err x ) _ _ = Prs_Err x

cond' _ ( Prs_Err x ) _ = Prs_Err x

cond' _ _ ( Prs_Err x ) = Prs_Err x

cond' ( Opnd ( Idec dc ) ) ( Opnd ( Itrm tm1 ) ) ( Opnd ( Itrm tm2 ))

let' ( Prs_Err x ) _ _ = Prs_Err x

let' _ ( Prs_Err x ) _ = Prs_Err x

let' _ _ ( Prs_Err x ) = Prs_Err x

let' ( Opnd ( Idec dc)) ( Opnd ( Itrm tm1)) ( Opnd ( Itrm tm2 ))

let' ( Opr _ _ _ ) _ _ = error "1"
let' _ ( Opr _ _ _ ) _ = error "2"
let' _ _ ( Opr _ _ _ ) = error "3"
let' _ ( Opnd ( PApp _ _ _ )) _ = error "4.1"
let' _ ( Opnd ( PairApp _ ) ) _ = error "4.2"
let' _ ( Opnd ( ParIfx _ _) ) _ = error "4.3"
let' _ ( Opnd ( TypApp _ ) ) _ = error "4.4"
let' _ ( Opnd ( Itrm _ ) ) _ = error "4.5"
let' _ ( Opnd ( Idec _ ) ) _ = error "4.6"
let' _ ( Opnd _ ) _ = error "5"
let' _ _ ( Opnd _ ) = error "6"

app' ( Opr ( Spl "" ) _ _ ) x = x

app' x ( Opr ( Spl "" ) _ _ ) = x

app' ( Opr ( OpBdr bdr ) _ _ )  ( Opnd ( Idec dc ))
	= Opnd ( PApp bdr dc False )

app' ( Opr ( OpBdr bdr ) _ _ )  ( Opnd ( Itrm srt ))
	= Opnd ( PApp bdr dc True )

app' (Opnd ( PApp bdr dc anon )) ( Opnd ( Itrm tm ))
	= opnd ( Binder bdr dc shft_tm [] [ifx_bdr] )

app' ( Opr ( OpIfx op ) _ _ ) ( Opnd ( Itrm tm ))

app' ( Opnd ( ParIfx op tm1 ) ) ( Opnd ( Itrm tm2 ))
	= opnd ( Binary' op tm1 tm2 [] [] )

app' ( Opr ( Spl "," ) _ _ )  ( Opnd ( Itrm tm1 ))

app' ( Opnd ( PairApp tm1 )) ( Opnd ( Itrm tm2 ))

	  where

app' ( Opr ( Spl "typed" ) _ _ ) opnd1

app' ( Opnd ( TypApp ( Opnd ( Itrm ( Pair tm1 tm2 _ a b )))))

app' ( Opr ( Spl "Not" ) _ _ ) ( Opnd ( Itrm tm ))

app' ( Opr ( Spl ":" ) _ _ ) ( Opnd ( Itrm tm ))

app' ( Opnd ( ParColon tm1 )) ( Opnd ( Itrm tm2 ))

app' ( Opnd ( Itrm tm1 )) ( Opnd ( Itrm tm2 ))

app' ( Opr ( OpItrm op ) stl _ ) ( Opnd ( Itrm tm ))

app' ( Opnd ( Itrm tm )) ( Opr ( OpItrm op ) _ _ ) -- happen?

app' ( Opr ( OpItrm op1 ) stl _ ) ( Opr ( OpItrm op2 ) _ _ )

app' ( Prs_Err mess ) _ = Prs_Err mess

app' _ ( Prs_Err mess ) = Prs_Err mess

app' x y = error ( "app' y: " ++ unparse ( Empty []) y ++ "\nx: " ++ unparse (Empty [] ) x )

opnd :: ITrm -> Flagged_ITrm

opnd = Opnd . Itrm

fetch_arg :: Flagged_ITrm -> ITrm

fetch_arg ( Opnd ( Itrm tm )) = tm

fetch_arg ( Opr ( OpItrm op ) _ _ ) = op

decpair' _ ( Prs_Err x ) _ = Prs_Err x

decpair' _ _ ( Prs_Err x ) = Prs_Err x

decpair' att ( Opnd ( Idec dc1 )) ( Opnd ( Idec dc2 ))

symbol_dec' ( Opnd ( Itrm tm )) nm attL

symbol_dec' ( Prs_Err mess ) nm _ = Prs_Err mess

data' ( _ , [ Prs_Err mesg ] ) _

data' ( type_nm , dcl ) ctr_defs

        		    att = [ dat_nm ( type_nm : ctr_nml ) ]	

ctr' nml argll (( nm , argl ) : ctrl )

ctr' nml argll []

check_arg tml ( Opnd ( Itrm tm ) : argl )

check_arg tml []

check_arg _ ( Prs_Err mesg : argl )

extend' ( Opnd ( Idec dc )) ( Opnd ( Isgn sg ))

extend' _ ( Prs_Err mesg )

extend' ( Prs_Err mesg ) _

def' ( _ , [ Prs_Err mesg ] ) _ _

def' _ ( Prs_Err mesg ) _

def' ( nm , fmls ) ( Opnd ( Itrm rhs )) att

make_tm ( Opnd ( Idec dc ) : dcl ) rhs

make_tm [] rhs

add_sg_att ( Prs_Err mesg ) _ = Prs_Err mesg

add_sg_att ( Opnd ( Isgn ( Empty attl ))) att

add_sg_att ( Opnd ( Isgn ( Extend  dc sg attl ))) att

recurse' _ ( Prs_Err mesg ) = Prs_Err mesg

recurse' tml ( Opnd ( Itrm srt ))

tag' tg tgL
