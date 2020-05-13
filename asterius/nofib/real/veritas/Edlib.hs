
module Edlib where

import Type_defs

import Core_datatype

infixr 9 <:

infixl 8 /// , />/ , /:>/ , /./ , /.>/, /.:>/, |||, |>|, |@|, |.|

infixl 8 ..., ./., \\\

infixl 1 `handle`, `ihandle`

type Xin = ( String , [Int] , [Int])

type Xout = String

type Xio = ( Xin , Xout )

type Xio_fn = Xin -> Xio

type Xst a = ( Xin , a , Xout )

type Xst_fn a = ( Xin , a ) -> ( Xin , a , Xout )

(\\\) :: Xio -> Xio_fn -> Xio

( xin , xout ) \\\ g

        where

(...) :: Xio_fn -> Xio_fn -> Xin -> Xio

(...) f g xin

(./.) :: Xio_fn -> ( Xin -> Xst ( MayBe a b )) -> Xin -> Xst ( MayBe a b )

(./.) f g xin

        where
        ( xin', xout' )  = f xin

app :: [ Xin -> Xio ] -> Xin -> Xio

app ( fn : fns ) = fn ... app fns

app [] =  \ xin -> ( xin , [] )

--partain: (|||) :: MayBe a b -> ( a -> MayBe c d ) -> MayBe c d
(|||) :: MayBe a b -> ( a -> MayBe c b ) -> MayBe c b

( Ok s ) ||| f
	= f s

( Bad mesg ) ||| f = Bad mesg

s |.| f
	= case s of

( Ok ( s , u )) |@| f
	= f s u

( Bad mesg ) |@| f = Bad mesg

( Ok ( s , u )) |>| f

(///) :: Xst (MayBe a b) -> ( a -> Xin -> Xst (MayBe c b)) -> Xst (MayBe c b)

x@( xin , st , xout ) /// f

(/./) :: (Xin -> Xst (MayBe a b)) -> ( a -> Xin -> Xst (MayBe c b))

(/./) f g xin

(/>/) :: Xst (MayBe a b) -> ( Xin -> Xst (MayBe c b)) -> Xst (MayBe (a,c) b)

x1@( xin , st , xout ) />/ f

(/.>/) :: (Xin -> Xst (MayBe a b)) -> ( Xin -> Xst (MayBe c b))

(/.>/) f g xin

x1@( xin , st , xout ) /:>/ f

(/.:>/) :: (Xin -> Xst (MayBe a b)) -> ( Xin -> Xst (MayBe [a] b))

(/.:>/) f g xin

handle f handler xin

ihandle x@( xin , st , _ ) handler

mk_ok x_fn arg

split :: ( Eq a ) => a -> [a] -> [[a]]

split c ( a : x )

split _ [] = []

(<:) :: [a] -> a -> [a]

l <: c = l ++ [c]

sendout ( _, _, xout ) x

return_val st xin = ( xin , st , [] )

return_err st xin = ( xin, Bad st , [] )

reTurn st xin = ( xin, Ok st, [] )

genuid ( ins , rsps , ( gno : gnoL ) )

( f >> g )
	= next_tk f /./ ( next_tk . g )

( f >>> g)

( f *>> g )

( f *>>> g)

( f >>+ g )

( f >>>+ g )

( f *>>+ g )

( f *>>>+ g )

( f >>^ g )

( f >>>^ g )

( f *>>^ g )

( f *>>>^ g )

next_tk f ( tk : tkL, pst, xin )

next_tk _ st@( [] , _ , _ )

discard_tk ( _ : tkL , pst , xin )

discard_tk st@( [] , _ , _ ) = st

pst_retract ( tkL , (tgL, Extend _ isg _ ), xin )

pst_retract _ = error "pst_retract on empty sg -- impossible"

pst_extend resL st

pst_extend' ( Opnd ( Idec idc ) : _ ) ( tkL, ( tgL, isg ), xin )

pst_extend' ( _ : resL )

pst_extend' []
