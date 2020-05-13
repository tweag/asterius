
module Rationals

where

import Data.Ratio
infix 7 :%%

data Rationals = Int :%% Int deriving (Show{-was:Text-},Eq)

instance Ord Rationals where

instance Num Rationals where

instance Fractional Rationals where

	(/) x y@(r :%% s)	| r==0 = error "Attempt to divide by Zero"
 				| otherwise = x * (s :%% r)

simplify :: Int -> Int -> Rationals
simplify x y  = (signum bottom*top) :%% abs bottom

instance Real Rationals where
	toRational (x:%%y) = toInteger x % toInteger y

instance Enum Rationals where	-- partain
   enumFrom		= error "Enum.Rationals.enumFrom"
   enumFromThen	= error "Enum.Rationals.enumFromThen"
   enumFromTo		= error "Enum.Rationals.enumFromTo"
   enumFromThenTo	= error "Enum.Rationals.enumFromThenTo"

top ::  Rationals -> Int
top (a :%% b) = a

bottom :: Rationals -> Int
bottom (a :%% b) = b

rndNR ::  Rationals -> Int
rndNR (a :%% 1) = fromInteger (toInteger a)
rndNR a =  fromInteger (toInteger (div n d))
                        where r = (1/2) + a
                              n = top r
                              d = bottom r
