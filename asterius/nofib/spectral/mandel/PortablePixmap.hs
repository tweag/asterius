
module PortablePixmap where

data PixMap = Pixmap Integer Integer Int [(Int,Int,Int)]

createPixmap::Integer -> Integer -> Int -> [(Int,Int,Int)] -> PixMap
createPixmap width height max colours = Pixmap width height max colours

instance Show PixMap where
	showsPrec prec (Pixmap x y z rgbs) = showHeader x y z . showRGB rgbs


showHeader::Integer -> Integer -> Int -> ShowS
showHeader x y z 	= showString "P6\n" . showBanner .
			  shows x . showReturn .
			  shows y . showReturn .
			  shows z . showReturn

showRGB::[(Int,Int,Int)] ->  ShowS
showRGB [] 		= id
showRGB ((r,g,b):rest)  = showChar (toEnum r) .
			  showChar (toEnum g) .
			  showChar (toEnum b) .
			  showRGB rest

showSpace  = showChar ' '
showReturn = showChar '\n'

showBanner = showString "# Portable pixmap created by Haskell Program :\n" .
	     showString "#\tPortablePixmap.lhs (Jon.Hill 28/5/92)\n"
