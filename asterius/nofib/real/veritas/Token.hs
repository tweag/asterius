
module Token(tokenise) where

import Core_datatype

import Kernel

import Type_defs

endofline  = "\n\f\r"

whitespace = " \t" ++ endofline

a_symbol = "!#$&*-+=~<>?/\\.:|@"

singlechrs = ",;\"`\'(){}[]\176\185"

binder = "\208\211\236\177\178\229\196"

ifx_binder = "\183\184\167\182\187"

ifx_op = "\179\180=\172:"

reserved = [ "empty", "signature", "extend", "combine", "with", "sharing" ,

tokenise :: String -> [Token]

tokenise string
	= denull ( scan' string "" )

denull :: [Token] -> [Token]

denull ( Clr "" : tkl ) = denull tkl

denull ( Clr tk : tkl )

denull ( Rvd tk : tkl ) = error " optimise to trap some reserved earlier and reduce length of reserved set"

denull ( Scan_Err mess : _ ) = [ Scan_Err mess ]

denull ( tk : tkl ) = tk : denull tkl

denull [] = []

denull x = error ( show x )

scan' ( a : x ) current

	| single a          = Clr current : ( Clr [a] : scan' x "" )

scan' [] current = [ Clr current ]

symbol_scan :: String -> String -> [Token]

symbol_scan ( a : x ) current

symbol_scan [] current = [ Clr current ]

super_scan _ "^" = [ Scan_Err "\nAttempt to superscript invalid string\n"]

super_scan ( a : x ) current

	| otherwise     = [ Scan_Err "\nInvalid superscript character\n" ]

super_scan [] _ = [ Scan_Err "\nEmpty superscript\n" ]

alphanum' ch = alphanum ch || ch == '_'

alphanum ch

single ch
	= ch `elem` singlechrs || ch > '\DEL'

discard :: String -> String

discard ( ch : x )

discard [] = ""

mk_bdr :: Char -> Binder_conn

mk_bdr x | fromEnum x == 208 = Pi

mk_bdr x | fromEnum x == 211 = Sigma

mk_bdr x | fromEnum x == 236 = Lambda

mk_bdr x | fromEnum x == 177 = Forall

mk_bdr x | fromEnum x == 178 = Exists

mk_bdr x | fromEnum x == 229 = Choose

mk_bdr '\208' = Pi

mk_bdr '\211' = Sigma

mk_bdr '\236' = Lambda

mk_bdr '\177' = Forall

mk_bdr '\178' = Exists

mk_bdr '\229' = Choose

mk_bdr '\196' = Delta

mk_bdr oth = error ( "Mk_bdr: " ++ [oth])

disp_tk :: Token -> String

disp_tk ( Rvd str ) = str

disp_tk ( Clr str ) = str
