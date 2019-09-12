import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.Token
import Text.Parsec.Prim
import Asterius.Types
import Data.Coerce
import Numeric

foreign import javascript "console.log(${1})" js_print :: JSVal -> IO ()

main :: IO ()
main = do
   let
      js_print_string :: String -> IO ()
      js_print_string s = js_print (coerce (toJSString s))

      parenSet = char '(' >> many parenSet >> char ')' :: Parsec String () Char
      parens = (many parenSet >> eof) <|> eof

   js_print_string $ show $ parse parens "" "()"

   -- example adapted from http://book.realworldhaskell.org/read/using-parsec.html (by-nc 3.0)
   let
      p_query = p_pair `sepBy` char '&'

      p_pair = do
         name <- many1 p_char
         value <- optionMaybe (char '=' >> many p_char)
         return (name, value)

      p_char = oneOf urlBaseChars
           <|> (char '+' >> return ' ')
           <|> p_hex

      urlBaseChars = ['a'..'z']++['A'..'Z']++['0'..'9']++"$-_.!*'(),"

      p_hex = do
         char '%'
         a <- hexDigit
         b <- hexDigit
         let ((d, _):_) = readHex [a,b]
         return . toEnum $ d

   js_print_string $ show $ parse p_query "" "foo=bar&a%21=b+c"
