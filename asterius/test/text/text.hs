{-# LANGUAGE OverloadedStrings #-}

import Asterius.Types
import Control.Monad
import qualified Data.Char as C
import Data.Coerce
import Data.Text as Text

foreign import javascript "console.log(${1})" js_print_bool :: Bool -> IO ()

foreign import javascript "console.log(${1})" js_print_int :: Int -> IO ()

foreign import javascript "console.log(${1})" js_print :: JSVal -> IO ()

sampleText :: Text
{-# NOINLINE sampleText #-}
sampleText = Text.pack sampleString

sampleString :: String
{-# NOINLINE sampleString #-}
sampleString = "Sample text"

sampleText2 :: Text
{-# NOINLINE sampleText2 #-}
sampleText2 = Text.pack "Sample text number 2"

main :: IO ()
main = do
  let js_print_string :: String -> IO ()
      js_print_string s = js_print (coerce (toJSString s))
      js_print_text :: Text -> IO ()
      js_print_text t = js_print_string (Text.unpack t)
  js_print_int (Text.length Text.empty)
  let roundTrip = Text.unpack sampleText == sampleString
  js_print_bool roundTrip
  js_print_string (Text.unpack sampleText)
  js_print_string (show sampleText)
  js_print_bool (sampleText == sampleText2)
  js_print_bool (sampleText == Text.copy sampleText)
  js_print_bool (sampleText >= sampleText2)
  js_print_bool (sampleText < sampleText2)
  js_print_text (sampleText `Text.append` sampleText2)
  js_print_text ('*' `Text.cons` sampleText)
  js_print_text (sampleText `Text.snoc` '*')
  js_print_text (Text.tail sampleText)
  js_print_text (Text.init sampleText)
  js_print_text (Text.map C.toUpper sampleText)
  js_print_text (Text.intersperse '-' sampleText)
  js_print_text (Text.intercalate " STOP " [sampleText, sampleText2])
  js_print_text (Text.replace "ample" "uper" sampleText)
  js_print_text (Text.toCaseFold sampleText2)
  js_print_text (Text.toLower sampleText2)
  js_print_text (Text.toUpper sampleText2)
  js_print_text (Text.toTitle sampleText2)
  js_print_text (Text.center 50 '~' sampleText)
  js_print_text (Text.concat [sampleText, " -- ", sampleText2])
  forM_ (Text.words sampleText2) js_print_text
  js_print_text (Text.filter (`notElem` ("aeiouyAEIOUY" :: String)) sampleText)
  js_print_text (Text.copy sampleText)
