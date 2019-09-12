module Features.Feature80 ( main ) where

import           Control.Applicative            (pure)
import           Control.Monad.Identity
import           Data.List.NonEmpty
import           Data.Semigroup
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                     hiding (Test)

import           Text.Parsec

main :: Test
main =
  testCase "Monoid instance (#80)" $ do
    parseString (as <> bs) "aabbb" @?= "aabbb"
    parseString (mempty <> as) "aabbb" @?= "aa"
    parseString (as <> mempty) "aabbb" @?= "aa"
    parseString (sconcat $ fromList [as, mempty, bs]) "aabbb" @?= "aabbb"
    parseString (mconcat [as, mempty, bs]) "aabbb" @?= "aabbb"
    parseString (mempty :: ParsecT String () Identity String) "aabbb" @?= ""
    parseString (stimes (2::Int) str_a) "aabbb" @?= "aa"
    parseFail   (stimes (3::Int) str_a) "aabbb" @?= "no parse"
    parseString ((one ch_a) <> (one ch_a) <> bs) "aabbb" @?= "aabbb"

 where
   one = fmap pure

   as :: ParsecT String () Identity String
   as = many $ char 'a'
   bs :: ParsecT String () Identity String
   bs = many $ char 'b'
   ch_a :: ParsecT String () Identity Char
   ch_a = char 'a'
   str_a :: ParsecT String () Identity String
   str_a = string "a"

   parseString :: ParsecT String () Identity String -> String -> String
   parseString p input =
      case parse p "Example" input of
        Left{}    -> error "Parse failure"
        Right str -> str

   parseFail :: ParsecT String () Identity String -> String -> String
   parseFail p input =
      case parse p "Example" input of
        Left{}  -> "no parse"
        Right _ -> error "Parsed but shouldn't"
