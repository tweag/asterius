{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

module Main where

import Data.Word
import Data.List
import GHC.Prim
import GHC.Exts


-- Having a wrapper gives us two things:
-- * it's easier to test everything (no need for code using raw primops)
-- * we test the deriving mechanism for Word8#
data TestWord8 = T8 Word8#
    deriving (Eq, Ord)

mkT8 :: Word -> TestWord8
mkT8 (W# a) = T8 (narrowWord8# a)

main :: IO ()
main = do
    let input = [ (a, b) | a <- allWord8, b <- allWord8 ]

    --
    -- (==)
    --
    let expected = [ a == b | (a, b) <- input ]
        actual = [ mkT8 a == mkT8 b | (a, b) <- input ]
    checkResults "(==)" input expected actual


checkResults
    :: (Eq a, Eq b, Show a, Show b) => String -> [a] -> [b] -> [b] -> IO ()
checkResults test inputs expected actual =
    case findIndex (\(e, a) -> e /= a) (zip expected actual) of
        Nothing -> putStrLn $ "Pass: " ++ test
        Just i -> error $
            "FAILED: " ++ test ++ " for input: " ++ show (inputs !! i)
              ++ " expected: " ++ show (expected !! i)
              ++ " but got: " ++ show (actual !! i)



-- 200 - succeeds
-- 150 - succeeds
-- 133 - succeeds
-- 130 - succeeds
-- 127 - succeeds
-- 126 - succeeds
-- 125 - fails
-- 100 - fails
-- Prelude Data.Word> maxBound  :: Word8
-- 255
-- Prelude Data.Word> minBound :: Word8
-- 0
-- Prelude> length $ [0..255-125]
-- 131
allWord8 :: [Word]
allWord8 = [ minWord8 .. (maxWord8 - 125)  ]

delta :: Word
delta = maxWord8 - minWord8

minWord8 :: Word
minWord8 = fromIntegral (minBound :: Word8)

maxWord8 :: Word
maxWord8 = fromIntegral (maxBound :: Word8)
