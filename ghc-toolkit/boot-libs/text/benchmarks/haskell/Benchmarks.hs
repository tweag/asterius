-- | Main module to run the micro benchmarks
--
{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

import Criterion.Main (defaultMain, bgroup, env)
import System.FilePath ((</>))
import System.IO (IOMode (WriteMode), openFile, hSetEncoding, utf8)

import qualified Benchmarks.Builder as Builder
import qualified Benchmarks.Concat as Concat
import qualified Benchmarks.DecodeUtf8 as DecodeUtf8
import qualified Benchmarks.EncodeUtf8 as EncodeUtf8
import qualified Benchmarks.Equality as Equality
import qualified Benchmarks.FileRead as FileRead
import qualified Benchmarks.FoldLines as FoldLines
import qualified Benchmarks.Mul as Mul
import qualified Benchmarks.Pure as Pure
import qualified Benchmarks.ReadNumbers as ReadNumbers
import qualified Benchmarks.Replace as Replace
import qualified Benchmarks.Search as Search
import qualified Benchmarks.Stream as Stream
import qualified Benchmarks.WordFrequencies as WordFrequencies

import qualified Benchmarks.Programs.BigTable as Programs.BigTable
import qualified Benchmarks.Programs.Cut as Programs.Cut
import qualified Benchmarks.Programs.Fold as Programs.Fold
import qualified Benchmarks.Programs.Sort as Programs.Sort
import qualified Benchmarks.Programs.StripTags as Programs.StripTags
import qualified Benchmarks.Programs.Throughput as Programs.Throughput

main :: IO ()
main = do
    sink <- openFile "/dev/null" WriteMode
    hSetEncoding sink utf8
    defaultMain
        [ Builder.benchmark
        , Concat.benchmark
        , env (DecodeUtf8.initEnv (tf "libya-chinese.html")) (DecodeUtf8.benchmark "html")
        , env (DecodeUtf8.initEnv (tf "yiwiki.xml")) (DecodeUtf8.benchmark "xml")
        , env (DecodeUtf8.initEnv (tf "ascii.txt")) (DecodeUtf8.benchmark "ascii")
        , env (DecodeUtf8.initEnv (tf "russian.txt")) (DecodeUtf8.benchmark  "russian")
        , env (DecodeUtf8.initEnv (tf "japanese.txt")) (DecodeUtf8.benchmark "japanese")
        , EncodeUtf8.benchmark "επανάληψη 竺法蘭共譯"
        , env (Equality.initEnv (tf "japanese.txt")) Equality.benchmark
        , FileRead.benchmark (tf "russian.txt")
        , FoldLines.benchmark (tf "russian.txt")
        , env Mul.initEnv Mul.benchmark
        , env (Pure.initEnv (tf "tiny.txt")) (Pure.benchmark "tiny")
        , env (Pure.initEnv (tf "ascii-small.txt")) (Pure.benchmark "ascii-small")
        , env (Pure.initEnv (tf "ascii.txt")) (Pure.benchmark "ascii")
        , env (Pure.initEnv (tf "english.txt")) (Pure.benchmark "english")
        , env (Pure.initEnv (tf "russian-small.txt")) (Pure.benchmark "russian")
        , env (Pure.initEnv (tf "japanese.txt")) (Pure.benchmark "japanese")
        , env (ReadNumbers.initEnv (tf "numbers.txt")) ReadNumbers.benchmark
        , env (Replace.initEnv (tf "russian.txt")) (Replace.benchmark "принимая" "своем")
        , env (Search.initEnv (tf "russian.txt")) (Search.benchmark "принимая")
        , env (Stream.initEnv (tf "russian.txt")) Stream.benchmark
        , env (WordFrequencies.initEnv (tf "russian.txt")) WordFrequencies.benchmark
        , bgroup "Programs"
            [ Programs.BigTable.benchmark sink
            , Programs.Cut.benchmark (tf "russian.txt") sink 20 40
            , Programs.Fold.benchmark (tf "russian.txt") sink
            , Programs.Sort.benchmark (tf "russian.txt") sink
            , Programs.StripTags.benchmark (tf "yiwiki.xml") sink
            , Programs.Throughput.benchmark (tf "russian.txt") sink
            ]
        ]
    where
    -- Location of a test file
    tf = ("../tests/text-test-data" </>)
