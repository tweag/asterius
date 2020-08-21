#!/usr/bin/env stack
{-
  stack --resolver lts-16.10 script
    --package Cabal
    --package containers
    --package pantry
    --package process
    --package text
-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

import           Data.List
import qualified Data.Map.Strict               as M
import qualified Data.Text                     as T
import qualified Distribution.Simple.Utils     as C
import qualified Distribution.Types.GenericPackageDescription
                                               as C
import qualified Distribution.Types.PackageName
                                               as C
import qualified Distribution.Types.Version    as C
import qualified Pantry                        as P
import           System.Environment.Blank
import           System.Process

type Snapshot = M.Map PkgName PkgInfo

type PkgName = String

type PkgVersion = String

data PkgInfo = PkgInfo
  { version :: PkgVersion,
    flagsOff, flagsOn :: [String]
  }
  deriving (Show)

parseVersion :: String -> (PkgName, PkgVersion)
parseVersion pkg_name_ver = (pkg_name, pkg_ver)
 where
  (reverse -> pkg_ver, reverse . tail -> pkg_name) =
    break (== '-') $ reverse pkg_name_ver

globalSnapshot :: IO Snapshot
globalSnapshot = do
  pkgs <- map parseVersion . words <$> readProcess
    "ghc-pkg-8.8.3"
    ["list", "--global", "--simple-output"]
    ""
  pure $ M.fromList
    [ (pkg_name, PkgInfo { version = pkg_ver, flagsOff = [], flagsOn = [] })
    | (pkg_name, pkg_ver) <- pkgs
    ]

stackageSnapshot :: P.RawSnapshotLocation -> IO Snapshot
stackageSnapshot raw_loc = do
  (r, _, _) <- P.runPantryAppClean
    $ P.loadAndCompleteSnapshotRaw raw_loc mempty mempty
  pure $ M.fromList
    [ ( pkg_name
      , PkgInfo
        { version  = case spLocation of
                       P.PLIHackage P.PackageIdentifier {..} _ _ ->
                         intercalate "." $ map show $ C.versionNumbers pkgVersion
                       _ -> error $ "stackageSnapshot " <> show spLocation
        , flagsOff = [ flag_name
                     | (C.unFlagName -> flag_name, flag) <- M.toList spFlags
                     , not flag
                     ]
        , flagsOn  = [ flag_name
                     | (C.unFlagName -> flag_name, flag) <- M.toList spFlags
                     , flag
                     ]
        }
      )
    | (C.unPackageName -> pkg_name, P.SnapshotPackage {..}) <- M.toList
      $ P.snapshotPackages r
    ]

asteriusSnapshot :: P.RawSnapshotLocation -> IO Snapshot
asteriusSnapshot raw_loc = do
  s_global   <- globalSnapshot
  s_stackage <- stackageSnapshot raw_loc
  pure
    $ M.adjust
        (\pkg_info -> pkg_info
          { flagsOff = C.ordNub $ "embed_linear" : flagsOff pkg_info
          }
        )
        "Rasterific"
    $ M.adjust
        (\pkg_info -> pkg_info
          { flagsOn = C.ordNub $ "integer-simple" : flagsOn pkg_info
          }
        )
        "blaze-textual"
    $ M.adjust
        (\pkg_info -> pkg_info
          { flagsOn = C.ordNub $ "embed-data-files" : flagsOn pkg_info
          }
        )
        "criterion"
    $ M.adjust
        (\pkg_info ->
          pkg_info { flagsOff = C.ordNub $ "integer-gmp" : flagsOff pkg_info }
        )
        "cryptonite"
    $ M.adjust
        (\pkg_info ->
          pkg_info { flagsOn = C.ordNub $ "Embed" : flagsOn pkg_info }
        )
        "hyphenation"
    $ M.adjust
        (\pkg_info -> pkg_info
          { flagsOn = C.ordNub $ "embed_data_files" : flagsOn pkg_info
          }
        )
        "pandoc"
    $ M.adjust
        (\pkg_info -> pkg_info
          { flagsOn = C.ordNub $ "embed_data_files" : flagsOn pkg_info
          }
        )
        "pandoc-citeproc"
    $ M.adjust
        (\pkg_info ->
          pkg_info { flagsOn = C.ordNub $ "embed-files" : flagsOn pkg_info }
        )
        "shake"
    $ M.unionWith const s_global s_stackage

makeCabalConfig :: Snapshot -> String
makeCabalConfig s =
  "constraints:\n  "
    <> intercalate
         ",\n  "
         (  mconcat
             [ [ pkg_name <> " -" <> f | f <- flagsOff ]
                 <> [ pkg_name <> " +" <> f | f <- flagsOn ]
             | (pkg_name, PkgInfo {..}) <- M.toList s
             ]
         <> [ pkg_name <> " ==" <> version
            | (pkg_name, PkgInfo {..}) <- M.toList s
            ]
         )
    <> "\n"

makePkgList :: Snapshot -> String
makePkgList = unlines . M.keys

main :: IO ()
main = do
  [raw_loc_s] <- getArgs
  raw_loc     <- P.resolvePaths Nothing $ P.parseRawSnapshotLocation $ T.pack
    raw_loc_s
  s_asterius <- asteriusSnapshot raw_loc
  writeFile "cabal.config" $ makeCabalConfig s_asterius
  writeFile "pkgs.txt" $ makePkgList s_asterius
