{-# LANGUAGE DeriveGeneric #-}

-- | This module contains type definitions copied from Cabal-1.24.2.0
-- to avoid a dependency on Cabal. Their contents for the benchmark are read
-- from a cache file using their 'Read' instance, see "GenericsBenchCache".
--
module Cabal24 where

import Data.Version (Version)
import GHC.Generics (Generic)
import Data.Map (Map)

data Benchmark = Benchmark {
        benchmarkName      :: String,
        benchmarkInterface :: BenchmarkInterface,
        benchmarkBuildInfo :: BuildInfo,
        benchmarkEnabled   :: Bool
    } deriving (Generic, Eq, Ord, Read, Show)

data BenchmarkInterface =
     BenchmarkExeV10 Version FilePath
   | BenchmarkUnsupported BenchmarkType
   deriving (Generic, Eq, Ord, Read, Show)

data BenchmarkType = BenchmarkTypeExe Version
                   | BenchmarkTypeUnknown String Version
    deriving (Generic, Eq, Ord, Read, Show)

data BuildInfo = BuildInfo {
        buildable         :: Bool,
        buildTools        :: [Dependency],
        cppOptions        :: [String],
        ccOptions         :: [String],
        ldOptions         :: [String],
        pkgconfigDepends  :: [Dependency],
        frameworks        :: [String],
        extraFrameworkDirs:: [String],
        cSources          :: [FilePath],
        jsSources         :: [FilePath],
        hsSourceDirs      :: [FilePath],
        otherModules      :: [ModuleName],
        defaultLanguage   :: Maybe Language,
        otherLanguages    :: [Language],
        defaultExtensions :: [Extension],
        otherExtensions   :: [Extension],
        oldExtensions     :: [Extension],
        extraLibs         :: [String],
        extraGHCiLibs     :: [String],
        extraLibDirs      :: [String],
        includeDirs       :: [FilePath],
        includes          :: [FilePath],
        installIncludes   :: [FilePath],
        options           :: [(CompilerFlavor,[String])],
        profOptions       :: [(CompilerFlavor,[String])],
        sharedOptions     :: [(CompilerFlavor,[String])],
        customFieldsBI    :: [(String,String)],
        targetBuildDepends :: [Dependency],
        targetBuildRenaming :: Map PackageName ModuleRenaming
    } deriving (Generic, Eq, Ord, Read, Show)

data BuildType
  = Simple
  | Configure
  | Make
  | Custom
  | UnknownBuildType String
  deriving (Generic, Eq, Ord, Read, Show)

data CompilerFlavor = GHC | GHCJS | NHC | YHC | Hugs | HBC | Helium
                    | JHC | LHC | UHC
                    | HaskellSuite String
                    | OtherCompiler String
                    deriving (Generic, Eq, Ord, Read, Show)

data Dependency = Dependency PackageName VersionRange
  deriving (Generic, Eq, Ord, Read, Show)

data Executable = Executable {
        exeName    :: String,
        modulePath :: FilePath,
        buildInfo  :: BuildInfo
    }
    deriving (Generic, Eq, Ord, Read, Show)

data Extension =
    EnableExtension KnownExtension
  | DisableExtension KnownExtension
  | UnknownExtension String
  deriving (Generic, Eq, Ord, Read, Show)

newtype FlagName = FlagName String
  deriving (Generic, Eq, Ord, Read, Show)

data KnownExtension =
    OverlappingInstances
  | UndecidableInstances
  | IncoherentInstances
  | DoRec
  | RecursiveDo
  | ParallelListComp
  | MultiParamTypeClasses
  | MonomorphismRestriction
  | FunctionalDependencies
  | Rank2Types
  | RankNTypes
  | PolymorphicComponents
  | ExistentialQuantification
  | ScopedTypeVariables
  | PatternSignatures
  | ImplicitParams
  | FlexibleContexts
  | FlexibleInstances
  | EmptyDataDecls
  | CPP
  | KindSignatures
  | BangPatterns
  | TypeSynonymInstances
  | TemplateHaskell
  | ForeignFunctionInterface
  | Arrows
  | Generics
  | ImplicitPrelude
  | NamedFieldPuns
  | PatternGuards
  | GeneralizedNewtypeDeriving
  | ExtensibleRecords
  | RestrictedTypeSynonyms
  | HereDocuments
  | MagicHash
  | TypeFamilies
  | StandaloneDeriving
  | UnicodeSyntax
  | UnliftedFFITypes
  | InterruptibleFFI
  | CApiFFI
  | LiberalTypeSynonyms
  | TypeOperators
  | RecordWildCards
  | RecordPuns
  | DisambiguateRecordFields
  | TraditionalRecordSyntax
  | OverloadedStrings
  | GADTs
  | GADTSyntax
  | MonoPatBinds
  | RelaxedPolyRec
  | ExtendedDefaultRules
  | UnboxedTuples
  | DeriveDataTypeable
  | DeriveGeneric
  | DefaultSignatures
  | InstanceSigs
  | ConstrainedClassMethods
  | PackageImports
  | ImpredicativeTypes
  | NewQualifiedOperators
  | PostfixOperators
  | QuasiQuotes
  | TransformListComp
  | MonadComprehensions
  | ViewPatterns
  | XmlSyntax
  | RegularPatterns
  | TupleSections
  | GHCForeignImportPrim
  | NPlusKPatterns
  | DoAndIfThenElse
  | MultiWayIf
  | LambdaCase
  | RebindableSyntax
  | ExplicitForAll
  | DatatypeContexts
  | MonoLocalBinds
  | DeriveFunctor
  | DeriveTraversable
  | DeriveFoldable
  | NondecreasingIndentation
  | SafeImports
  | Safe
  | Trustworthy
  | Unsafe
  | ConstraintKinds
  | PolyKinds
  | DataKinds
  | ParallelArrays
  | RoleAnnotations
  | OverloadedLists
  | EmptyCase
  | AutoDeriveTypeable
  | NegativeLiterals
  | BinaryLiterals
  | NumDecimals
  | NullaryTypeClasses
  | ExplicitNamespaces
  | AllowAmbiguousTypes
  | JavaScriptFFI
  | PatternSynonyms
  | PartialTypeSignatures
  | NamedWildCards
  | DeriveAnyClass
  | DeriveLift
  | StaticPointers
  | StrictData
  | Strict
  | ApplicativeDo
  | DuplicateRecordFields
  | TypeApplications
  | TypeInType
  | UndecidableSuperClasses
  | MonadFailDesugaring
  | TemplateHaskellQuotes
  | OverloadedLabels
  deriving (Generic, Eq, Ord, Read, Show)

data Language =
    Haskell98
  | Haskell2010
  | UnknownLanguage String
  deriving (Generic, Eq, Ord, Read, Show)

data Library = Library {
        exposedModules    :: [ModuleName],
        reexportedModules :: [ModuleReexport],
        requiredSignatures:: [ModuleName],
        exposedSignatures:: [ModuleName],
        libExposed        :: Bool,
        libBuildInfo      :: BuildInfo
    }
    deriving (Generic, Eq, Ord, Read, Show)

data License =
    GPL (Maybe Version)
  | AGPL (Maybe Version)
  | LGPL (Maybe Version)
  | BSD2
  | BSD3
  | BSD4
  | MIT
  | ISC
  | MPL Version
  | Apache (Maybe Version)
  | PublicDomain
  | AllRightsReserved
  | UnspecifiedLicense
  | OtherLicense
  | UnknownLicense String
  deriving (Generic, Eq, Ord, Read, Show)

newtype ModuleName = ModuleName [String]
  deriving (Generic, Eq, Ord, Read, Show)

data ModuleReexport = ModuleReexport {
       moduleReexportOriginalPackage :: Maybe PackageName,
       moduleReexportOriginalName    :: ModuleName,
       moduleReexportName            :: ModuleName
    } deriving (Generic, Eq, Ord, Read, Show)

data ModuleRenaming = ModuleRenaming Bool [(ModuleName, ModuleName)]
  deriving (Generic, Eq, Ord, Read, Show)

data PackageDescription
    =  PackageDescription {
        package        :: PackageIdentifier,
        license        :: License,
        licenseFiles   :: [FilePath],
        copyright      :: String,
        maintainer     :: String,
        author         :: String,
        stability      :: String,
        testedWith     :: [(CompilerFlavor,VersionRange)],
        homepage       :: String,
        pkgUrl         :: String,
        bugReports     :: String,
        sourceRepos    :: [SourceRepo],
        synopsis       :: String,
        description    :: String,
        category       :: String,
        customFieldsPD :: [(String,String)],
        buildDepends   :: [Dependency],
        specVersionRaw :: Either Version VersionRange,
        buildType      :: Maybe BuildType,
        setupBuildInfo :: Maybe SetupBuildInfo,
        library        :: Maybe Library,
        executables    :: [Executable],
        testSuites     :: [TestSuite],
        benchmarks     :: [Benchmark],
        dataFiles      :: [FilePath],
        dataDir        :: FilePath,
        extraSrcFiles  :: [FilePath],
        extraTmpFiles  :: [FilePath],
        extraDocFiles  :: [FilePath]
    } deriving (Generic, Eq, Ord, Read, Show)

data PackageIdentifier
    = PackageIdentifier {
        pkgName    :: PackageName,
        pkgVersion :: Version
     }
     deriving (Generic, Eq, Ord, Read, Show)

newtype PackageName = PackageName { unPackageName :: String }
    deriving (Generic, Eq, Ord, Read, Show)

data RepoKind =
    RepoHead
  | RepoThis
  | RepoKindUnknown String
  deriving (Generic, Eq, Ord, Read, Show)

data RepoType = Darcs | Git | SVN | CVS
              | Mercurial | GnuArch | Bazaar | Monotone
              | OtherRepoType String
  deriving (Generic, Eq, Ord, Read, Show)

data SetupBuildInfo = SetupBuildInfo {
        setupDepends        :: [Dependency],
        defaultSetupDepends :: Bool
    }
    deriving (Generic, Eq, Ord, Read, Show)

data SourceRepo = SourceRepo {
  repoKind     :: RepoKind,
  repoType     :: Maybe RepoType,
  repoLocation :: Maybe String,
  repoModule   :: Maybe String,
  repoBranch   :: Maybe String,
  repoTag      :: Maybe String,
  repoSubdir   :: Maybe FilePath
}
  deriving (Generic, Eq, Ord, Read, Show)

data TestSuite = TestSuite {
        testName      :: String,
        testInterface :: TestSuiteInterface,
        testBuildInfo :: BuildInfo,
        testEnabled   :: Bool
    }
    deriving (Generic, Eq, Ord, Read, Show)

data TestSuiteInterface =
     TestSuiteExeV10 Version FilePath
   | TestSuiteLibV09 Version ModuleName
   | TestSuiteUnsupported TestType
   deriving (Generic, Eq, Ord, Read, Show)

data TestType = TestTypeExe Version
              | TestTypeLib Version
              | TestTypeUnknown String Version
    deriving (Generic, Eq, Ord, Read, Show)

data VersionRange
  = AnyVersion
  | ThisVersion            Version
  | LaterVersion           Version
  | EarlierVersion         Version
  | WildcardVersion        Version
  | UnionVersionRanges     VersionRange VersionRange
  | IntersectVersionRanges VersionRange VersionRange
  | VersionRangeParens     VersionRange
  deriving (Generic, Eq, Ord, Read, Show)
