module System.Directory.Internal.Posix where
#include <HsDirectoryConfig.h>
#if !defined(mingw32_HOST_OS)
#ifdef HAVE_LIMITS_H
# include <limits.h>
#endif
import Prelude ()
import System.Directory.Internal.Prelude
#ifdef HAVE_UTIMENSAT
import System.Directory.Internal.C_utimensat
#endif
import System.Directory.Internal.Common
import System.Directory.Internal.Config (exeExtension)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (POSIXTime)
import System.FilePath ((</>), isRelative, splitSearchPath)
import qualified Data.Time.Clock.POSIX as POSIXTime
import qualified GHC.Foreign as GHC
import qualified System.Posix as Posix

createDirectoryInternal :: FilePath -> IO ()
createDirectoryInternal path = Posix.createDirectory path 0o777

removePathInternal :: Bool -> FilePath -> IO ()
removePathInternal True  = Posix.removeDirectory
removePathInternal False = Posix.removeLink

renamePathInternal :: FilePath -> FilePath -> IO ()
renamePathInternal = Posix.rename

-- | On POSIX, equivalent to 'simplifyPosix'.
simplify :: FilePath -> FilePath
simplify = simplifyPosix

-- we use the 'free' from the standard library here since it's not entirely
-- clear whether Haskell's 'free' corresponds to the same one
foreign import ccall unsafe "free" c_free :: Ptr a -> IO ()

c_PATH_MAX :: Maybe Int
#ifdef PATH_MAX
c_PATH_MAX | c_PATH_MAX' > toInteger maxValue = Nothing
           | otherwise                        = Just (fromInteger c_PATH_MAX')
  where c_PATH_MAX' = (#const PATH_MAX)
        maxValue = maxBound `asTypeInMaybe` c_PATH_MAX
        asTypeInMaybe :: a -> Maybe a -> a
        asTypeInMaybe = const
#else
c_PATH_MAX = Nothing
#endif

foreign import ccall "realpath" c_realpath :: CString -> CString -> IO CString

withRealpath :: CString -> (CString -> IO a) -> IO a
withRealpath path action = case c_PATH_MAX of
  Nothing ->
    -- newer versions of POSIX support cases where the 2nd arg is NULL;
    -- hopefully that is the case here, as there is no safer way
    bracket (realpath nullPtr) c_free action
  Just pathMax ->
    -- allocate one extra just to be safe
    allocaBytes (pathMax + 1) (realpath >=> action)
  where realpath = throwErrnoIfNull "" . c_realpath path

canonicalizePathWith :: ((FilePath -> IO FilePath) -> FilePath -> IO FilePath)
                     -> FilePath
                     -> IO FilePath
canonicalizePathWith attemptRealpath path = do
  encoding <- getFileSystemEncoding
  let realpath path' =
        GHC.withCString encoding path' (`withRealpath` GHC.peekCString encoding)
  attemptRealpath realpath path

canonicalizePathSimplify :: FilePath -> IO FilePath
canonicalizePathSimplify = pure

findExecutablesLazyInternal :: ([FilePath] -> String -> ListT IO FilePath)
                            -> String
                            -> ListT IO FilePath
findExecutablesLazyInternal findExecutablesInDirectoriesLazy binary =
  liftJoinListT $ do
    path <- getPath
    pure (findExecutablesInDirectoriesLazy path binary)

exeExtensionInternal :: String
exeExtensionInternal = exeExtension

getDirectoryContentsInternal :: FilePath -> IO [FilePath]
getDirectoryContentsInternal path =
  bracket
    (Posix.openDirStream path)
    Posix.closeDirStream
    start
  where
    start dirp = loop id
      where
        loop acc = do
          e <- Posix.readDirStream dirp
          if null e
            then pure (acc [])
            else loop (acc . (e:))

getCurrentDirectoryInternal :: IO FilePath
getCurrentDirectoryInternal = Posix.getWorkingDirectory

-- | Convert a path into an absolute path.  If the given path is relative, the
-- current directory is prepended and the path may or may not be simplified.
-- If the path is already absolute, the path is returned unchanged.  The
-- function preserves the presence or absence of the trailing path separator.
--
-- If the path is already absolute, the operation never fails.  Otherwise, the
-- operation may throw exceptions.
--
-- Empty paths are treated as the current directory.
prependCurrentDirectory :: FilePath -> IO FilePath
prependCurrentDirectory path
  | isRelative path =
    ((`ioeAddLocation` "prependCurrentDirectory") .
     (`ioeSetFileName` path)) `modifyIOError` do
      (</> path) <$> getCurrentDirectoryInternal
  | otherwise = pure path

setCurrentDirectoryInternal :: FilePath -> IO ()
setCurrentDirectoryInternal = Posix.changeWorkingDirectory

linkToDirectoryIsDirectory :: Bool
linkToDirectoryIsDirectory = False

createSymbolicLink :: Bool -> FilePath -> FilePath -> IO ()
createSymbolicLink _ = Posix.createSymbolicLink

readSymbolicLink :: FilePath -> IO FilePath
readSymbolicLink = Posix.readSymbolicLink

type Metadata = Posix.FileStatus

getSymbolicLinkMetadata :: FilePath -> IO Metadata
getSymbolicLinkMetadata = Posix.getSymbolicLinkStatus

getFileMetadata :: FilePath -> IO Metadata
getFileMetadata = Posix.getFileStatus

fileTypeFromMetadata :: Metadata -> FileType
fileTypeFromMetadata stat
  | isLink    = SymbolicLink
  | isDir     = Directory
  | otherwise = File
  where
    isLink = Posix.isSymbolicLink stat
    isDir  = Posix.isDirectory stat

fileSizeFromMetadata :: Metadata -> Integer
fileSizeFromMetadata = fromIntegral . Posix.fileSize

accessTimeFromMetadata :: Metadata -> UTCTime
accessTimeFromMetadata =
  POSIXTime.posixSecondsToUTCTime . posix_accessTimeHiRes

modificationTimeFromMetadata :: Metadata -> UTCTime
modificationTimeFromMetadata =
  POSIXTime.posixSecondsToUTCTime . posix_modificationTimeHiRes

posix_accessTimeHiRes, posix_modificationTimeHiRes
  :: Posix.FileStatus -> POSIXTime
#if MIN_VERSION_unix(2, 6, 0)
posix_accessTimeHiRes = Posix.accessTimeHiRes
posix_modificationTimeHiRes = Posix.modificationTimeHiRes
#else
posix_accessTimeHiRes = realToFrac . Posix.accessTime
posix_modificationTimeHiRes = realToFrac . Posix.modificationTime
#endif

type Mode = Posix.FileMode

modeFromMetadata :: Metadata -> Mode
modeFromMetadata = Posix.fileMode

allWriteMode :: Posix.FileMode
allWriteMode =
  Posix.ownerWriteMode .|.
  Posix.groupWriteMode .|.
  Posix.otherWriteMode

hasWriteMode :: Mode -> Bool
hasWriteMode m = m .&. allWriteMode /= 0

setWriteMode :: Bool -> Mode -> Mode
setWriteMode False m = m .&. complement allWriteMode
setWriteMode True  m = m .|. allWriteMode

setFileMode :: FilePath -> Mode -> IO ()
setFileMode = Posix.setFileMode

setFilePermissions :: FilePath -> Mode -> IO ()
setFilePermissions = setFileMode

getAccessPermissions :: FilePath -> IO Permissions
getAccessPermissions path = do
  m <- getFileMetadata path
  let isDir = fileTypeIsDirectory (fileTypeFromMetadata m)
  r <- Posix.fileAccess path True  False False
  w <- Posix.fileAccess path False True  False
  x <- Posix.fileAccess path False False True
  pure Permissions
       { readable   = r
       , writable   = w
       , executable = x && not isDir
       , searchable = x && isDir
       }

setAccessPermissions :: FilePath -> Permissions -> IO ()
setAccessPermissions path (Permissions r w e s) = do
  m <- getFileMetadata path
  setFileMode path (modifyBit (e || s) Posix.ownerExecuteMode .
                    modifyBit w Posix.ownerWriteMode .
                    modifyBit r Posix.ownerReadMode .
                    modeFromMetadata $ m)
  where
    modifyBit :: Bool -> Posix.FileMode -> Posix.FileMode -> Posix.FileMode
    modifyBit False b m = m .&. complement b
    modifyBit True  b m = m .|. b

copyOwnerFromStatus :: Posix.FileStatus -> FilePath -> IO ()
copyOwnerFromStatus st dst = do
  Posix.setOwnerAndGroup dst (Posix.fileOwner st) (-1)

copyGroupFromStatus :: Posix.FileStatus -> FilePath -> IO ()
copyGroupFromStatus st dst = do
  Posix.setOwnerAndGroup dst (-1) (Posix.fileGroup st)

tryCopyOwnerAndGroupFromStatus :: Posix.FileStatus -> FilePath -> IO ()
tryCopyOwnerAndGroupFromStatus st dst = do
  ignoreIOExceptions (copyOwnerFromStatus st dst)
  ignoreIOExceptions (copyGroupFromStatus st dst)

copyFileWithMetadataInternal :: (Metadata -> FilePath -> IO ())
                             -> (Metadata -> FilePath -> IO ())
                             -> FilePath
                             -> FilePath
                             -> IO ()
copyFileWithMetadataInternal copyPermissionsFromMetadata
                             copyTimesFromMetadata
                             src
                             dst = do
  st <- Posix.getFileStatus src
  copyFileContents src dst
  tryCopyOwnerAndGroupFromStatus st dst
  copyPermissionsFromMetadata st dst
  copyTimesFromMetadata st dst

setTimes :: FilePath -> (Maybe POSIXTime, Maybe POSIXTime) -> IO ()
#ifdef HAVE_UTIMENSAT
setTimes path' (atime', mtime') =
  withFilePath path' $ \ path'' ->
  withArray [ maybe utimeOmit toCTimeSpec atime'
            , maybe utimeOmit toCTimeSpec mtime' ] $ \ times ->
  throwErrnoPathIfMinus1_ "" path' $
    c_utimensat c_AT_FDCWD path'' times 0
#else
setTimes path' (Just atime', Just mtime') = setFileTimes' path' atime' mtime'
setTimes path' (atime', mtime') = do
  m <- getFileMetadata path'
  let atimeOld = accessTimeFromMetadata m
  let mtimeOld = modificationTimeFromMetadata m
  setFileTimes' path'
    (fromMaybe (POSIXTime.utcTimeToPOSIXSeconds atimeOld) atime')
    (fromMaybe (POSIXTime.utcTimeToPOSIXSeconds mtimeOld) mtime')

setFileTimes' :: FilePath -> POSIXTime -> POSIXTime -> IO ()
# if MIN_VERSION_unix(2, 7, 0)
setFileTimes' = Posix.setFileTimesHiRes
#  else
setFileTimes' pth atime' mtime' =
  Posix.setFileTimes pth
    (fromInteger (truncate atime'))
    (fromInteger (truncate mtime'))
# endif
#endif

-- | Get the contents of the @PATH@ environment variable.
getPath :: IO [FilePath]
getPath = splitSearchPath <$> getEnv "PATH"

getHomeDirectoryInternal :: IO FilePath
getHomeDirectoryInternal = getEnv "HOME"

getXdgDirectoryInternal :: IO FilePath -> XdgDirectory -> IO FilePath
getXdgDirectoryInternal getHomeDirectory xdgDir = do
  case xdgDir of
    XdgData   -> get "XDG_DATA_HOME"   ".local/share"
    XdgConfig -> get "XDG_CONFIG_HOME" ".config"
    XdgCache  -> get "XDG_CACHE_HOME"  ".cache"
  where
    get name fallback = do
      env <- lookupEnv name
      case env of
        Nothing   -> (</> fallback) <$> getHomeDirectory
        Just path -> pure path

getXdgDirectoryListInternal :: XdgDirectoryList -> IO [FilePath]
getXdgDirectoryListInternal xdgDirs =
  case xdgDirs of
    XdgDataDirs   -> get "XDG_DATA_DIRS"   ["/usr/local/share/", "/usr/share/"]
    XdgConfigDirs -> get "XDG_CONFIG_DIRS" ["/etc/xdg"]
  where
    get name fallback = do
      env <- lookupEnv name
      case env of
        Nothing    -> pure fallback
        Just paths -> pure (splitSearchPath paths)

getAppUserDataDirectoryInternal :: FilePath -> IO FilePath
getAppUserDataDirectoryInternal appName =
  (\ home -> home <> ('/' : '.' : appName)) <$> getHomeDirectoryInternal

getUserDocumentsDirectoryInternal :: IO FilePath
getUserDocumentsDirectoryInternal = getHomeDirectoryInternal

getTemporaryDirectoryInternal :: IO FilePath
getTemporaryDirectoryInternal = fromMaybe "/tmp" <$> lookupEnv "TMPDIR"

#endif
