module System.Directory.Internal.Common where
import Prelude ()
import System.Directory.Internal.Prelude
import System.FilePath ((</>), isPathSeparator, isRelative,
                        pathSeparator, splitDrive, takeDrive)

-- | A generator with side-effects.
newtype ListT m a = ListT { unListT :: m (Maybe (a, ListT m a)) }

emptyListT :: Applicative m => ListT m a
emptyListT = ListT (pure Nothing)

maybeToListT :: Applicative m => m (Maybe a) -> ListT m a
maybeToListT m = ListT (((\ x -> (x, emptyListT)) <$>) <$> m)

listToListT :: Applicative m => [a] -> ListT m a
listToListT [] = emptyListT
listToListT (x : xs) = ListT (pure (Just (x, listToListT xs)))

liftJoinListT :: Monad m => m (ListT m a) -> ListT m a
liftJoinListT m = ListT (m >>= unListT)

listTHead :: Functor m => ListT m a -> m (Maybe a)
listTHead (ListT m) = (fst <$>) <$> m

listTToList :: Monad m => ListT m a -> m [a]
listTToList (ListT m) = do
  mx <- m
  case mx of
    Nothing -> return []
    Just (x, m') -> do
      xs <- listTToList m'
      return (x : xs)

andM :: Monad m => m Bool -> m Bool -> m Bool
andM mx my = do
  x <- mx
  if x
    then my
    else return x

sequenceWithIOErrors_ :: [IO ()] -> IO ()
sequenceWithIOErrors_ actions = go (Right ()) actions
  where

    go :: Either IOError () -> [IO ()] -> IO ()
    go (Left e)   []       = ioError e
    go (Right ()) []       = pure ()
    go s          (m : ms) = s `seq` do
      r <- tryIOError m
      go (thenEither s r) ms

    -- equivalent to (*>) for Either, defined here to retain compatibility
    -- with base prior to 4.3
    thenEither :: Either b a -> Either b a -> Either b a
    thenEither x@(Left _) _ = x
    thenEither _          y = y

-- | Similar to 'try' but only catches a specify kind of 'IOError' as
--   specified by the predicate.
tryIOErrorType :: (IOError -> Bool) -> IO a -> IO (Either IOError a)
tryIOErrorType check action = do
  result <- tryIOError action
  case result of
    Left  err -> if check err then pure (Left err) else throwIO err
    Right val -> pure (Right val)

-- | Attempt to perform the given action, silencing any IO exception thrown by
-- it.
ignoreIOExceptions :: IO () -> IO ()
ignoreIOExceptions io = io `catchIOError` (\_ -> pure ())

specializeErrorString :: String -> (IOError -> Bool) -> IO a -> IO a
specializeErrorString str errType action = do
  mx <- tryIOErrorType errType action
  case mx of
    Left  e -> throwIO (ioeSetErrorString e str)
    Right x -> pure x

ioeAddLocation :: IOError -> String -> IOError
ioeAddLocation e loc = do
  ioeSetLocation e newLoc
  where
    newLoc = loc <> if null oldLoc then "" else ":" <> oldLoc
    oldLoc = ioeGetLocation e

data FileType = File
              | SymbolicLink -- ^ POSIX: either file or directory link; Windows: file link
              | Directory
              | DirectoryLink -- ^ Windows only: directory link
              deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | Check whether the given 'FileType' is considered a directory by the
-- operating system.  This affects the choice of certain functions
-- e.g. `removeDirectory` vs `removeFile`.
fileTypeIsDirectory :: FileType -> Bool
fileTypeIsDirectory Directory     = True
fileTypeIsDirectory DirectoryLink = True
fileTypeIsDirectory _             = False

-- | Return whether the given 'FileType' is a link.
fileTypeIsLink :: FileType -> Bool
fileTypeIsLink SymbolicLink  = True
fileTypeIsLink DirectoryLink = True
fileTypeIsLink _             = False

data Permissions
  = Permissions
  { readable :: Bool
  , writable :: Bool
  , executable :: Bool
  , searchable :: Bool
  } deriving (Eq, Ord, Read, Show)

-- | Convert a path into an absolute path.  If the given path is relative, the
-- current directory is prepended.  If the path is already absolute, the path
-- is returned unchanged.  The function preserves the presence or absence of
-- the trailing path separator.
--
-- If the path is already absolute, the operation never fails.  Otherwise, the
-- operation may fail with the same exceptions as 'getCurrentDirectory'.
--
-- (internal API)
prependCurrentDirectoryWith :: IO FilePath -> FilePath -> IO FilePath
prependCurrentDirectoryWith getCurrentDirectory path =
  ((`ioeAddLocation` "prependCurrentDirectory") .
   (`ioeSetFileName` path)) `modifyIOError` do
    if isRelative path -- avoid the call to `getCurrentDirectory` if we can
    then do
      cwd <- getCurrentDirectory
      let curDrive = takeWhile (not . isPathSeparator) (takeDrive cwd)
      let (drive, subpath) = splitDrive path
      -- handle drive-relative paths (Windows only)
      pure . (</> subpath) $
        case drive of
          _ : _ | (toUpper <$> drive) /= (toUpper <$> curDrive) ->
                    drive <> [pathSeparator]
          _ -> cwd
    else pure path

-- | Truncate the destination file and then copy the contents of the source
-- file to the destination file.  If the destination file already exists, its
-- attributes shall remain unchanged.  Otherwise, its attributes are reset to
-- the defaults.
copyFileContents :: FilePath            -- ^ Source filename
                 -> FilePath            -- ^ Destination filename
                 -> IO ()
copyFileContents fromFPath toFPath =
  (`ioeAddLocation` "copyFileContents") `modifyIOError` do
    withBinaryFile toFPath WriteMode $ \ hTo ->
      copyFileToHandle fromFPath hTo

-- | Copy all data from a file to a handle.
copyFileToHandle :: FilePath            -- ^ Source file
                 -> Handle              -- ^ Destination handle
                 -> IO ()
copyFileToHandle fromFPath hTo =
  (`ioeAddLocation` "copyFileToHandle") `modifyIOError` do
    withBinaryFile fromFPath ReadMode $ \ hFrom ->
      copyHandleData hFrom hTo

-- | Copy data from one handle to another until end of file.
copyHandleData :: Handle                -- ^ Source handle
               -> Handle                -- ^ Destination handle
               -> IO ()
copyHandleData hFrom hTo =
  (`ioeAddLocation` "copyData") `modifyIOError` do
    allocaBytes bufferSize go
  where
    bufferSize = 131072 -- 128 KiB, as coreutils `cp` uses as of May 2014 (see ioblksize.h)
    go buffer = do
      count <- hGetBuf hFrom buffer bufferSize
      when (count > 0) $ do
        hPutBuf hTo buffer count
        go buffer

-- | Special directories for storing user-specific application data,
--   configuration, and cache files, as specified by the
--   <http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html XDG Base Directory Specification>.
--
--   Note: On Windows, 'XdgData' and 'XdgConfig' map to the same directory.
--
--   @since 1.2.3.0
data XdgDirectory
  = XdgData
    -- ^ For data files (e.g. images).
    --   Defaults to @~\/.local\/share@ and can be
    --   overridden by the @XDG_DATA_HOME@ environment variable.
    --   On Windows, it is @%APPDATA%@
    --   (e.g. @C:\/Users\//\<user\>/\/AppData\/Roaming@).
    --   Can be considered as the user-specific equivalent of @\/usr\/share@.
  | XdgConfig
    -- ^ For configuration files.
    --   Defaults to @~\/.config@ and can be
    --   overridden by the @XDG_CONFIG_HOME@ environment variable.
    --   On Windows, it is @%APPDATA%@
    --   (e.g. @C:\/Users\//\<user\>/\/AppData\/Roaming@).
    --   Can be considered as the user-specific equivalent of @\/etc@.
  | XdgCache
    -- ^ For non-essential files (e.g. cache).
    --   Defaults to @~\/.cache@ and can be
    --   overridden by the @XDG_CACHE_HOME@ environment variable.
    --   On Windows, it is @%LOCALAPPDATA%@
    --   (e.g. @C:\/Users\//\<user\>/\/AppData\/Local@).
    --   Can be considered as the user-specific equivalent of @\/var\/cache@.
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | Search paths for various application data, as specified by the
--   <http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html XDG Base Directory Specification>.
--
--   Note: On Windows, 'XdgDataDirs' and 'XdgConfigDirs' yield the same result.
--
--   @since 1.3.2.0
data XdgDirectoryList
  = XdgDataDirs
    -- ^ For data files (e.g. images).
    --   Defaults to @/usr/local/share/@ and @/usr/share/@ and can be
    --   overridden by the @XDG_DATA_DIRS@ environment variable.
    --   On Windows, it is @%PROGRAMDATA%@ or @%ALLUSERSPROFILE%@
    --   (e.g. @C:\/ProgramData@).
  | XdgConfigDirs
    -- ^ For configuration files.
    --   Defaults to @/etc/xdg@ and can be
    --   overridden by the @XDG_CONFIG_DIRS@ environment variable.
    --   On Windows, it is @%PROGRAMDATA%@ or @%ALLUSERSPROFILE%@
    --   (e.g. @C:\/ProgramData@).
  deriving (Bounded, Enum, Eq, Ord, Read, Show)
