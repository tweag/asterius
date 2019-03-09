module System.Directory.Internal.Common where
import Prelude ()
import System.Directory.Internal.Prelude
import System.FilePath
  ( addTrailingPathSeparator
  , hasTrailingPathSeparator
  , isPathSeparator
  , isRelative
  , joinDrive
  , joinPath
  , normalise
  , pathSeparator
  , pathSeparators
  , splitDirectories
  , splitDrive
  )

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

-- | Given a list of path segments, expand @.@ and @..@.  The path segments
-- must not contain path separators.
expandDots :: [FilePath] -> [FilePath]
expandDots = reverse . go []
  where
    go ys' xs' =
      case xs' of
        [] -> ys'
        x : xs ->
          case x of
            "." -> go ys' xs
            ".." ->
              case ys' of
                [] -> go (x : ys') xs
                ".." : _ -> go (x : ys') xs
                _ : ys -> go ys xs
            _ -> go (x : ys') xs

-- | Convert to the right kind of slashes.
normalisePathSeps :: FilePath -> FilePath
normalisePathSeps p = (\ c -> if isPathSeparator c then pathSeparator else c) <$> p

-- | Remove redundant trailing slashes and pick the right kind of slash.
normaliseTrailingSep :: FilePath -> FilePath
normaliseTrailingSep path = do
  let path' = reverse path
  let (sep, path'') = span isPathSeparator path'
  let addSep = if null sep then id else (pathSeparator :)
  reverse (addSep path'')

-- | Convert empty paths to the current directory, otherwise leave it
-- unchanged.
emptyToCurDir :: FilePath -> FilePath
emptyToCurDir ""   = "."
emptyToCurDir path = path

-- | Similar to 'normalise' but empty paths stay empty.
simplifyPosix :: FilePath -> FilePath
simplifyPosix ""   = ""
simplifyPosix path = normalise path

-- | Similar to 'normalise' but:
--
-- * empty paths stay empty,
-- * parent dirs (@..@) are expanded, and
-- * paths starting with @\\\\?\\@ are preserved.
--
-- The goal is to preserve the meaning of paths better than 'normalise'.
simplifyWindows :: FilePath -> FilePath
simplifyWindows "" = ""
simplifyWindows path =
  case drive' of
    "\\\\?\\" -> drive' <> subpath
    _ -> simplifiedPath
  where
    simplifiedPath = joinDrive drive' subpath'
    (drive, subpath) = splitDrive path
    drive' = upperDrive (normaliseTrailingSep (normalisePathSeps drive))
    subpath' = appendSep . avoidEmpty . prependSep . joinPath .
               stripPardirs . expandDots . skipSeps .
               splitDirectories $ subpath

    upperDrive d = case d of
      c : ':' : s | isAlpha c && all isPathSeparator s -> toUpper c : ':' : s
      _ -> d
    skipSeps = filter (not . (`elem` (pure <$> pathSeparators)))
    stripPardirs | pathIsAbsolute || subpathIsAbsolute = dropWhile (== "..")
                 | otherwise = id
    prependSep | subpathIsAbsolute = (pathSeparator :)
               | otherwise = id
    avoidEmpty | not pathIsAbsolute
                 && (null drive || hasTrailingPathSep) -- prefer "C:" over "C:."
                 = emptyToCurDir
               | otherwise = id
    appendSep p | hasTrailingPathSep
                  && not (pathIsAbsolute && null p)
                  = addTrailingPathSeparator p
                | otherwise = p
    pathIsAbsolute = not (isRelative path)
    subpathIsAbsolute = any isPathSeparator (take 1 subpath)
    hasTrailingPathSep = hasTrailingPathSeparator subpath

data FileType = File
              | SymbolicLink -- ^ POSIX: either file or directory link; Windows: file link
              | Directory
              | DirectoryLink -- ^ Windows only: directory link
              deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | Check whether the given 'FileType' is considered a directory by the
-- operating system.  This affects the choice of certain functions
-- e.g. 'System.Directory.removeDirectory' vs 'System.Directory.removeFile'.
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
