-- |
-- Module      :  Asterius.Ar
-- Copyright   :  (c) 2018 EURL Tweag
-- License     :  All rights reserved (see LICENCE file in the distribution).
--
-- Loading of @ar@ files.
module Asterius.Ar
  ( loadArchive,
  )
where

import Asterius.Binary.File
import Asterius.Internals.Temp
import Asterius.Types
import Control.Monad
import Data.Either
import Data.Traversable
import qualified IfaceEnv as GHC
import System.Directory
import System.Exit
import System.FilePath
import System.Process

-------------------------------------------------------------------------------

-- | Load the contents of an archive (@.a@) file. 'loadArchive' ignores (@.o@)
-- files in the archive that cannot be parsed. Also, the metadata of the
-- contained files are ignored.
loadArchive :: GHC.NameCacheUpdater -> FilePath -> IO AsteriusCachedModule
loadArchive ncu p' = do
  p <- makeAbsolute p'
  withTempDir "ahc-ar" $ \tmpdir -> do
    (ec, out, err) <-
      readCreateProcessWithExitCode
        ((proc "ar" ["x", p]) {cwd = Just tmpdir})
        ""
    when (ec /= ExitSuccess) $ fail $ "loadArchive failed with " <> out <> err
    os <- map (tmpdir </>) <$> listDirectory tmpdir
    mconcat . rights <$> for os (tryGetFile ncu)
