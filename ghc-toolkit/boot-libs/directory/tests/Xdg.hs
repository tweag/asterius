{-# LANGUAGE CPP #-}
module Xdg where
#if !defined(mingw32_HOST_OS) && MIN_VERSION_base(4, 7, 0)
import System.Environment (setEnv, unsetEnv)
#endif
#include "util.inl"

main :: TestEnv -> IO ()
main _t = do

  -- smoke tests
  _ <- getXdgDirectoryList XdgDataDirs
  _ <- getXdgDirectoryList XdgConfigDirs

  T(expect) () True -- avoid warnings about redundant imports

#if !defined(mingw32_HOST_OS) && MIN_VERSION_base(4, 7, 0)
  unsetEnv "XDG_DATA_DIRS"
  unsetEnv "XDG_CONFIG_DIRS"
  T(expectEq) () ["/usr/local/share/", "/usr/share/"] =<<
    getXdgDirectoryList XdgDataDirs
  T(expectEq) () ["/etc/xdg"] =<< getXdgDirectoryList XdgConfigDirs

  setEnv "XDG_DATA_DIRS" "/a:/b:/c"
  setEnv "XDG_CONFIG_DIRS" "/d:/e:/f"
  T(expectEq) () ["/a", "/b", "/c"] =<< getXdgDirectoryList XdgDataDirs
  T(expectEq) () ["/d", "/e", "/f"] =<< getXdgDirectoryList XdgConfigDirs

  setEnv "XDG_CACHE_HOME" "g"
  T(expectEq) () "g/h" =<< getXdgDirectory XdgCache "h"
#endif

  return ()
