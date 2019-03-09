{-# LANGUAGE CPP #-}
module Simplify where
#include "util.inl"
import System.Directory.Internal (simplifyWindows)
import System.FilePath (normalise)

main :: TestEnv -> IO ()
main _t = do
  T(expectIOErrorType) () (const True) (setCurrentDirectory "")
  T(expectEq) () (simplifyWindows "") ""
  T(expectEq) () (simplifyWindows ".") "."
  T(expectEq) () (simplifyWindows "a///b") (normalise "a/b")
  T(expectEq) () (simplifyWindows "./a//b") (normalise "a/b")
  T(expectEq) () (simplifyWindows "a/../../../b/.") (normalise "../../b")
  T(expectEq) () (simplifyWindows "a/.././b/./") (normalise "b/")
  T(expectEq) () (simplifyWindows "C:/a/../b") (normalise "C:/b")
  T(expectEq) () (simplifyWindows "\\\\?\\./a\\../b") "\\\\?\\./a\\../b"
  T(expectEq) () (simplifyWindows "C:/a") (normalise "C:/a")
  T(expectEq) () (simplifyWindows "/a") (normalise "/a")
#ifdef mingw32_HOST_OS
  T(expectEq) () (simplifyWindows "C:") "C:"
  T(expectEq) () (simplifyWindows "c:\\\\") "C:\\"
  T(expectEq) () (simplifyWindows "C:.") "C:"
  T(expectEq) () (simplifyWindows "C:.\\\\") "C:.\\"
  T(expectEq) () (simplifyWindows "C:..") "C:.."
  T(expectEq) () (simplifyWindows "C:..\\") "C:..\\"
  T(expectEq) () (simplifyWindows "C:\\.\\") "C:\\"
  T(expectEq) () (simplifyWindows "C:\\a") "C:\\a"
  T(expectEq) () (simplifyWindows "C:\\a\\\\b\\") "C:\\a\\b\\"
  T(expectEq) () (simplifyWindows "\\\\a\\b") "\\\\a\\b"
  T(expectEq) () (simplifyWindows "//a\\b/c/./d") "\\\\a\\b\\c\\d"
  T(expectEq) () (simplifyWindows "/.") "\\"
  T(expectEq) () (simplifyWindows "/./") "\\"
  T(expectEq) () (simplifyWindows "/../") "\\"
  T(expectEq) () (simplifyWindows "\\a\\.") "\\a"
  T(expectEq) () (simplifyWindows "//?") "\\\\?"
  T(expectEq) () (simplifyWindows "//?\\") "\\\\?\\"
  T(expectEq) () (simplifyWindows "//?/a/b") "\\\\?\\a/b"
#endif
