{-# LANGUAGE CPP #-}
module DoesPathExist where
#include "util.inl"

main :: TestEnv -> IO ()
main _t = do

  T(expect) () =<< doesPathExist rootDir

  createDirectory "somedir"
  writeFile "somefile" "somedata"
  writeFile "\x3c0\x42f\x97f3\xe6\x221e" "somedata"

  T(expect) () . not =<< doesPathExist ""
  T(expect) () . not =<< doesPathExist "nonexistent"
  T(expect) () =<< doesPathExist "."
  T(expect) () =<< doesPathExist "somedir"
  T(expect) () =<< doesPathExist "somefile"
  T(expect) () =<< doesPathExist "./somefile"
#if defined(mingw32_HOST_OS)
  T(expect) () =<< doesPathExist "SoMeDiR"
  T(expect) () =<< doesPathExist "sOmEfIlE"
#endif
  T(expect) () =<< doesPathExist "\x3c0\x42f\x97f3\xe6\x221e"

  where
#if defined(mingw32_HOST_OS)
    rootDir = "C:\\"
#else
    rootDir = "/"
#endif
