import Asterius.BuildInfo
import Data.List
import System.Environment.Blank
import System.Process

main :: IO ()
main = do
  args <- getArgs
  ahc <- getAhc
  ahcPkg <- getAhcPkg
  ahcLd <- getAhcLd
  let extra_prog_args =
        [ "--with-ghc=" <> ahc,
          "--with-ghc-pkg=" <> ahcPkg,
          "--ghc-option=-fexternal-interpreter",
          "--ghc-option=-pgml" <> ahcLd
        ]
      extra_args =
        [ "--disable-shared",
          "--disable-executable-dynamic",
          "--disable-profiling",
          "--disable-debug-info",
          "--disable-library-for-ghci",
          "--disable-split-sections",
          "--disable-split-objs",
          "--disable-executable-stripping",
          "--disable-library-stripping",
          "--disable-tests",
          "--disable-coverage",
          "--disable-benchmarks",
          "--disable-relocatable"
        ]
          <> extra_prog_args
      (global_flags, command_flags) = span ("-" `isPrefixOf`) args
      new_command_flags = case command_flags of
        command : flags
          | command
              `elem` [ "new-build",
                       "new-configure",
                       "v2-build",
                       "v2-configure",
                       "v1-configure",
                       "v1-reconfigure",
                       "v1-install"
                     ] ->
            command : (extra_args <> flags)
          | command `elem` ["v1-build"] ->
            command : (extra_prog_args <> flags)
          | command
              `elem` [ "update",
                       "help",
                       "info",
                       "list",
                       "fetch",
                       "user-config",
                       "get",
                       "init",
                       "sandbox",
                       "new-update",
                       "v2-update",
                       "v1-sandbox"
                     ] ->
            command_flags
          | otherwise ->
            error $ "ahc-cabal: Unsupported command " <> command
        _ -> command_flags
      new_args = global_flags <> new_command_flags
  callProcess "cabal" new_args
