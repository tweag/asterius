import Asterius.Boot
import System.Environment.Blank

main :: IO ()
main = do
  conf_opts <- getEnvDefault "ASTERIUS_CONFIGURE_OPTIONS" ""
  build_opts <- getEnvDefault "ASTERIUS_BUILD_OPTIONS" ""
  install_opts <- getEnvDefault "ASTERIUS_INSTALL_OPTIONS" ""
  defBootArgs <- defaultBootArgs
  boot
    defBootArgs
      { configureOptions = configureOptions defBootArgs <> " " <> conf_opts
      , buildOptions = buildOptions defBootArgs <> " " <> build_opts
      , installOptions = installOptions defBootArgs <> " " <> install_opts
      }
