import Asterius.Boot
import System.Environment.Blank

main :: IO ()
main = do
  conf_opts <- getEnvDefault "ASTERIUS_CONFIGURE_OPTIONS" ""
  defaultBootArgs <- getDefaultBootArgs
  boot
    defaultBootArgs
      { configureOptions = configureOptions defaultBootArgs <> " " <> conf_opts
      }
