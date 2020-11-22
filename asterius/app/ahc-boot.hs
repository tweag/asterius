import Asterius.Boot
import Asterius.FixEnv
import System.Environment.Blank

main :: IO ()
main = do
  fixEnv
  conf_opts <- getEnvDefault "ASTERIUS_CONFIGURE_OPTIONS" ""
  boot
    defaultBootArgs
      { configureOptions = configureOptions defaultBootArgs <> " " <> conf_opts
      }
