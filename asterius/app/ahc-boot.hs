import Asterius.Boot
import System.Console.GetOpt
import System.Environment

opts :: [OptDescr ()]
opts =
  [ Option
      []
      ["rts-only"]
      (NoArg ())
      "Only recompile rts Cmm files and update current store"
  ]

main :: IO ()
main = do
  args <- getArgs
  case getOpt Permute opts args of
    ([], [], []) -> getDefaultBootArgs >>= boot
    ([()], [], []) -> do
      boot_args <- getDefaultBootArgs
      boot boot_args {rtsOnly = True}
    _ -> fail $ usageInfo "ahc-boot" opts
