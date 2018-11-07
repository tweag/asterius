import Asterius.Boot

main :: IO ()
main = boot defaultBootArgs {buildOptions = buildOptions defaultBootArgs}
