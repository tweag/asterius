import System.Process

main :: IO ()
main = callProcess "ahc-link" ["--input", "test/jsffi/jsffi.hs"]
