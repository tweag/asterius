import System.Directory
import System.Environment
import System.Process

main :: IO ()
main = do
  args <- getArgs
  withCurrentDirectory "test/todomvc" $ callCommand "npm install"
  callProcess "ahc-link" $
    [ "--browser"
    , "--input"
    , "test/todomvc/todomvc.hs"
    , "--output-link-report"
    , "test/todomvc/todomvc.link.txt"
    ] <>
    args
