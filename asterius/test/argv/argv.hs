import System.Environment

main :: IO ()
main = getArgs >>= print  -- expected output: ["extra","flags"]
