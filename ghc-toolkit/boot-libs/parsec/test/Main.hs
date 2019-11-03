
import Test.Framework

import Bugs ( bugs )
import Features ( features )

main :: IO ()
main = do
  defaultMain
    [ testGroup "Bugs" bugs
    , testGroup "Features" features
    ]
