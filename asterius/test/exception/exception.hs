{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception
import System.IO.Error

main :: IO ()
main = catch (throwIO (userError "BOOM")) (\(e :: SomeException) -> print e)
