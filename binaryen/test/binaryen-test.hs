import Bindings.Binaryen.Raw
import Foreign
import Foreign.C

main :: IO ()
main = do
  m <- c_BinaryenModuleCreate
  ft <-
    withCString "func_type" $ \p0 ->
      withArray [c_BinaryenTypeInt32] $ \p1 ->
        c_BinaryenAddFunctionType m p0 c_BinaryenTypeInt32 p1 1
  e <- c_BinaryenUnreachable m
  _ <- withCString "func" $ \p0 -> c_BinaryenAddFunction m p0 ft nullPtr 0 e
  c_BinaryenModulePrint m
  c_BinaryenModuleValidate m >>= print
  c_BinaryenModuleDispose m
