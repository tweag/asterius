import Asterius.FFI
import Data.Functor
import Language.Haskell.GHC.Toolkit.Run

main :: IO ()
main = do
  (c, get_ffi_state) <- addJSFFIProcessor mempty
  void $ runHaskell defaultConfig {compiler = c} ["test/jsffi/jsffi.hs"]
  ffi_state <- get_ffi_state
  print ffi_state
