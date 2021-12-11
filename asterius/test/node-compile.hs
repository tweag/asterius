{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

import Asterius.Internals
import qualified Asterius.Internals.FList as FList
import qualified Asterius.Marshal as OldMarshal
import qualified Asterius.NewMarshal as NewMarshal
import Asterius.Types
import Control.Exception
import Control.Monad.Except
import Data.Binary (encode)
import Data.Binary.Put
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Foreign
import GHC.Exts
import qualified Language.WebAssembly.WireFormat as Wasm
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Process
import Test.QuickCheck
import Test.QuickCheck.Monadic

type Shrink a = a -> FList.FList a

preserve :: a -> Shrink a -> Shrink a
preserve def f a = FList.snoc r def where r = f a

shrinkExpression :: Shrink Expression
shrinkExpression = preserve Unreachable $ \expr -> case expr of
  Block {..} -> do
    _new_bodys <- traverse shrinkExpression bodys
    pure expr {bodys = _new_bodys}
  If {..} -> do
    _new_ifTrue <- shrinkExpression ifTrue
    _new_ifFalse <- shrinkMaybeExpression ifFalse
    pure expr {ifTrue = _new_ifTrue, ifFalse = _new_ifFalse}
  Loop {..} -> do
    _new_body <- shrinkExpression body
    pure Loop {name = name, body = _new_body}
  Break {..} -> do
    _new_condition <- shrinkMaybeExpression breakCondition
    pure Break {name = name, breakCondition = _new_condition}
  Switch {..} -> do
    _new_condition <- shrinkExpression condition
    pure Switch
      { names = names,
        defaultName = defaultName,
        condition = _new_condition
      }
  Call {..} -> do
    _new_operands <- traverse shrinkExpression operands
    pure expr {operands = _new_operands}
  Call {..} -> do
    _new_operands <- traverse shrinkExpression operands
    pure expr {operands = _new_operands}
  CallIndirect {..} -> do
    _new_indirect_target <- shrinkExpression indirectTarget
    _new_operands <- traverse shrinkExpression operands
    pure
      expr
        { indirectTarget = _new_indirect_target,
          operands = _new_operands
        }
  SetLocal {..} -> do
    _new_value <- shrinkExpression value
    pure expr {value = _new_value}
  Load {..} -> do
    _new_ptr <- shrinkExpression ptr
    pure expr {ptr = _new_ptr}
  Store {..} -> do
    _new_ptr <- shrinkExpression ptr
    _new_value <- shrinkExpression value
    pure expr {ptr = _new_ptr, value = _new_value}
  Unary {..} -> do
    _new_operand0 <- shrinkExpression operand0
    pure expr {operand0 = _new_operand0}
  Binary {..} -> do
    _new_operand0 <- shrinkExpression operand0
    _new_operand1 <- shrinkExpression operand1
    pure expr {operand0 = _new_operand0, operand1 = _new_operand1}
  Host {..} -> do
    _new_operands <- traverse shrinkExpression operands
    pure expr {operands = _new_operands}
  _ -> mempty

shrinkMaybeExpression :: Shrink (Maybe Expression)
shrinkMaybeExpression = preserve Nothing $ \case
  Just expr -> Just <$> shrinkExpression expr
  _ -> mempty

shrinkModule' :: Shrink Module
shrinkModule' m@Module {..} = _shrink_funcs {-_shrink_memory <> _shrink_exports <>-}
  where
    _shrink_memory, _shrink_exports, _shrink_funcs :: FList.FList Module
    _shrink_memory = case memory of
      Memory {..}
        | not (BS.null memoryExportName) || not (null dataSegments) ->
          pure
            m
              { memory = memory {memoryExportName = mempty, dataSegments = mempty}
              }
        | otherwise -> mempty
    _shrink_exports
      | not (null functionExports) = pure m {functionExports = []}
      | otherwise = mempty
    _shrink_funcs = do
      let (_function_map_no_shrink, _function_map_to_shrink) =
            Map.partition
              ( \Function {..} -> case body of
                  Unreachable -> True
                  _ -> False
              )
              functionMap'
      ( do
          (_func_to_shrink_key, Function {..}) <-
            fromList $
              toList _function_map_to_shrink
          pure
            m
              { functionMap' =
                  _function_map_no_shrink
                    <> Map.insert
                      _func_to_shrink_key
                      Function
                        { functionType = functionType,
                          varTypes = varTypes,
                          body = Unreachable
                        }
                      _function_map_to_shrink
              }
        )
        <> ( do
               (_func_to_shrink_key, Function {..}) <-
                 fromList $
                   toList _function_map_to_shrink
               _shrink_expr <-
                 fromList $
                   filter (/= body) (toList (shrinkExpression body))
               pure
                 m
                   { functionMap' =
                       _function_map_no_shrink
                         <> Map.insert
                           _func_to_shrink_key
                           Function
                             { functionType = functionType,
                               varTypes = varTypes,
                               body = _shrink_expr
                             }
                           _function_map_to_shrink
                   }
           )

shrinkModule :: Module -> [Module]
shrinkModule = toList . shrinkModule'

type Backend = (String, Module -> IO LBS.ByteString)

binaryenBackend, wasmToolkitBackend :: Backend
binaryenBackend =
  ( "binaryen",
    \m -> withPool $ \pool ->
      fmap LBS.fromStrict $
        OldMarshal.marshalModule pool m
          >>= OldMarshal.serializeModule
  )
wasmToolkitBackend =
  ( "wasm-toolkit",
    \m -> case runExcept (NewMarshal.makeModule m) of
      Left err -> throwIO err
      Right r -> pure $ runPut $ Wasm.putModule r
  )

testNodeCompile :: Backend -> Module -> Property
testNodeCompile (backend_tag, backend) m = monadicIO $ do
  _result <- run $ do
    tmpdir <- getTemporaryDirectory
    bracket
      (openBinaryTempFile tmpdir "wasm-toolkit-test.wasm")
      (\(p, _) -> removeFile p)
      ( \(p, h) -> do
          buf <- backend m
          LBS.hPut h buf
          hClose h
          (_exit_code, _stdout, _stderr) <-
            readProcessWithExitCode
              "node"
              ["test" </> "node-compile" </> "node-compile.js", p]
              ""
          case _exit_code of
            ExitSuccess -> pure Nothing
            _ -> do
              (p_err, h_err) <- openTempFile tmpdir "wasm-toolkit-test-dump.bin"
              LBS.hPut h_err $ encode m
              hClose h_err
              pure $ Just (_exit_code, _stdout, _stderr, p_err)
      )
  case _result of
    Just (_exit_code, _stdout, _stderr, p_err) ->
      fail $
        "Compiling serialized module via "
          <> backend_tag
          <> " backend failed.\nExit code: "
          <> show _exit_code
          <> "\nStdout: "
          <> _stdout
          <> "\nStderr: "
          <> _stderr
          <> "\nModule binary dump path: "
          <> p_err
    _ -> pure ()

testNodeCompileWithShrink ::
  Backend -> (Module -> [Module]) -> Module -> Property
testNodeCompileWithShrink backend s m = shrinking s m $ testNodeCompile backend

testNodeCompileBoth :: (Module -> [Module]) -> Module -> Property
testNodeCompileBoth s m =
  testNodeCompileWithShrink binaryenBackend s m
    .&&. testNodeCompileWithShrink wasmToolkitBackend s m

main :: IO ()
main = do
  m_fib <- getFile $ "test" </> "fib" </> "fib.bin"
  quickCheck $ testNodeCompileBoth shrinkModule m_fib
