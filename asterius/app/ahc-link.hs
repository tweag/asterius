{-# LANGUAGE RecordWildCards #-}

import Asterius.Main
import Asterius.Types
import Data.Maybe
import Options.Applicative
import System.FilePath

parseTask :: Parser Task
parseTask =
  (\browser i m_input_mjs m_out_dir m_out_base no_streaming full_sym_table bundle sync binaryen dbg lr dot ir r ghc_flags export_funcs root_syms ->
     Task
       { target =
           if browser
             then Browser
             else Node
       , inputHS = i
       , inputEntryMJS = m_input_mjs
       , outputDirectory = fromMaybe (takeDirectory i) m_out_dir
       , outputBaseName = fromMaybe (takeBaseName i) m_out_base
       , noStreaming = no_streaming
       , fullSymTable = full_sym_table
       , bundle = bundle
       , sync = sync
       , binaryen = binaryen
       , debug = dbg
       , outputLinkReport = lr || dbg
       , outputGraphViz = dot || dbg
       , outputIR = ir || dbg
       , run = r
       , extraGHCFlags = ghc_flags
       , exportFunctions =
           [AsteriusEntitySymbol {entityName = sym} | sym <- export_funcs]
       , extraRootSymbols =
           [AsteriusEntitySymbol {entityName = sym} | sym <- root_syms]
       }) <$>
  switch (long "browser" <> help "Target browsers instead of Node.js") <*>
  strOption (long "input-hs" <> help "Input path of the Haskell Main module") <*>
  optional
    (strOption
       (long "input-mjs" <> help "Input path of the JavaScript entry module")) <*>
  optional
    (strOption
       (long "output-directory" <>
        help "Output directory, defaults to same directory of --input-hs")) <*>
  optional
    (strOption
       (long "output-prefix" <>
        help
          "Output file name prefix, defaults to --input-hs with .hs extension stripped")) <*>
  switch (long "no-streaming" <> help "Don't use WebAssembly.compileStreaming") <*>
  switch
    (long "full-sym-table" <>
     help
       "Generate a full symbol table. Enable this if you intend to use RTS API.") <*>
  switch (long "bundle" <> help "Generate a standalone .js file.") <*>
  switch
    (long "sync" <>
     help "Use synchronous compilation & instantiation API for WebAssembly") <*>
  switch (long "binaryen" <> help "Use the binaryen backend") <*>
  switch (long "debug" <> help "Enable debug mode in the runtime") <*>
  switch (long "output-link-report" <> help "Output linking report") <*>
  switch
    (long "output-graphviz" <>
     help "Output GraphViz file of symbol dependencies") <*>
  switch (long "output-ir" <> help "Output Asterius IR of compiled modules") <*>
  switch (long "run" <> help "Run compiled code with node") <*>
  many (strOption (long "ghc-option" <> help "Extra GHC flags")) <*>
  many
    (strOption (long "export-function" <> help "Symbol of exported function")) <*>
  many
    (strOption
       (long "extra-root-symbol" <>
        help "Symbol of extra root entity, e.g. Main_f_closure"))

opts :: ParserInfo Task
opts =
  info
    (parseTask <**> helper)
    (fullDesc <>
     progDesc "Producing a standalone WebAssembly binary from Haskell" <>
     header "ahc-link - Linker for the Asterius compiler")

main :: IO ()
main = do
  task <- execParser opts
  ahcLinkMain task
