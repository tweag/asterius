# Using `ahc-link`

`ahc-link` is the frontend program of asterius compiler. It reads Haskell modules as inputs and generates one `.wasm` WebAssembly binary file and one `.js` stub loader script, which can be run in a Node.js or Chrome runtime.

The help text of `ahc-link` is pasted here for your convenience:

```
ahc-link - Linker for the Asterius compiler

Usage: ahc-link [--browser] --input ARG [--output-wasm ARG] [--output-js ARG]
                [--output-link-report ARG] [--output-graphviz ARG] [--debug]
                [--optimize] [--output-ir] [--run] [--heap-size ARG]
                [--asterius-instance-callback ARG] [--ghc-option ARG]
                [--export-function ARG] [--extra-root-symbol ARG]
  Producing a standalone WebAssembly binary from Haskell

Available options:
  --browser                Target browsers instead of Node.js
  --input ARG              Path of the Main module
  --output-wasm ARG        Output path of WebAssembly binary, defaults to same
                           path of Main
  --output-js ARG          Output path of JavaScript, defaults to same path of
                           Main. Must be the same directory as the WebAssembly
                           binary.
  --output-link-report ARG Output path of linking report
  --output-graphviz ARG    Output path of GraphViz file of symbol dependencies
  --debug                  Enable debug mode in the runtime
  --optimize               Enable binaryen & V8 optimization
  --output-ir              Output Asterius IR of compiled modules
  --run                    Run the compiled module with Node.js
  --heap-size ARG          Heap size in MBs, used for both nursery/object pool.
                           Defaults to 1024.
  --asterius-instance-callback ARG
                           Supply a JavaScript callback expression which will be
                           invoked on the initiated asterius instance. Defaults
                           to calling Main.main
  --ghc-option ARG         Extra GHC flags
  --export-function ARG    Symbol of exported function
  --extra-root-symbol ARG  Symbol of extra root entity, e.g. Main_f_closure
  -h,--help                Show this help text
```
