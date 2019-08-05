
-- We need to do some ugly hacks here because of GHC magic

module Main (main) where

import Control.Monad
import Data.List
import Data.Maybe
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Simple.Utils
import Distribution.Text
import System.Cmd
import System.FilePath
import System.Exit
import System.Directory

main :: IO ()
main = do let hooks = autoconfUserHooks {
                  regHook = addPrimModule
                          $ regHook simpleUserHooks,
                  buildHook = buildHook simpleUserHooks,
                  haddockHook = addPrimModuleForHaddock
                              $ haddockHook simpleUserHooks }
          defaultMainWithHooks hooks

type Hook a = PackageDescription -> LocalBuildInfo -> UserHooks -> a -> IO ()

addPrimModule :: Hook a -> Hook a
addPrimModule f pd lbi uhs x =
    do let -- I'm not sure which one of these we actually need to change.
           -- It seems bad that there are two.
           pd' = addPrimModuleToPD pd
           lpd = addPrimModuleToPD (localPkgDescr lbi)
           lbi' = lbi { localPkgDescr = lpd }
       f pd' lbi' uhs x

addPrimModuleForHaddock :: Hook a -> Hook a
addPrimModuleForHaddock f pd lbi uhs x =
    do let pc = withPrograms lbi
           pc' = userSpecifyArgs "haddock" ["GHC/Prim.hs"] pc
           lbi' = lbi { withPrograms = pc' }
       f pd lbi' uhs x

addPrimModuleToPD :: PackageDescription -> PackageDescription
addPrimModuleToPD pd =
    case library pd of
    Just lib ->
        let ems = fromJust (simpleParse "GHC.Prim") : exposedModules lib
            lib' = lib { exposedModules = ems }
        in pd { library = Just lib' }
    Nothing ->
        error "Expected a library, but none found"
