module Asterius.Internals.Name
  ( lookupRdrNameInModule
    )
where

import HscTypes
  ( HscEnv (..),
    ModIface (..)
    )
import LoadIface (loadSysInterface)
import Module (ModuleName)
import Name (Name)
import Outputable (text)
import Packages
  ( LookupResult (..),
    lookupModuleWithSuggestions
    )
import RdrName
  ( ImpDeclSpec (..),
    ImpItemSpec (..),
    ImportSpec (..),
    RdrName,
    gre_name,
    lookupGRE_RdrName,
    mkGlobalRdrEnv
    )
import RnNames (gresFromAvails)
import SrcLoc (noSrcSpan)
import TcRnMonad
  ( initIfaceTcRn,
    initTcInteractive
    )

lookupRdrNameInModule :: HscEnv -> ModuleName -> RdrName -> IO Name
lookupRdrNameInModule hsc_env mod_name rdr_name = do
  let found_module =
        lookupModuleWithSuggestions (hsc_dflags hsc_env) mod_name Nothing
  case found_module of
    LookupFound mod _ -> do
      (_, mb_iface) <-
        initTcInteractive hsc_env $ initIfaceTcRn $ loadSysInterface doc mod
      case mb_iface of
        Just iface -> do
          let decl_spec = ImpDeclSpec
                { is_mod = mod_name,
                  is_as = mod_name,
                  is_qual = False,
                  is_dloc = noSrcSpan
                  }
              imp_spec = ImpSpec decl_spec ImpAll
              env =
                mkGlobalRdrEnv
                  (gresFromAvails (Just imp_spec) (mi_exports iface))
          case lookupGRE_RdrName rdr_name env of
            [gre] -> return (gre_name gre)
            _ ->
              fail
                "Asterius.Internals.Name.lookupRdrNameInModule: lookupGRE_RdrName failed"
        Nothing ->
          fail
            "Asterius.Internals.Name.lookupRdrNameInModule: could not determine the exports of the module"
    _ -> fail "Asterius.Internals.Name.lookupRdrNameInModule: name not found"
  where
    doc = text "contains a name used in an invocation of lookupRdrNameInModule"
