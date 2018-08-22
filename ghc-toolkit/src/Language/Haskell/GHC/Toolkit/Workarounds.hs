module Language.Haskell.GHC.Toolkit.Workarounds
  ( forceModifySessionDynFlags
  , forceNoTablesNextToCode
  ) where

import qualified GhcMonad as GHC
import qualified GhcPlugins as GHC

forceModifySessionDynFlags ::
     GHC.GhcMonad m => (GHC.DynFlags -> GHC.DynFlags) -> m ()
forceModifySessionDynFlags f =
  GHC.modifySession $ \hsc_env ->
    hsc_env
      { GHC.hsc_dflags = f (GHC.hsc_dflags hsc_env)
      , GHC.hsc_IC =
          (GHC.hsc_IC hsc_env)
            {GHC.ic_dflags = f (GHC.ic_dflags (GHC.hsc_IC hsc_env))}
      }

forceNoTablesNextToCode :: GHC.GhcMonad m => m ()
forceNoTablesNextToCode =
  forceModifySessionDynFlags $ \dflags ->
    dflags
      { GHC.settings =
          (GHC.settings dflags)
            { GHC.sPgm_c =
                ( fst (GHC.pgm_c dflags)
                , filter
                    (/= GHC.Option "-DTABLES_NEXT_TO_CODE")
                    (snd (GHC.pgm_c dflags)))
            , GHC.sPgm_a =
                ( fst (GHC.pgm_a dflags)
                , filter
                    (/= GHC.Option "-DTABLES_NEXT_TO_CODE")
                    (snd (GHC.pgm_a dflags)))
            , GHC.sPgm_l =
                ( fst (GHC.pgm_l dflags)
                , filter
                    (/= GHC.Option "-DTABLES_NEXT_TO_CODE")
                    (snd (GHC.pgm_l dflags)))
            }
      }
