module Language.Haskell.GHC.Toolkit.Workarounds
  ( settingsNoTablesNextToCode
  ) where

import qualified GhcPlugins as GHC

settingsNoTablesNextToCode :: GHC.Settings -> GHC.Settings
settingsNoTablesNextToCode settings =
  settings
    { GHC.sPgm_c =
        ( fst (GHC.sPgm_c settings)
        , filter
            (/= GHC.Option "-DTABLES_NEXT_TO_CODE")
            (snd (GHC.sPgm_c settings)))
    }
