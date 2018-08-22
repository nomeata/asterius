module Language.Haskell.GHC.Toolkit.FrontendPlugin
  ( makeFrontendPlugin
  ) where

import Config
import Control.Monad
import Data.List
import DriverPhases
import DriverPipeline
import GHC
import GhcPlugins hiding ((<>))
import Language.Haskell.GHC.Toolkit.Compiler
import Language.Haskell.GHC.Toolkit.Hooks
import Panic

makeFrontendPlugin :: Ghc Compiler -> FrontendPlugin
makeFrontendPlugin init_c =
  defaultFrontendPlugin
    { frontend =
        \_ targets -> do
          c <- init_c
          flip gfinally (finalize c) $ do
            h <- liftIO $ hooksFromCompiler c
            let (hs_targets, non_hs_targets) =
                  partition isHaskellishTarget targets
            if null hs_targets
              then do
                dflags <- getSessionDynFlags
                void $
                  setSessionDynFlags
                    dflags
                      { ghcLink = NoLink
                      , settings =
                          (settings dflags)
                            { sPgm_c =
                                ( fst (sPgm_c (settings dflags))
                                , filter
                                    (/= Option "-DTABLES_NEXT_TO_CODE")
                                    (snd (sPgm_c (settings dflags))))
                            , sPgm_a =
                                ( fst (sPgm_a (settings dflags))
                                , filter
                                    (/= Option "-DTABLES_NEXT_TO_CODE")
                                    (snd (sPgm_a (settings dflags))))
                            , sPgm_l =
                                ( fst (sPgm_l (settings dflags))
                                , filter
                                    (/= Option "-DTABLES_NEXT_TO_CODE")
                                    (snd (sPgm_l (settings dflags))))
                            }
                      , integerLibrary = IntegerSimple
                      , tablesNextToCode = False
                      , hooks = h
                      }
                env <- getSession
                liftIO $ oneShot env StopLn targets
              else do
                do dflags <- getSessionDynFlags
                   void $
                     setSessionDynFlags
                       dflags
                         { settings =
                             (settings dflags)
                               { sPgm_c =
                                   ( fst (sPgm_c (settings dflags))
                                   , filter
                                       (/= Option "-DTABLES_NEXT_TO_CODE")
                                       (snd (sPgm_c (settings dflags))))
                               , sPgm_a =
                                   ( fst (sPgm_a (settings dflags))
                                   , filter
                                       (/= Option "-DTABLES_NEXT_TO_CODE")
                                       (snd (sPgm_a (settings dflags))))
                               , sPgm_l =
                                   ( fst (sPgm_l (settings dflags))
                                   , filter
                                       (/= Option "-DTABLES_NEXT_TO_CODE")
                                       (snd (sPgm_l (settings dflags))))
                               }
                         , integerLibrary = IntegerSimple
                         , tablesNextToCode = False
                         , hooks = h
                         }
                env <- getSession
                o_files <-
                  liftIO $ traverse (compileFile env StopLn) non_hs_targets
                dflags <- getSessionDynFlags
                void $
                  setSessionDynFlags
                    dflags
                      { ghcMode = CompManager
                      , ldInputs =
                          map (FileOption "") o_files ++ ldInputs dflags
                      }
                traverse (uncurry GHC.guessTarget) hs_targets >>= setTargets
                ok_flag <- load LoadAllTargets
                when (failed ok_flag) $
                  liftIO $
                  throwGhcExceptionIO $ Panic "GHC.load returned Failed."
    }
