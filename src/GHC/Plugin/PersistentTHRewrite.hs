module GHC.Plugin.PersistentThRewrite where

import CoreSyn
import GhcPlugins
import HsDecls
import HsDumpAst
import HsExtension
import HsSyn
import OccName
import RdrName
import TcEvidence
import Var

rewriteModule
   :: HsModule GhcPs
   -> HsModule GhcPs
rewriteModule _ = undefined

