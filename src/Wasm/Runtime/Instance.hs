{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Wasm.Runtime.Instance where

import           Data.Functor.Classes
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text.Lazy (Text)
import           Lens.Micro.Platform
import           Text.Show

import           Wasm.Runtime.Mutable
import           Wasm.Runtime.Func as Func
import           Wasm.Runtime.Global as Global
import           Wasm.Runtime.Memory as Memory
import           Wasm.Runtime.Table as Table
import           Wasm.Syntax.AST
import           Wasm.Syntax.Types
import           Wasm.Util.Show
import           Wasm.Util.Source

type ExportInst f m = Map Text (Extern f m)

data Extern f m
  = ExternFunc ((ModuleFunc f m))
  | ExternTable (((TableInst m (RuntimeFunc))))
  | ExternMemory (((Memory.MemoryInst m)))
  | ExternGlobal (GenHS (Global.GlobalInst m))

_ExternFunc :: Traversal' (Extern f m) ((ModuleFunc f m))
_ExternFunc f (ExternFunc x) = ExternFunc <$> f x
_ExternFunc _ x = pure x

_ExternTable :: Traversal' (Extern f m) ((TableInst m RuntimeFunc))
_ExternTable f (ExternTable x) = ExternTable <$> f x
_ExternTable _ x = pure x

_ExternMemory :: Traversal' (Extern f m) ((Memory.MemoryInst m))
_ExternMemory f (ExternMemory x) = ExternMemory <$> f x
_ExternMemory _ x = pure x

_ExternGlobal :: Traversal' (Extern f m) (GenHS (Global.GlobalInst m))
_ExternGlobal f (ExternGlobal x) = ExternGlobal <$> f x
_ExternGlobal _ x = pure x

data ModuleInst f m = ModuleInst
  { _miModule   :: Module f
  , _miFuncs    :: [(ModuleFunc f m)]
  , _miTables   :: [(TableInst m (RuntimeFunc))]
  , _miMemories :: [(MemoryInst m)]
  , _miGlobals  :: [GenHS (GlobalInst m)]
  , _miExports  :: ExportInst f m
  }


instance (Regioned f, Show1 f) => Show (ModuleInst f m) where
  showsPrec d ModuleInst {..} =
    showParen (d > 10)
      $ showString "ModuleInst {"
      . showString "\n  _miFuncs    = "
--      . foldr (\x -> ((showString "\n    " . showLiftPrec 11 x) .)) id _miFuncs
      . showString "\n  _miTables   = "
--      . showListWith (showsPrec 11) _miTables
      . showString "\n  _miMemories = "
--      . showsPrec 11 _miMemories
      . showString "\n  _miGlobals  = "
--      . showsPrec 11 _miGlobals
      . showString "\n  _miExports  = "
--      . showsPrec 11 (M.keys _miExports)
      . showString "\n}"

makeLenses ''ModuleInst

{- Auxiliary functions -}

emptyModuleInst :: Module f -> ModuleInst f m
emptyModuleInst m = ModuleInst
  { _miModule   = m
  , _miFuncs    = []
  , _miTables   = []
  , _miMemories = []
  , _miGlobals  = []
  , _miExports  = mempty
  }

{-
externTypeOf :: (MonadRef m, Monad m) => Extern f m -> GenHS (m ExternType)
externTypeOf = \case
  ExternFunc func   -> GenHS [|| pure $ ExternFuncType (Func.typeOf $$(runHS func)) ||]
  ExternTable tab   -> GenHS [|| ExternTableType <$> Table.typeOf $$(runHS tab) ||]
  ExternMemory mem  -> GenHS [|| ExternMemoryType <$> Memory.typeOf $$(runHS mem) ||]
  ExternGlobal glob -> GenHS [|| ExternGlobalType <$> Global.typeOf $$(runHS glob) ||]
  -}

export :: ModuleInst f m -> Text -> Maybe (Extern f m)
export inst name = inst^.miExports.at name
