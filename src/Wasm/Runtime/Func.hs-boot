{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RoleAnnotations #-}
module Wasm.Runtime.Func where

import Wasm.Util.Source
import Data.Functor.Classes

instance (Regioned f, Show1 f) => Show1 (FuncInst f m) where

type ModuleFunc (f :: * -> *) (m :: * -> *) = FuncInst f m ModuleRef

type ModuleRef = Int

type role FuncInst nominal nominal representational

data FuncInst (f :: * -> *) (m :: * -> *) (a :: *)

data CompiledFunc

