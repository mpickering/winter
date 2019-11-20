{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RoleAnnotations #-}
module Wasm.Runtime.Func where

type ModuleFunc (f :: * -> *) (m :: * -> *) = FuncInst f m ModuleRef

type ModuleRef = Int

type role FuncInst nominal nominal representational

data FuncInst (f :: * -> *) (m :: * -> *) (a :: *)
