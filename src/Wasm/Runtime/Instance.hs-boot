{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RoleAnnotations #-}
module Wasm.Runtime.Instance where

type role ModuleInst nominal nominal

data ModuleInst (f :: * -> *) (m :: * -> *)
