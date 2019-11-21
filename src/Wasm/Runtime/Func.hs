{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Wasm.Runtime.Func where

import Data.Functor.Classes
import Wasm.Syntax.AST
import Wasm.Syntax.Types
import Wasm.Syntax.Values
import Wasm.Util.Source
import Lens.Micro.Platform
import Wasm.Exec.EvalTypes

data FuncInst f m a
  = AstFunc FuncType a (f (Func f))
  | CompFunc FuncType CompiledFunc
  | HostFunc FuncType (WQ ([Value] -> [Value]))
  | HostFuncEff FuncType (WQ ([Value] -> m [Value]))
  deriving (Functor, Foldable, Traversable)

type ModuleRef      = Int
type ModuleFunc f m = FuncInst f m ModuleRef

_AstFunc :: Traversal' (FuncInst f m a) (FuncType, a, f (Func f))
_AstFunc f (AstFunc x y z) = (\(x',y',z') -> AstFunc x' y' z') <$> f (x, y, z)
_AstFunc _ x = pure x

_HostFunc :: Traversal' (FuncInst f m a) (FuncType, WQ ([Value] -> [Value]))
_HostFunc f (HostFunc x y) = (\(x',y') -> HostFunc x' y') <$> f (x, y)
_HostFunc _ x = pure x

_HostFuncEff :: Traversal' (FuncInst f m a) (FuncType, WQ ([Value] -> m [Value]))
_HostFuncEff f (HostFuncEff x y) = (\(x',y') -> HostFuncEff x' y') <$> f (x, y)
_HostFuncEff _ x = pure x

instance (Regioned f, Show1 f, Show a) => Show (FuncInst f m a) where
  showsPrec d = \case
    AstFunc ty a f ->
      showParen (d > 10)
        $ showString "AstFunc "
        . showsPrec 11 ty
        . showString " "
        . showsPrec 11 a
        . showString " "
        . showsPrec1 11 f
    CompFunc ft _ -> showParen (d > 10) $ showString "CompiledFunc " . showsPrec 11 ft
    HostFunc ty _f ->
      showParen (d > 10)
        $ showString "HostFunc "
        . showsPrec 11 ty
    HostFuncEff ty _f ->
      showParen (d > 10)
        $ showString "HostFuncEff "
        . showsPrec 11 ty

instance (Regioned f, Show1 f) => Show1 (FuncInst f m) where
  liftShowsPrec h _k d = \case
    AstFunc ty a f ->
      showParen (d > 10)
        $ showString "AstFunc "
        . showsPrec 11 ty
        . showString " "
        . h 11 a
        . showString " "
        . showsPrec1 11 f
    CompFunc ft _ -> showParen (d > 10) $ showString "CompiledFunc " . showsPrec 11 ft
    HostFunc ty _f ->
      showParen (d > 10)
        $ showString "HostFunc "
        . showsPrec 11 ty
    HostFuncEff ty _f ->
      showParen (d > 10)
        $ showString "HostFuncEff "
        . showsPrec 11 ty

alloc :: FuncType -> a -> f (Func f) -> FuncInst f m a
alloc = AstFunc

data CompiledFunc = CompiledFunc { getComp :: !(GenHS ([Value] -> EvalTHS IO [Value])) }
data RuntimeFunc = RuntimeFunc { funTy :: FuncType
                               , runFunc :: [Value] -> EvalTHS IO [Value] }


allocCompiled :: FuncType -> CompiledFunc -> FuncInst f m a
allocCompiled = CompFunc

allocHost :: FuncType -> WQ ([Value] -> [Value]) -> FuncInst f m a
allocHost = HostFunc

allocHostEff :: FuncType -> WQ ([Value] -> m [Value]) -> FuncInst f m a
allocHostEff = HostFuncEff

typeOf :: FuncInst f m a -> FuncType
typeOf = \case
  AstFunc ft _ _ -> ft
  HostFunc ft _ -> ft
  HostFuncEff ft _ -> ft
  CompFunc ft _ -> ft
