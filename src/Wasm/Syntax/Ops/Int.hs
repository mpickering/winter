{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveLift #-}

module Wasm.Syntax.Ops.Int where

import           Control.DeepSeq
import           GHC.Generics
import           GHC.TypeLits

import           Wasm.Syntax.Ops.Kind
import Language.Haskell.TH.Syntax

data family IntOp :: Nat -> * -> *

data instance IntOp bits Unary
  = Clz | Ctz | Popcnt
  deriving (Generic, NFData, Show, Lift)

data instance IntOp bits Binary
  = Add | Sub | Mul | DivS | DivU | RemS | RemU | And | Or | Xor | Shl | ShrS | ShrU | Rotl | Rotr
  deriving (Generic, NFData, Show, Lift)

data instance IntOp bits Test
  = Eqz
  deriving (Generic, NFData, Show, Lift)

data instance IntOp bits Compare
  = Eq | Ne | LtS | LtU | GtS | GtU | LeS | LeU | GeS | GeU
  deriving (Generic, NFData, Show, Lift)

data instance IntOp bits Convert
  = ExtendSI32 | ExtendUI32 | WrapI64 | TruncSF32 | TruncUF32 | TruncSF64 | TruncUF64 | ReinterpretFloat
  deriving (Generic, NFData, Show, Lift)

type I32Op = IntOp 32

type I64Op = IntOp 64
