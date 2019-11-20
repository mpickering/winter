{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}

module Wasm.Syntax.Memory where

import           Control.DeepSeq
import           Data.Int
import           GHC.Generics
import Language.Haskell.TH.Syntax

type Size    = Int32 -- number of pages
type Address = Int64
type Offset  = Int32

data PackSize = Pack8 | Pack16 | Pack32
  deriving (Eq, Generic, NFData, Ord, Show, Lift)

data Extension = SX | ZX
  deriving (Eq, Generic, NFData, Ord, Show, Lift)
