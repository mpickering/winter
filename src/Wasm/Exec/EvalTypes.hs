{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Wasm.Exec.EvalTypes where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Except
import Control.Monad.Cont
import           Data.Functor.Identity
import           Control.Monad.Trans.Reader hiding (local)
import qualified Control.Monad.Trans.Reader as Reader
import           Control.Monad.Trans.State
import qualified Data.ByteString.Lazy as B
import           Data.Default.Class (Default(..))
import           Data.Fix
import           Data.Functor.Classes
import           Data.Int
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import           Data.List hiding (lookup, elem)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Text.Lazy (Text, unpack)
import qualified Data.Vector as V
import           Lens.Micro.Platform
import           Prelude hiding (lookup, elem)
import           Text.Show (showListWith)

import           Wasm.Exec.EvalNumeric
import {-# SOURCE #-} qualified Wasm.Runtime.Func as Func
import qualified Wasm.Runtime.Global as Global
import qualified Wasm.Runtime.Memory as Memory
import {-# SOURCE #-} Wasm.Runtime.Instance
import           Wasm.Runtime.Mutable
import           Wasm.Runtime.Table as Table
import           Wasm.Syntax.AST
import           Wasm.Syntax.Ops
import           Wasm.Syntax.Types
import           Wasm.Syntax.Values as Values
import           Wasm.Util.Source
import Language.Haskell.TH
import qualified Language.Haskell.TH.Syntax as TH

-- import           Debug.Trace
{- Errors -}

data EvalError
  = EvalLinkError Region String
  | EvalTrapError Region String
  | EvalCrashError Region String
  | EvalMemoryError Region Memory.MemoryError
  | EvalGlobalError Region Global.GlobalError
  | EvalTableError Region Table.TableError
  | EvalExhaustionError Region String
  | EvalNumericError Region NumericError
  deriving (Show, Eq)

instance Exception EvalError

memoryErrorString :: Memory.MemoryError -> String
memoryErrorString = \case
  Memory.MemoryBoundsError  -> "out of bounds memory access"
  Memory.MemorySizeOverflow -> "memory size overflow"
  Memory.MemorySizeLimit    -> "memory size limit reached"
  Memory.MemoryTypeError    -> "type mismatch at memory access"
  Memory.MemoryOutOfMemory  -> "out of memory"

{-
numericError at = \case
  NumericError.IntegerOverflow -> "integer overflow"
  NumericError.IntegerDivideByZero -> "integer divide by zero"
  NumericError.InvalidConversionToInteger -> "invalid conversion to integer"
  EvalNumeric.TypeError (i, v, t) ->
    Crash.error at
      ("type error, expected " ^ Types.string_of_value_type t ^ " as operand " ^
       string_of_int i ^ ", got " ^ Types.string_of_value_type (type_of v))
  exn -> raise exn
-}

{- Administrative Expressions & Configurations -}

type Stack a = GenHS [a]


data Frame f m = Frame
  { _frameInst :: !(ModuleInst f m)
  , _frameLocals :: [GenHS (Mutable m Value)]
  }

instance Show (Frame f m) where
  showsPrec d Frame {..}
    = showString "Frame (with "
  --s. showsString "locals" --Prec d (length _frameLocals)
    . showString " locals)"

makeLenses ''Frame

data Code f m r = Code
  { _codeStack  :: !((Stack Value))
  , _codeInstrs :: ![f (AdminInstr f m r)]
  }

instance (Regioned f, Show1 f) => Show (Code f m r) where
  showsPrec d Code {..} =
    showParen (d > 10)
      $ showString "Code "
   --   . showsPrec 11 _codeStack
      . showString " "
      . showListWith (showsPrec1 11) _codeInstrs

data AdminInstr f m r
  = Plain !(Instr f)
  | Invoke !(Func.ModuleFunc f m)
  | Trapping !String
  | Returning !(Stack Value)
  | Breaking !Int !(Stack Value)
  | Label !Int !(Func.CompiledFunc r) !(Code f m r)
  | Framed !Int !(Frame f m) !(Code f m r)

instance (Regioned f, Show1 f) => Show (AdminInstr f m r) where
  showsPrec d = showParen (d > 10) . \case
    Plain p      -> showString "Plain "     . showsPrec 11 p
    Invoke i     -> showString "Invoke "    . showsPrec1 11 i
    Trapping t   -> showString "Trapping "  . showsPrec1 11 t
    Returning r  -> showString "Returning " . showString "code"
    Breaking i s -> showString "Breaking "  . showsPrec 11 i
                                           . showString " "
   --                                        . showsPrec1 11 s
    Label i l c  -> showString "Label "     . showsPrec 11 i
                                           . showString " "
                                           -- . showListWith (showsPrec1 11) l
                                           . showString " "
                                           . showsPrec 11 c
    Framed i f c -> showString "Framed "    . showsPrec 11 i
                                           . showString " "
                                           . showsPrec 11 f
                                           . showString " "
                                           . showsPrec 11 c
makeLenses ''Code

data Config f m = Config
  { _configFrame   :: !(Frame f m)
  , _configBudget  :: !Int                {- to model stack overflow -}
  }

makeLenses ''Config

type EvalT m a = GenHST (ExceptT EvalError m) a

type EvalTHS m a = (ExceptT EvalError m) a
type CEvalT f m a = Config f m -> GenHST (ExceptT EvalError m) a

