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

module Wasm.Exec.Eval where

import Debug.Trace
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
import qualified Wasm.Runtime.Func as Func
import Wasm.Runtime.Func (ModuleRef, ModuleFunc)
import qualified Wasm.Runtime.Global as Global
import           Wasm.Runtime.Instance
import qualified Wasm.Runtime.Memory as Memory
import           Wasm.Runtime.Mutable
import           Wasm.Exec.EvalTypes
import           Wasm.Runtime.Table as Table
import           Wasm.Syntax.AST
import           Wasm.Syntax.Ops
import           Wasm.Syntax.Types
import           Wasm.Syntax.Values as Values
import           Wasm.Util.Source
import Language.Haskell.TH
import qualified Language.Haskell.TH.Syntax as TH

-- import           Debug.Trace




-- Compile time only
--
{-
getInst :: Monad m => ModuleRef -> Config f m -> ModuleInst f m
getInst ref cfg =
  let mres = view (configModules.at ref) cfg
  in case mres of
    Nothing -> error $  "Reference to unknown module #" ++ show ref
    Just x  -> x
    -}

getFrameInst :: Monad m => Config f m -> ModuleInst f m
getFrameInst = view (configFrame.frameInst)

newConfig :: IntMap (ModuleInst f m) -> ModuleInst f m -> Config f m
newConfig mods inst = Config
  {
    _configFrame   = Frame inst []
  , _configBudget  = 300
  }

plain :: Regioned f => f (Instr f) -> f (AdminInstr f m r)
plain e = Plain (value e) @@ region e
{-# INLINE plain #-}

lookup :: (Regioned f, Monad m)
       => String -> s -> Lens' s [a] -> Var f -> EvalTHS m a
lookup category inst l x@(value -> x') =
  if fromIntegral x' < length (inst^.l)
  then pure $ inst^?!l.ix (fromIntegral x')
  else throwError $
    EvalCrashError (region x) ("undefined " <> category <> " " <> show x')

assignment :: (Regioned f, Applicative m, MonadError EvalError m)
           => String -> s -> Lens' s [a] -> Var f -> a -> m s
assignment category inst l x@(value -> x') v =
  if fromIntegral x' < length (inst^.l)
  then pure $ inst & l.ix (fromIntegral (value x)) .~ v
  else throwError $
    EvalCrashError (region x) ("cannot assign " <> category <> " " <> show x')

type_ :: (Regioned f, Monad m)
      => ModuleInst f m -> Var f -> EvalTHS m FuncType
type_ inst = fmap value . lookup "type" inst (miModule.moduleTypes)

func :: (Regioned f, Monad m)
     => ModuleInst f m -> Var f -> EvalTHS m ((ModuleFunc f m))
func inst = lookup "function" inst miFuncs

table :: (Regioned f, Monad m)
      => ModuleInst f m -> Var f -> EvalTHS m ((TableInst m (Func.RuntimeFunc)))
table inst = lookup "table" inst miTables

memory :: (Regioned f, Monad m)
       => ModuleInst f m -> Var f -> EvalTHS m ((Memory.MemoryInst m))
memory inst = lookup "memory" inst miMemories

global :: (Regioned f, Monad m)
       => ModuleInst f m -> Var f -> EvalTHS m (GenHS (Global.GlobalInst m))
global inst = lookup "global" inst miGlobals

local :: (Regioned f)
      => Frame f IO -> Var f -> EvalTHS IO (GenHS (Mutable IO Value))
local frame = lookup "local" frame frameLocals

elem :: (Regioned f)
     => ModuleInst f IO -> Var f -> GenHS Table.Index -> Region
     -> EvalT IO Func.RuntimeFunc
elem inst x i at' = genHS $ do
  t <- throwTH $ table inst x
  [|| do
    x <- lift $ $$(runHS $ Table.load t i)
    case x of
      Nothing -> throwError $
        EvalTrapError at' ("uninitialized element " ++ show $$(runHS i))
      Just f -> pure f ||]

funcElem :: (Regioned f)
         => ModuleInst f IO -> Var f -> GenHS Table.Index -> Region
         -> EvalT IO Func.RuntimeFunc
funcElem = elem
{-# INLINE funcElem #-}

takeFrom :: Monad m
         => Int -> Stack a -> Region -> EvalT m [a]
takeFrom n vs at' = genHS [|| do
  if n > length $$(runHS vs)
  then throwError $ EvalCrashError at' "stack underflow"
  else pure $ take n $$(runHS vs) ||]

dropFrom :: Monad m
         => Int -> Stack a -> Region -> EvalT m [a]
dropFrom n vs at' = genHS $ [|| do
  if n > length $$(runHS vs)
  then throwError $ EvalCrashError at' "stack underflow"
  else pure $ drop n $$(runHS vs)  ||]

partialZip :: [a] -> [b] -> [Either a (Either b (a, b))]
partialZip [] [] = []
partialZip xs [] = map Left xs
partialZip [] ys = map (Right . Left) ys
partialZip (x:xs) (y:ys) = Right (Right (x, y)) : partialZip xs ys

checkTypes :: Monad m
           => Region -> [ValueType] -> [Value] -> EvalTHS m ()
checkTypes at ts xs = forM_ (partialZip ts xs) $ \case
  Left t ->
    throwError $ EvalCrashError at $ "missing argument of type " ++ show t
  Right (Left x) ->
    throwError $ EvalCrashError at $ "unexpected argument " ++ show x
  Right (Right (t, x)) | Values.typeOf x /= t ->
    throwError $ EvalCrashError at $ "expected type " ++ show t
      ++ " got " ++ show x
    | otherwise -> return ()


{- Evaluation -}

{-
 * Conventions:
 *   e  : instr
 *   v  : value
 *   es : instr list
 *   vs : value stack
 *   c : config
 -}

appStack :: Stack a -> Stack a -> Stack a
appStack vs1 vs2 = GenHS [|| $$(runHS vs1) ++ $$(runHS vs2) ||]

emptyStack :: Stack a
emptyStack = GenHS [|| [] ||]

step_work :: (Regioned f, Show1 f)
          => Stack Value -> Region -> AdminInstr f IO r
          -> (Code f IO r -> CEvalT f IO r)
          -> CEvalT f IO r
step_work vs at i k' cfg =
  let k c = k' c cfg
  in
    case i of
  Plain e' -> {-# SCC step_Plain #-} instr vs at e' k' cfg

  Trapping msg -> {-# SCC step_Trapping #-}
    genHS [|| throwError $ EvalTrapError at msg ||]
  Returning _  -> {-# SCC step_Returning #-}
    genHS [||throwError $ EvalCrashError at "undefined frame" ||]
  Breaking x _ -> {-# SCC step_Breaking #-}
    genHS [|| throwError $ EvalCrashError at ("undefined label" ++ show x) ||]

  Label _ _ (Code vs' []) -> {-# SCC step_Label1 #-}
    k $ Code (appStack vs' vs) []
  Label n es0@(Func.CompiledFunc cf) code'@(Code _ (t@(value -> c) : _)) -> {-# SCC step_Label2 #-}
    case c of
      Trapping msg -> {-# SCC step_Label3 #-}
        k $ Code vs [Trapping msg @@ region t]
      Returning vs0 -> {-# SCC step_Label4 #-}
        k $ Code vs [Returning vs0 @@ region t]
      Breaking 0 vs0 -> {-# SCC step_Label5 #-} genHS $ do
        liftIO $ print ("found match", n)
        [|| do
          abd <- $$(run $ takeFrom n vs0 at)
          $$(runHS $ cf) (abd ++ $$(runHS vs))
          ||]
      Breaking bk vs0 -> {-# SCC step_Label6 #-} genHS $ do
        liftIO $ print ("Breaking", bk)
        run $ k $ Code vs [Breaking (bk - 1) vs0 @@ at]
      _ -> {-# SCC step_Label7 #-} do
        step code' (\res ->
          k' $ Code vs [Label n es0 res @@ at]) cfg

  Framed _ _ (Code vs' []) -> {-# SCC step_Framed1 #-}
    k $ Code (appStack vs' vs) []
  Framed _ _ (Code _ (t@(value -> Trapping msg) : _)) -> {-# SCC step_Framed2 #-}
    k $ Code vs [Trapping msg @@ region t]
  Framed n _ (Code _ ((value -> Returning vs0) : _)) -> {-# SCC step_Framed3 #-} genHS $ [|| do
    vs0' <- $$(run $ takeFrom n vs0 at)
    $$(run $ k $ Code (appStack (GenHS [|| vs0' ||]) vs) [])
    ||]
  Framed n frame' code' -> {-# SCC step_Framed4 #-}
    let cfg' = cfg & configFrame .~ frame'
                   & configBudget %~ pred
    in
      step code'  (\res ->
        k' $ Code vs [Framed n frame' res @@ at]) cfg'

  Invoke func -> {-# SCC step_Invoke #-}

    let FuncType ins outs = Func.typeOf func
        n = length ins
    in genHS $ [|| do
    let budget = 100 -- $$(configBudget cfg)
    when (budget == 0) $
      throwError $ EvalExhaustionError at "call stack exhausted"


    (res :: ([Value], [Value])) <-
      if n > length $$(runHS vs)
      then throwError $ EvalCrashError at "stack underflow"
      else pure $ splitAt n $$(runHS vs)

    -- traceM $ "Invoke: ins  = " ++ show ins
    -- traceM $ "Invoke: args = " ++ show args
    -- traceM $ "Invoke: outs = " ++ show outs
    -- traceM $ "Invoke: vs'  = " ++ show vs'
    let args = reverse (fst res)
        vs' = snd res

    checkTypes at ins args

    $$(run $ case func of
      {-
      Func.AstFunc _ ref f -> genHS [|| do
        locals' <- lift $ traverse newMut $
          args ++ $$(TH.unsafeTExpCoerce $ TH.lift $ map defaultValue (value f^.funcLocals))
        $$(let
               inst' = getInst ref cfg
               code' = Code emptyStack [Plain (Fix (Block outs (value f^.funcBody))) @@ region f]
               frame' = Frame inst' (GenHS $ [|| locals' ||])
           in run $ k $ Code (GenHS [|| vs' ||]) [Framed (length outs) frame' code' @@ at])
        ||] -}

      Func.CompFunc _ (Func.CompiledFunc f) -> genHS [|| do
        res <- reverse <$> $$(runHS $ f) args
        checkTypes at outs res
        $$(run $ k $ Code (appStack (GenHS [|| res ||]) (GenHS [|| vs' ||])) [])
        -- try (reverse (f args) ++ vs', [])
        -- with Crash (_, msg) -> EvalCrashError at msg)
        ||]

      Func.HostFunc _ f -> genHS [|| do
        -- jww (2018-11-01): Need an exception handler here, so we can
        -- report host errors.
        let res = reverse ($$(runHS $ getC f) args)
        checkTypes at outs res
        $$(run $ k $ Code (appStack (GenHS [|| res ||]) (GenHS [|| vs' ||])) []) ||]
        -- try (reverse (f args) ++ vs', [])
        -- with Crash (_, msg) -> EvalCrashError at msg)

      Func.HostFuncEff _ f -> genHS [|| do
        -- jww (2018-11-01): Need an exception handler here, so we can
        -- report host errors.
        res <- lift $ reverse <$> $$(runHS $ getC f) args
        checkTypes at outs res
        $$(run $ k $ Code (appStack (GenHS [|| res ||]) (GenHS [|| vs' ||])) [])
        -- try (reverse (f args) ++ vs', [])
        -- with Crash (_, msg) -> EvalCrashError at msg)
        ||]) ||]

{-# SPECIALIZE step_work
      :: Stack Value -> Region -> AdminInstr Phrase IO r
      -> (Code Phrase IO r -> CEvalT Phrase IO r)
      -> CEvalT Phrase IO r #-}

instr :: (Regioned f, Show1 f)
      => Stack Value -> Region -> Instr f
      -> (Code f IO r -> CEvalT f IO r)
      -> CEvalT f IO r
instr vs at e' k' cfg =
  let k c = k' c cfg
  in case (unFix e', vs) of
  (Unreachable, vs)              -> {-# SCC step_Unreachable #-}
    k $ Code vs [Trapping "unreachable executed" @@ at]
  (Nop, vs)                      -> {-# SCC step_Nop #-}
    k $ Code vs []
  (Block ts es', vs)             -> genHS $ [||
--    let jump vs = $$(run $ eval (Code (GenHS [|| vs ||]) []) cfg)
    $$(run $ k $ Code vs [Label (length ts) (Func.CompiledFunc $ (GenHS $ [|| \vs -> $$(run $ k (Code (GenHS [|| vs ||]) [])) ||])) (Code emptyStack (map plain es')) @@ at]) ||]
  (Loop _ es', vs)               -> genHS $ [||
    let ll_loop l_vs = do
--          lift $ print @[Value] l_vs
          $$(run $ k (Code (GenHS [|| l_vs ||]) [Label 0 (Func.CompiledFunc (GenHS [|| ll_loop ||])) (Code emptyStack (map plain es')) @@ at]))
    in do
--        lift $ print @[Value] $$(runHS vs)
        ll_loop $$(runHS vs) ||]
  (If ts es1 es2, vs') -> genHS $ [|| do
    case $$(runHS vs) of
      I32 0 : vs' -> $$(run $ k $ Code (GenHS [|| vs' ||]) [Plain (Fix (Block ts es2)) @@ at])
      I32 _ : vs' ->
        $$(run $ k $ Code (GenHS [|| vs' ||]) [Plain (Fix (Block ts es1)) @@ at]) ||]
  (Br x, vs)                     -> {-# SCC step_Br #-}
    k $ Code emptyStack [Breaking (value x) vs @@ at]
  (BrIf x, vs)          -> {-# SCC step_BrIf1 #-} genHS $ [|| do
    case $$(runHS vs) of
      I32 0 : vs' -> $$(run $ k $ Code (GenHS [|| vs' ||]) [])
      I32 i : vs' -> lift (print i) >>
                      $$(run $ k $ Code (GenHS [|| vs' ||]) [Plain (Fix (Br x)) @@ at]) ||]
  (BrTable xs x, v) -> genHS $ [|| do
    case $$(runHS vs) of
      (I32 i) : vs' | i < 0 || fromIntegral i >= $$(liftTy $ length xs)  ->
        $$(run $ k $ Code (GenHS [|| vs' ||]) [Plain (Fix (Br x)) @@ at])
--                    | otherwise -> {-# SCC step_BrTable2 #-}
--        $$(k $ Code (GenHS [|| vs' ||]) [Plain (Fix (Br (xs !! fromIntegral i))) @@ at]) ||]
            ||]
  (Return, vs)                   -> {-# SCC step_Return #-}
    k $ Code vs [Returning vs @@ at]

  (Call x, vs) -> {-# SCC step_Call #-} genHS $ do
    let inst = getFrameInst cfg
    f <- throwTH $ func inst x
    run $ k $ Code vs [Invoke f @@ at]

  (CallIndirect x, vs) -> {-# SCC step_CallIndirect #-}
    let inst = getFrameInst cfg
    in genHS $ do
    t <- throwTH $ type_ inst x
    let runToComp :: GenHS Func.RuntimeFunc -> Func.CompiledFunc [Value]
        runToComp rf = Func.CompiledFunc (GenHS [|| Func.runFunc $$(runHS rf) ||])
    [||
      case $$(runHS vs) of
        I32 i : vs -> do
          func <- $$(run $ funcElem inst (0 @@ at) (GenHS [|| i ||]) at)
          if t /= Func.funTy func
            then $$(run $ k $ Code (GenHS [|| vs ||]) [Trapping "indirect call type mismatch" @@ at])
            else $$(run $ k $ Code (GenHS [|| vs ||]) [Invoke (Func.CompFunc t (runToComp (GenHS [|| func ||]))) @@ at]) ||]

  (Drop, vs) -> {-# SCC step_Drop #-}
    genHS $ [|| do
      case $$(runHS vs) of
        _ : vs' -> $$(run $ k $ Code (GenHS [|| vs' ||]) [])
      ||]
  (Select, vs) -> {-# SCC step_Select1 #-}
    genHS $ [|| do
      case $$(runHS vs) of
        (I32 0 : v2 : _ : vs') -> $$(run $ k $ Code (GenHS ([|| v2 : vs' ||])) [])
        (I32 _ : _ : v1 : vs') -> $$(run $ k $ Code (GenHS ([|| (v1 : vs') ||])) []) ||]

  (GetLocal x, vs) -> genHS $ do
    let frame  = _configFrame cfg
    mut <- throwTH $ local frame x
    [|| do
      l <- lift $ getMut $$(runHS mut)
      let s' = l : $$(runHS vs)
      $$(run $ k $ Code (GenHS [|| s' ||]) []) ||]

  (SetLocal x, vs ) -> genHS $ do --v : vs') -> {-# SCC step_SetLocal #-} do
    let frame = _configFrame cfg
    mut <- throwTH $ local frame x
    [|| case $$(runHS vs) of
          v : vs' -> do
--            lift $ print ("SetMut", v)
            lift $ setMut $$(runHS mut) v
            $$(run $ k $ Code (GenHS [|| vs' ||]) [] ) ||]

  (TeeLocal x, vs ) --v : vs') -> {-# SCC step_TeeLocal #-} do
    -> genHS $ do
    let frame = _configFrame cfg
    mut <- throwTH $ local frame x
    [|| case $$(runHS vs) of
          v : _ -> do
--            lift $ print ("TeeMut", v)
            lift $ setMut $$(runHS mut) v
            let s' = v : $$(runHS vs)
            $$(run $ k $ Code (GenHS [|| s' ||]) []) ||]

  (GetGlobal x, vs) -> {-# SCC step_GetGlobal #-} genHS $ do
    let inst = getFrameInst cfg
    glb_var <- throwTH $ global inst x
    [|| do
      g <- lift (Global.load $$(runHS glb_var))
      $$(run $ k $ Code (GenHS [|| g : $$(runHS vs) ||]) []) ||]

  (SetGlobal x, vs) -> genHS $ do --v : vs') -> {-# SCC step_SetGlobal #-} do

    let inst = getFrameInst cfg
    glb_var <- throwTH $ global inst x
    [|| do
      case $$(runHS vs) of
        v : vs' -> do
          eres <- lift $ runExceptT $ Global.store $$(runHS glb_var) v
          case eres of
            Right () -> $$(run $ k $ Code (GenHS [|| vs' ||]) [])
            Left err -> throwError $ EvalCrashError at $ case err of
              Global.GlobalNotMutable -> "write to immutable global"
              Global.GlobalTypeError  -> "type mismatch at global write" ||]

  (Load op, vs) -> {-# SCC step_Load #-} genHS $ do
    let inst = getFrameInst cfg
    --liftIO $ print (length (_miMemories inst))
    mem <- throwTH $ memory inst (0 @@ at)
    let off :: Int32
        off = fromIntegral (op^.memoryOffset)
        ty = op^.memoryValueType
    [||
        case $$(runHS vs) of
          (I32 i : vs') -> do
            let addr = fromIntegral $ i64_extend_u_i32 (fromIntegral i)
            eres <- lift $ runExceptT $ $$(case op^.memorySize of
              Nothing        -> [|| $$(runHS $ Memory.loadValue mem (GenHS [|| addr ||]) (GenHS $ liftTy off) (GenHS $ liftTy ty)) ||]
              Just (sz, ext) -> [|| $$(runHS $ Memory.loadPacked sz ext mem (GenHS [|| addr ||]) (GenHS $ liftTy off) (GenHS $ liftTy ty)) ||])
            case eres of
              Right v' -> do
--                lift $ print ("loaded", i)
                $$(run $ k (Code (GenHS [|| v' : vs' ||]) []))
              Left exn -> $$(run $ k (Code (GenHS [|| vs' ||]) [Trapping "mem err" @@ at])) ||]

  (Store op, vs) -> {-# SCC step_Store #-} genHS $ do
    let inst = getFrameInst cfg
    --liftIO $ print (length (_miMemories inst))
    mem <- throwTH $ memory inst (0 @@ at)
    let off = fromIntegral (op^.memoryOffset)

    [|| case $$(runHS vs) of
          v : I32 i : vs' ->  do
            let addr = fromIntegral $ i64_extend_u_i32 (fromIntegral i)
            eres <- lift $ runExceptT $ $$(case op^.memorySize of
              Nothing -> runHS $ Memory.storeValue mem (GenHS [|| addr ||]) (GenHS $ liftTy off) (GenHS [|| v ||])
              Just sz -> runHS $ Memory.storePacked sz mem (GenHS [|| addr ||]) (GenHS $ liftTy off) (GenHS [|| v ||]) )
            case eres of
              Right () -> $$(run $ k $ Code (GenHS [|| vs' ||]) [])
              Left exn -> $$(run $ k $ Code (GenHS [|| vs' ||]) [Trapping "foo" @@ at]) ||]
  (MemorySize, vs) -> {-# SCC step_MemorySize #-} genHS $ do
    let inst = getFrameInst cfg

    --liftIO $ print (length (_miMemories inst))
    mem  <- throwTH $ memory inst (0 @@ at)
    [|| do
      sz   <- lift $ $$(runHS $ Memory.size mem)
      $$(run $ k $ Code (GenHS [|| (I32 sz : $$(runHS vs)) ||]) []) ||]

  (MemoryGrow, vs) -> {-# SCC step_MemoryGrow #-} genHS $ do
    let inst    = getFrameInst cfg
    --liftIO $ print (length (_miMemories inst))
    mem     <- throwTH $ memory inst (0 @@ at)
    [|| case $$(runHS vs) of
          (I32 delta : vs') -> do
              oldSize <- lift $ $$(runHS $ Memory.size mem)
              eres    <- lift $ runExceptT $ $$(runHS $ Memory.grow mem (GenHS [|| delta ||]))
              let result = case eres of
                    Left _   -> -1
                    Right () -> oldSize
              $$(run $ k $ Code ((GenHS [|| I32 result  : vs' ||])) []) ||]

  (Const v, vs) -> {-# SCC step_Const #-}
    k $ Code (GenHS [|| $$(liftTy $ value v) : $$(runHS vs) ||]) []

  (Test testop, vs) -> {-# SCC step_Test #-} genHS $ do
    [|| case $$(runHS vs) of
          v : vs' -> do
            let eres = $$(case testop of
                  -- todo, remove branching as testop is static
                  I32TestOp o -> [|| testOp @Int32 intTestOp o v ||]
                  I64TestOp o -> [|| testOp @Int64 intTestOp o v ||])
            case eres of
              Left err -> $$(run $ k $ Code (GenHS [|| vs' ||]) [Trapping "test" @@ at])
              Right v' -> $$(run $ k $ Code (GenHS [|| (v' : vs') ||]) []) ||]

  (Compare relop, vs) -> {-# SCC step_Compare #-} genHS $
    [|| case $$(runHS vs) of
          v2 : v1 : vs' -> do
            let eres = $$(case relop of
                  I32CompareOp o -> [|| compareOp @Int32 intRelOp o v1 v2 ||]
                  I64CompareOp o -> [|| compareOp @Int64 intRelOp o v1 v2 ||]
                  F32CompareOp o -> [|| compareOp @Float floatRelOp o v1 v2 ||]
                  F64CompareOp o -> [|| compareOp @Double floatRelOp o v1 v2 ||])
            case eres of
              Left err -> $$(run $ k $ Code (GenHS [|| vs' ||]) [Trapping "compare" @@ at])
              Right v' -> $$(run $ k $ Code (GenHS [|| (v' : vs') ||]) []) ||]

  (Unary unop, vs) -> {-# SCC step_Unary #-} genHS $ do
    [|| case $$(runHS vs) of
          v : vs' -> do
            let eres = $$(case unop of
                  I32UnaryOp o -> [|| unaryOp @Int32 intUnOp o v ||]
                  I64UnaryOp o -> [|| unaryOp @Int64 intUnOp o v ||]
                  F32UnaryOp o -> [|| unaryOp @Float floatUnOp o v ||]
                  F64UnaryOp o -> [|| unaryOp @Double floatUnOp o v ||])
            case eres of
              Left err -> $$(run $ k $ Code (GenHS [|| vs' ||]) [Trapping (show "un") @@ at])
              Right v' -> $$(run $ k $ Code (GenHS [|| (v' : vs') ||]) []) ||]

  (Binary binop, vs) -> {-# SCC step_Binary #-} genHS $
    [|| case $$(runHS vs) of
          v2 : v1 : vs' -> do
              let eres = $$(case binop of
                    I32BinaryOp o -> [|| binaryOp @Int32 intBinOp o v1 v2 ||]
                    I64BinaryOp o -> [|| binaryOp @Int64 intBinOp o v1 v2 ||]
                    F32BinaryOp o -> [|| binaryOp @Float floatBinOp o v1 v2 ||]
                    F64BinaryOp o -> [|| binaryOp @Double floatBinOp o v1 v2 ||])
              case eres of
                Left err -> $$(run $ k $ Code (GenHS [|| vs' ||]) [Trapping "bin" @@ at])
                Right v' -> $$(run $ k $ Code (GenHS [|| (v' : vs') ||]) []) ||]

  (Convert cvtop, vs) -> {-# SCC step_Convert #-} genHS $ do
    [|| case $$(runHS vs) of
          v : vs' -> do
            let eres = $$(case cvtop of
                  I32ConvertOp o -> [|| intCvtOp @Int32 o v ||]
                  I64ConvertOp o -> [|| intCvtOp @Int64 o v ||]
                  F32ConvertOp o -> [|| floatCvtOp @Float o v ||]
                  F64ConvertOp o -> [|| floatCvtOp @Double o v ||])
            case eres of
              Left err -> $$(run $ k $ Code (GenHS [|| vs' ||]) [Trapping "err" @@ at])
              Right v' -> $$(run $ k $ Code (GenHS [|| (v' : vs') ||]) []) ||]
      {-
-}
{-
  _ ->  {-# SCC step_fallthrough_ #-} genHS $ [|| do
    let s1 = showVS (reverse $$(runHS vs))
        s2 = showTys (map Values.typeOf (reverse $$(runHS vs)))
    throwError $ EvalCrashError at
      ("missing or ill-typed operand on stack (" ++ s1 ++ " : " ++ s2 ++ ")") ||]
      -}

showVS :: [Value] -> String
showVS = show

showTys :: [ValueType] -> String
showTys = show


{-# SPECIALIZE instr
      :: Stack Value -> Region -> Instr Phrase
      -> (Code Phrase IO r -> CEvalT Phrase IO r)
      -> CEvalT Phrase IO r #-}

step :: (Regioned f,  Show1 f)
     => Code f IO r -> (Code f IO r -> CEvalT f IO r) -> CEvalT f IO r
step (Code _ []) _ _ = error "Cannot step without instructions"
step (Code vs (e:es)) k cfg =
  genHS $ do
    liftIO $ putStrLn $ "step: " ++ showsPrec1 11 e ""
    run $ step_work vs (region e) (value e) (k . (codeInstrs <>~ es)) cfg

{-# SPECIALIZE step
      :: Code Phrase IO r -> (Code Phrase IO r -> CEvalT Phrase IO r)
      -> CEvalT Phrase IO r #-}

eval :: (Regioned f, Show1 f)
     => Code f IO [Value] -> CEvalT f IO ([Value])
eval c@(Code vs es) cfg = case es of
  [] -> genHS [|| pure $$(runHS vs) ||]
  t@(value -> Trapping msg) : _ ->
     genHS [|| throwError $ EvalTrapError $$(liftTy (region t)) msg ||]
  _ -> step c eval cfg

{- Functions & Constants -}

invoke :: (Regioned f, Show1 f)
       => IntMap (ModuleInst f IO)
       -> ModuleInst f IO
       -> ModuleFunc f IO
       -> GenHS [Value]
       -> EvalT IO [Value]
invoke mods inst func vs =
  let (at, inst') = case func of
        Func.AstFunc _ i f -> (region f, mods^?!ix i)
        _ -> (def, inst)
  in genHS $ do
      --liftIO $ print (length (_miMemories inst'))
      [||  do
        reverse <$> $$(run $ eval (Code (GenHS [|| reverse $$(runHS vs) ||]) [Invoke func @@ at]) (newConfig mods inst')) ||]


invokeByName :: (Regioned f, Show1 f)
             => IntMap (ModuleInst f IO) -> ModuleInst f IO -> Text -> GenHS [Value]
             -> EvalTHS IO (EvalT IO [Value])
invokeByName mods inst name vs = do
  -- traceM $ "invokeByName " ++ unpack name
  case inst ^. miExports.at name of
    Just (ExternFunc f) -> return $ invoke mods inst f vs
    e -> throwError $ EvalCrashError def $
      "Cannot invoke export " ++ unpack name ++ ": "

{-
getByName :: (Regioned f, Show1 f)
          => ModuleInst f IO -> Text -> EvalT IO Value
getByName inst name = case inst ^. miExports.at name of
  Just (ExternGlobal g) -> lift $ getMut (g^.Global.giContent)
  e -> throwError $ EvalCrashError def $
    "Cannot get exported global " ++ unpack name ++ ": " ++ show e
    -}

evalConst :: (Regioned f, Show1 f)
          => IntMap (ModuleInst f IO)
          -> ModuleInst f IO -> Expr f -> EvalT IO Value
evalConst mods inst expr = genHS $ [|| do
  vs <- $$(run $ eval (Code emptyStack (map plain (value expr))) (newConfig mods inst))
  case vs of
    [v] -> pure v
    _ -> throwError $
      EvalCrashError $$(liftTy (region expr)) "wrong number of results on stack" ||]

i32 :: Monad m => Value -> Region -> EvalTHS m Int32
i32 v at = case v of
  I32 i -> pure i
  _ -> throwError $ EvalCrashError at "type error: i32 value expected"

{- Modules -}

{-
      Func.AstFunc _ ref f -> genHS [|| do
        locals' <- lift $ traverse newMut $
          args ++ $$(TH.unsafeTExpCoerce $ TH.lift $ map defaultValue (value f^.funcLocals))
        $$(let
               inst' = getInst ref cfg
               code' = Code emptyStack [Plain (Fix (Block outs (value f^.funcBody))) @@ region f]
               frame' = Frame inst' (GenHS $ [|| locals' ||])
           in run $ k $ Code (GenHS [|| vs' ||]) [Framed (length outs) frame' code' @@ at])
           -}

createVar :: ValueType -> (GenHS (Mutable IO Value) -> GenHS (EvalTHS IO r)) -> GenHS (EvalTHS IO r)
createVar t k = GenHS [|| do
  v <- lift $ newMut $$(liftTy $ defaultValue t)
  $$(runHS $ k (GenHS [|| v ||]))
  ||]

createVars :: [ValueType] -> ([GenHS (Mutable IO Value)] -> GenHS (EvalTHS IO r)) -> GenHS (EvalTHS IO r)
createVars [] k = k []
createVars (x:xs) k = createVar x (\v -> createVars xs (\vs -> k (v:vs)))

createFunc :: (Regioned f, Show1 f)
           => ModuleInst f IO -> ModuleRef -> f (Func f)
           -> (ModuleFunc f IO -> EvalTHS IO (GenHS (EvalTHS IO r)))
           -> EvalTHS IO (GenHS (EvalTHS IO r))
createFunc inst ref f k = do
  ty <- type_ inst (value f^.funcType)
  -- Make IORefs which will always be used by this function
  -- There is one variable for each argument and one for each local
  -- variable. When the function is called the argument variables are set
  -- to the arguments and the local variables reset to their default
  -- values.
  let FuncType ins outs = ty
  pure $ createVars ins (\arg_vs ->
    createVars (value f^.funcLocals) (\local_vs -> GenHS $ do
    let fun_body = [Plain (Fix (Block outs (value f^.funcBody))) @@ region f]
        frame' = (Frame inst (arg_vs ++ local_vs))
        frame_body = [Framed (length outs) frame' (Code emptyStack fun_body) @@ region f]
        cfg = Config frame' 300
        compiled_func = GenHS [|| \vs -> do
          -- Set arg vars to passed arguments
          $$(run $ setArgs arg_vs (GenHS [|| vs ||]))
          -- Reset local variables to default values
          $$(run $ setLocals (zip (map defaultValue (value f^.funcLocals)) local_vs))
          $$(run $ eval (Code (GenHS [|| vs ||]) frame_body) cfg)
          ||]

    join $ fmap runHS $ throwTH $ k $ Func.allocCompiled ty (Func.CompiledFunc compiled_func)
    ))

setLocals :: [(Value, GenHS (Mutable IO Value))] -> EvalT IO ()
setLocals [] = genHS [|| return () ||]
setLocals ((v, var): vs) = genHS [||
                            --(lift $ print ("setLocal", v)) >>
                            (lift $ setMut $$(runHS var) v)
                              >> $$(run $ setLocals vs) ||]

setArgs :: [GenHS (Mutable IO Value)] -> GenHS [Value] -> EvalT IO ()
setArgs [] as = genHS [|| return () ||]
setArgs (var: vs) as = genHS [|| case $$(runHS as) of
                                    v : vs' -> (lift $ setMut $$(runHS var) v)
--                                              >> (lift $ print ("setArgs", v))
                                                >> $$(run $ setArgs vs (GenHS [|| vs' ||])) ||]


createFuncs :: (Regioned f, Show1 f)
           => ModuleInst f IO -> ModuleRef -> [f (Func f)]
           -> ([ModuleFunc f IO] -> EvalTHS IO (GenHS (EvalTHS IO r)))
           -> EvalTHS IO (GenHS (EvalTHS IO r))
createFuncs inst ref [] k = k []
createFuncs inst ref (f:fs) k = createFunc inst ref f (\f' -> createFuncs inst ref fs (\fs' -> k (f' : fs')))


createHostFunc :: FuncType -> WQ ([Value] -> [Value]) -> ModuleFunc f m
createHostFunc = Func.allocHost

createHostFuncEff :: FuncType -> WQ ([Value] -> m [Value]) -> ModuleFunc f m
createHostFuncEff = Func.allocHostEff

createTable :: (Regioned f)
            => Table f ->
               (TableInst IO Func.RuntimeFunc -> GenHS (EvalTHS IO r))
               -> GenHS (EvalTHS IO r)
createTable tab k = Table.alloc (value tab) k

createTables :: Regioned f => [Table f]
             -> ([TableInst IO Func.RuntimeFunc] -> GenHS (EvalTHS IO r))
             -> GenHS (EvalTHS IO r)
createTables [] k = k []
createTables (x:xs) k = createTable x (\m -> createTables xs (\xs' -> k (m: xs')))

liftMem :: Monad m
        => Region -> ExceptT Memory.MemoryError m a -> EvalTHS m a
liftMem at act = do
  eres <- lift $ runExceptT act
  case eres of
    Left err -> throwError $ EvalMemoryError at err
    Right x  -> pure x

createMemory :: (Regioned f)
             => Memory f
             -> (Memory.MemoryInst IO -> GenHS (ExceptT EvalError IO r))
             -> GenHS (ExceptT EvalError IO r)
createMemory mem k = Memory.alloc (value mem) k

createMemorys :: Regioned f =>
                 [Memory f] -> ([Memory.MemoryInst IO] -> GenHS (ExceptT EvalError IO r))
                            -> GenHS (ExceptT EvalError IO r)
createMemorys [] k = k []
createMemorys (x:xs) k = createMemory x (\m -> createMemorys xs (\xs' -> k (m:xs')))

createGlobal :: (Regioned f, Show1 f)
             => IntMap (ModuleInst f IO) -> ModuleInst f IO -> f (Global f)
             -> EvalTHS IO ((EvalT IO (Global.GlobalInst IO)))
createGlobal mods inst x@(value -> glob) = pure $ genHS $ [|| do
  v <- $$(run $ evalConst mods inst (glob^.globalValue))
  eres <- lift $ runExceptT $ Global.alloc $$(liftTy (glob^.globalType)) v
  case eres of
    Left err -> throwError $ EvalGlobalError $$(liftTy (region x)) err
    Right g  -> pure g ||]


createExport :: (Regioned f, Monad m)
             => ModuleInst f m -> f (Export f) -> EvalTHS m (ExportInst f m)
createExport inst (value -> ex) = do
  ext <- case ex^.exportDesc of
    FuncExport   x -> ExternFunc   <$> func inst x
    TableExport  x -> ExternTable  <$> table inst x
    MemoryExport x -> ExternMemory <$> memory inst x
    GlobalExport x -> ExternGlobal <$> global inst x
  pure $ M.singleton (ex^.exportName) ext

initTable :: (Regioned f, Show1 f)
          => IntMap (ModuleInst f IO) -> ModuleInst f IO -> f (TableSegment f)
          -> EvalT IO ()
initTable mods inst s@(value -> seg) =
  let s = region (seg ^. segmentOffset)
      si = (seg^.segmentInit)
      fs = traverse (func inst) si
  in genHS $ do
    etab <- liftIO $ runExceptT (table inst (seg^.segmentIndex))
    let tab = either (error . show) id etab
    [|| do
      c <- $$(run $ evalConst mods inst (seg^.segmentOffset))
      offset <- i32 c s
      let end_ = offset + fromIntegral $$(liftTy $ length si)
      bound <- lift $ $$(runHS $ Table.size tab)
      when (bound < end_ || end_ < offset) $
        throwError $ EvalLinkError $$(liftTy (region (seg^.segmentIndex))) "elements segment does not fit table"
      lift $ $$(runHS $ Table.blit tab (GenHS [|| offset ||]) (GenHS [|| V.fromList $$(runHS $ genFs fs) ||]))
      ||]



genFs :: ExceptT EvalError IO [ModuleFunc f IO] -> GenHS ([Func.RuntimeFunc])
genFs fs = GenHS $ do
  efs <- liftIO $ runExceptT fs
  fs_l <- either (fail . show) return efs
  unroll fs_l
  where
    unroll [] = [|| [] ||]
    unroll (f1:fss) =
      let lf = case f1 of
            Func.AstFunc {} -> error "genFS - astFunc"
            Func.CompFunc ft (Func.CompiledFunc f) -> [|| (Func.RuntimeFunc ft $ $$(runHS f)) ||]
            Func.HostFunc ft (WQ _ w) -> [|| Func.RuntimeFunc ft (return . $$(runHS w)) ||]
            Func.HostFuncEff ft (WQ _ w) -> [|| Func.RuntimeFunc ft (lift . $$(runHS w)) ||]
      in
        [|| $$lf : $$(unroll fss) ||]

initMemory :: (Regioned f, Show1 f)
           => IntMap (ModuleInst f IO) -> ModuleInst f IO -> f (MemorySegment f)
           -> EvalTHS IO (EvalT IO ())
initMemory mods inst s@(value -> seg) = do
  mem <- memory inst (seg^.segmentIndex)
  return $ genHS [|| do
    c <- $$(run $ evalConst mods inst (seg^.segmentOffset))
    offset' <- i32 c $$(liftTy (region (seg^.segmentOffset)))
    let offset = i64_extend_u_i32 (fromIntegral offset')
    let end_ = offset + fromIntegral $$(liftTy (B.length (seg^.segmentInit)))
    bound <- lift $ $$(runHS $ Memory.bound mem)
    when (fromIntegral bound < end_ || end_ < fromIntegral offset) $
      throwError $ EvalLinkError $$(liftTy (region s)) "data segment does not fit memory"
    let v = (V.fromList ($$(liftTy (B.unpack (seg^.segmentInit)))))
    liftMem $$(liftTy (region s)) $
      $$(runHS $ Memory.storeBytes mem (GenHS [|| fromIntegral offset ||]) (GenHS [|| v ||])) ||]
      --(fromIntegral offset)

addImport :: (Regioned f, MonadRef m, Monad m)
          => ModuleInst f m
          -> Extern f m
          -> f (Import f)
          -> EvalTHS m (ModuleInst f m)
addImport inst ext im = do
  typ <- undefined --externTypeOf ext
  if not (matchExternType typ (importTypeFor (inst^.miModule) (value im)))
    then throwError $ EvalLinkError (region im) "incompatible import type"
    else pure $ case ext of
      ExternFunc func   -> inst & miFuncs    %~ (func :)
      ExternTable tab   -> inst & miTables   %~ (tab  :)
      ExternMemory mem  -> inst & miMemories %~ (mem  :)
      ExternGlobal glob -> inst & miGlobals  %~ (glob :)

resolveImports :: (Regioned f, Show1 f)
               => Map Text ModuleRef
               -> IntMap (ModuleInst f IO)
               -> ModuleInst f IO
               -> EvalTHS IO (ModuleInst f IO)
resolveImports names mods inst = flip execStateT inst $
  forM_ (reverse (inst^.miModule.moduleImports)) $ \im -> do
    let im' = value im
    case M.lookup (im'^.importModule) names of
      Nothing -> throwError $ EvalLinkError (region im) $
        "Missing module for import: " ++ show (value im)
      Just ref -> case IM.lookup ref mods of
        Nothing -> throwError $ EvalLinkError (region im) $
          "Missing module for import: " ++ show (value im)
        Just src ->
          case M.lookup (im'^.importItem) (src^.miExports) of
            Nothing -> throwError $ EvalLinkError (region im) $
              "Missing extern for import: " ++ show (value im)
            Just ext -> do
              m <- get
              m' <- lift $ addImport m ext im
              put m'

throwTH :: EvalTHS IO a -> Q a
throwTH act = do
  x <- liftIO $ runExceptT act
  case x of
    (Left err) -> fail (show err)
    Right r -> return r

initialize :: (Regioned f, Show1 f)
           => f (Module f)
           -> Map Text ModuleRef
           -> IntMap (ModuleInst f IO)
           -> ((ModuleRef, ModuleInst f IO) -> GenHS (ExceptT EvalError IO r))
           -> GenHS (ExceptT EvalError IO r)
initialize (value -> mod) names mods k = GenHS $ do
  liftIO $ print mod
  inst <- throwTH $ resolveImports names mods (emptyModuleInst mod)
  liftIO $ print inst
  let ref = nextKey mods
  liftIO $ print ref
  runHS $ createTables (mod^.moduleTables) (\ts -> GenHS $
    runHS $ createMemorys (mod^.moduleMemories) (\ms -> GenHS $
      let inst2 = inst & miTables %~ (<> ts)
                       & miMemories %~ (<> ms)
      in join $ fmap runHS $ throwTH $ createFuncs inst2 ref (mod^.moduleFuncs) (\fs ->
--      createGlobals mods inst (mod^.moduleGlobals) (\gs ->
        let inst3 = inst2
                & miFuncs    %~ (<> fs) -- & traverse.Func._AstFunc._2 .~ ref))
                & miTables   %~ (<> ts)
                & miMemories %~ (<> ms)
--                & miGlobals  %~ (<> gs)
            mods1 = IM.insert ref inst3 mods
            init_table = (flip map) (mod^.moduleElems) $ initTable mods1 inst3
        in pure $ GenHS $ do
        liftIO $ print "mem"
        init_mem <- throwTH $ forM (mod ^. moduleData) $ initMemory mods1 inst3
        es <- throwTH $ traverse (createExport inst3) (mod^.moduleExports)
        let inst4 = inst3 & miExports .~ mconcat es
        liftIO $ print "Here"
        starts <- throwTH $ forM (mod^.moduleStart) $ \start -> do
                    f <- func inst4 start
                    return $ genHS [||
                      void $ $$(run $ invoke (IM.insert ref inst4 mods) inst4 f (GenHS [|| [] ||])) ||]
        [|| do
              $$(run $ spillList init_table)
              $$(run $ spillList init_mem)
              $$(run $ fromMaybe (genHS [|| return () ||]) starts)
              $$(runHS $ k (ref, inst4)) ||] )))

spillList :: [EvalT IO ()] -> EvalT IO ()
spillList [] = genHS [|| return () ||]
spillList (x:xs) = genHS [|| $$(run x) >> $$(run $ spillList xs) ||]

{-
--  gs <- traverse (createGlobal mods inst) (mod^.moduleGlobals)

  init_mem <- forM (mod^.moduleData)  $ initMemory mods1 inst1

--  forM_ (mod^.moduleStart) $ \start -> do
--    f <- func inst3 start
--    invoke (IM.insert ref inst3 mods) inst3 f (GenHS [|| [] ||])

  return (ref, inst3)
  -}

nextKey :: IntMap a -> IM.Key
nextKey m = go (max 1 (IM.size m))
 where
  go k | IM.member k m = go (succ k)
       | otherwise = k
