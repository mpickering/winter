{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}

module Wasm.Runtime.Memory where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Except
import           Data.Array.ST (newArray, readArray, MArray, STUArray)
import           Data.Array.Unsafe (castSTUArray)
import           Data.Bits
import           Data.Int
import           Data.Vector (Vector, MVector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import           Data.Word
import           GHC.ST (runST, ST)
import           Lens.Micro.Platform

import           Wasm.Runtime.Mutable
import           Wasm.Syntax.Memory
import           Wasm.Syntax.Types
import           Wasm.Syntax.Values (Value)
import qualified Wasm.Syntax.Values as Values
import Language.Haskell.TH (reportError)
import Language.Haskell.TH.Syntax (unsafeTExpCoerce)

import Control.Monad.Primitive

data MemoryInst m = MemoryInst
  { _miContent :: GenHS (Mutable m (MVector (PrimState m) Word8))
  , _miMax :: Maybe Size
  }

instance Show (MemoryInst m) where
  showsPrec _d MemoryInst {..} = showString "MemoryInst"

makeLenses ''MemoryInst

data MemoryError
  =  MemoryTypeError
  |  MemoryBoundsError
  |  MemorySizeOverflow
  |  MemorySizeLimit
  |  MemoryOutOfMemory
  deriving (Show, Eq)

pageSize :: Size
pageSize = 0x10000 {- 64 KiB -}

packedSize :: PackSize -> Size
packedSize = \case
  Pack8 -> 1
  Pack16 -> 2
  Pack32 -> 4

withinLimits :: Size -> Maybe Size -> Bool
withinLimits n = \case
  Nothing -> True
  Just m -> n <= m

create :: PrimMonad m => Size -> Either MemoryError (GenHS (m (MVector (PrimState m) Word8)))
create n
  | n > 0x10000 = Left MemorySizeOverflow
  | otherwise   = Right (GenHS [|| VM.replicate (fromIntegral (n * pageSize)) 0 ||])

alloc :: MemoryType -> (MemoryInst IO -> GenHS (ExceptT e IO r)) -> GenHS ((ExceptT e IO r))
alloc (Limits min' mmax) k = case create min' of
  Left err -> GenHS (fail (show err))
  Right m -> GenHS [|| do
    m' <- $$(runHS m)
    mem <- lift $ newMut m'
--    pure $ assert (withinLimits min' mmax) $
    $$(runHS $ k $ MemoryInst
            { _miContent = GenHS [|| mem ||]
            , _miMax = mmax
            } ) ||]

bound :: MemoryInst IO -> GenHS (IO Size)
bound mem = GenHS $ [|| bound_raw $$(runHS $ mem^.miContent) ||]

bound_raw :: (Mutable IO (MVector s a)) -> IO Size
bound_raw mem = do
  m <- getMut mem
  pure $ fromIntegral $ VM.length m

size :: MemoryInst IO -> GenHS (IO Size)
size mem = GenHS [|| do
  b <- $$(runHS $ bound mem)
  return (b `div` pageSize)
  ||]

typeOf :: MemoryInst IO -> GenHS (IO MemoryType)
typeOf mem = GenHS [|| Limits <$> $$(runHS $ size mem) <*> pure $$(liftTy (mem^.miMax)) ||]

grow :: MemoryInst IO -> GenHS Size -> GenHS (ExceptT MemoryError IO ())
grow mem delta = GenHS $ [|| do
  oldSize <- lift $ $$(runHS $ size mem)
  let newSize = oldSize + $$(runHS delta)
  if | oldSize > newSize ->
         throwError MemorySizeOverflow
     | not (withinLimits newSize $$(liftTy (mem^.miMax))) ->
         throwError MemorySizeLimit
     | newSize > 0x10000 ->
         throwError MemorySizeOverflow
     | otherwise -> lift $ do
          mv <- getMut $$(runHS $ mem^.miContent)
          mv' <- VM.grow mv (fromIntegral ($$(runHS delta) * pageSize))
          forM_ [oldSize * pageSize .. newSize * pageSize - 1] $ \i ->
             VM.write mv' (fromIntegral i) 0
          setMut $$(runHS $ mem^.miContent) mv' ||]

loadByte :: MemoryInst IO -> GenHS Address -> GenHS (ExceptT MemoryError IO Word8)
loadByte mem a = GenHS $ [|| loadByte_raw $$(runHS $ mem ^. miContent)
                                          $$(runHS $ a) ||]


loadByte_raw :: (Mutable IO (MVector RealWorld Word8)) -> Address -> (ExceptT MemoryError IO Word8)
loadByte_raw mem a = do
  bnd <- lift $ bound_raw mem
  if | a >= fromIntegral bnd
        -> throwError MemoryBoundsError
     | otherwise -> lift $ do
        mv <- getMut mem
        VM.read mv (fromIntegral a)

storeByte :: MemoryInst IO -> GenHS Address -> GenHS Word8
          -> GenHS (ExceptT MemoryError IO ())
storeByte mem ca cb = GenHS $ [|| do
  let a = $$(runHS ca)
      b = $$(runHS cb)
  bnd <- lift $ $$(runHS $ bound mem)
  if | a >= fromIntegral bnd ->
       throwError MemoryBoundsError
     | otherwise -> lift $ do
        mv <- getMut $$(runHS $ mem^.miContent)
        VM.write mv (fromIntegral a) b ||]

storeByte_raw :: Mutable IO (MVector RealWorld Word8) -> Address -> Word8 -> ExceptT MemoryError IO ()
storeByte_raw mem a b = do
  bnd <- lift $ bound_raw mem
  if | a >= fromIntegral bnd ->
       throwError MemoryBoundsError
     | otherwise -> lift $ do
        mv <- getMut mem
        VM.write mv (fromIntegral a) b

loadBytes :: MemoryInst IO -> GenHS Address -> GenHS Size
          -> GenHS (ExceptT MemoryError IO (Vector Word8))
loadBytes mem a n = GenHS [|| V.generateM (fromIntegral $$(runHS n)) $ \i ->
  $$(runHS $ loadByte mem (GenHS [|| ($$(runHS a) + fromIntegral i) ||]))  ||]

storeBytes :: MemoryInst IO -> GenHS Address -> GenHS (Vector Word8)
           -> GenHS (ExceptT MemoryError IO ())
storeBytes mem a bs = GenHS $ [|| do
  bnd <- lift $ $$(runHS $ bound mem)
  if | fromIntegral $$(runHS a) + V.length $$(runHS bs) > fromIntegral bnd ->
       throwError MemoryBoundsError
     | otherwise -> lift $ do
        mv <- getMut $$(runHS $ mem ^. miContent)
        zipWithM_ (VM.write mv) [fromIntegral $$(runHS a)..] (V.toList $$(runHS bs)) ||]

effectiveAddress :: Address -> Offset -> ExceptT MemoryError IO Address
effectiveAddress a o = do
  let ea = a + fromIntegral o
  if ea < a
    then throwError MemoryBoundsError
    else pure ea

{-
loadn :: MemoryInst IO -> GenHS Address -> GenHS Offset -> GenHS Size
      -> GenHS (ExceptT MemoryError IO Int64)
loadn mem a o n = GenHS $ [||
  let
    loop a' n' =
       if n' == 0
        then pure 0
        else do
          r <- loop (a' + 1) (n' - 1)
          let x = shiftL r 8
          b <- $$(runHS $ loadByte mem (GenHS [|| a' ||]))
          pure $ fromIntegral b .|. x
  in assert ($$(runHS n) > 0 && $$(runHS n) <= 8) $ do
      addr <- effectiveAddress $$(runHS a) $$(runHS o)
      loop addr $$(runHS n)
  ||]
  -}

loadn :: MemoryInst IO -> GenHS Address -> GenHS Offset -> GenHS Size
      -> GenHS (ExceptT MemoryError IO Int64)
loadn mem a o n = GenHS [|| loadn_raw $$(runHS $ mem ^. miContent) $$(runHS a)
                                      $$(runHS o) $$(runHS n) ||]

loadn_raw :: Mutable IO (MVector RealWorld Word8) -> Address -> Offset -> Size
      -> (ExceptT MemoryError IO Int64)
loadn_raw mem a o n =
  let
    loop a' n' =
       if n' == 0
        then pure 0
        else do
          r <- loop (a' + 1) (n' - 1)
          let x = shiftL r 8
          b <- loadByte_raw mem a'
          pure $ fromIntegral b .|. x
  in assert (n > 0 && n <= 8) $ do
      addr <- effectiveAddress a o
      loop addr n

storen ::
          MemoryInst IO -> GenHS Address -> GenHS Offset -> GenHS Size -> GenHS Int64
       -> GenHS (ExceptT MemoryError IO ())
storen mem ca co cn cx = GenHS $ [||
  let a = $$(runHS ca)
      o = $$(runHS co)
      n = $$(runHS cn)
      x = $$(runHS cx)
  in assert (n > 0 && n <= 8) $ do
      addr <- effectiveAddress a o
      storen_loop_raw $$(runHS $ mem^.miContent) addr n x
      ||]

storen_loop :: MemoryInst IO -> GenHS Address -> GenHS Size -> GenHS Int64 -> GenHS (ExceptT MemoryError IO ())
storen_loop mem addr n x = GenHS [||
    let loop :: Address -> Size -> Int64 -> ExceptT MemoryError IO ()
        loop a' n' x'
          | n' <= 0 = return ()
          | otherwise = do
            loop (a' + 1) (n' - 1) (shiftR x' 8)

            let x'' :: Word8
                x'' = fromIntegral x' .&. 0xff
            $$(runHS $ storeByte mem (GenHS [|| a' ||]) (GenHS [|| x'' ||]) )
    in loop $$(runHS addr) $$(runHS n) $$(runHS x)
    ||]

storen_loop_raw :: Mutable IO (MVector RealWorld Word8) -> Address -> Size -> Int64 -> ExceptT MemoryError IO ()
storen_loop_raw mem addr n x =
    let loop :: Address -> Size -> Int64 -> ExceptT MemoryError IO ()
        loop a' n' x'
          | n' <= 0 = return ()
          | otherwise = do
            loop (a' + 1) (n' - 1) (shiftR x' 8)

            let x'' :: Word8
                x'' = fromIntegral x' .&. 0xff
            storeByte_raw mem a' x''
    in loop addr n x



cast :: (MArray (STUArray s) a (ST s), MArray (STUArray s) b (ST s))
     => a -> ST s b
cast x = newArray (0 :: Int, 0) x >>= castSTUArray >>= flip readArray 0
{-# INLINE cast #-}

floatToBits :: Float -> Int32
floatToBits x = runST (cast x)

floatFromBits :: Int32 -> Float
floatFromBits x = runST (cast x)

doubleToBits :: Double -> Int64
doubleToBits x = runST (cast x)

doubleFromBits :: Int64 -> Double
doubleFromBits x = runST (cast x)

loadValue :: MemoryInst IO -> GenHS Address -> GenHS Offset -> ValueType
          -> GenHS (ExceptT MemoryError IO Value)
loadValue mem a o t = GenHS [||
  $$(runHS $ loadn mem a o (GenHS $ liftTy $ valueTypeSize t)) >>= \n -> pure $ $$(case t of
    I32Type -> [|| Values.I32 (fromIntegral n) ||]
    I64Type -> [|| Values.I64 n ||]
    F32Type -> [|| Values.F32 (floatFromBits (fromIntegral n)) ||]
    F64Type -> [|| Values.F64 (doubleFromBits n) ||] ) ||]

storeValue ::
              MemoryInst IO -> GenHS Address -> GenHS Offset -> GenHS Value
           -> GenHS (ExceptT MemoryError IO ())
storeValue mem a o v = GenHS [||
  let x = case $$(runHS v) of
        Values.I32 y -> fromIntegral y
        Values.I64 y -> y
        Values.F32 y -> fromIntegral $ floatToBits y
        Values.F64 y -> doubleToBits y
  in $$(runHS $ storen mem a o (GenHS [|| (valueTypeSize (Values.typeOf $$(runHS v))) ||]) (GenHS [|| x ||])) ||]

-- jww (2018-10-31): Is this type signature correct?
extend :: Address -> Offset -> Extension -> Address
extend x n = \case
  ZX -> x
  SX -> let sh = 64 - 8 * fromIntegral n in shiftR (shiftL x sh) sh

loadPacked ::
            PackSize
           -> Extension
           -> MemoryInst IO
           -> GenHS Address
           -> GenHS Offset
           -> ValueType
           -> GenHS (ExceptT MemoryError IO Value)
loadPacked sz ext mem a o t = GenHS $ [||
  assert (packedSize sz <= $$(liftTy $ valueTypeSize t)) $ do
    let n = packedSize sz
    v <- $$(runHS $ loadn mem a o (GenHS [|| n ||]))
    let n = packedSize sz
    let x = extend v n ext
    $$(case t of
      I32Type -> [|| pure $ Values.I32 (fromIntegral x) ||]
      I64Type -> [|| pure $ Values.I64 x ||]
      _ -> [|| throwError MemoryTypeError ||] ) ||]

storePacked ::
               PackSize -> MemoryInst IO -> GenHS Address -> GenHS Offset -> GenHS Value
            -> GenHS (ExceptT MemoryError IO ())
storePacked sz mem a o v = GenHS [||
  assert (packedSize sz <= valueTypeSize (Values.typeOf $$(runHS v))) $ do
    let n = packedSize sz
    x <- case $$(runHS v) of
          Values.I32 y -> pure $ fromIntegral y
          Values.I64 y -> pure y
          _ -> throwError MemoryTypeError
    $$(runHS $ storen mem a o (GenHS [|| n ||]) (GenHS [|| x ||])) ||]
