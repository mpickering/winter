{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Wasm.Runtime.Table where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Except
import           Data.Int (Int32)
import           Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import           Lens.Micro.Platform

import           Wasm.Runtime.Mutable
import           Wasm.Syntax.Types

type Size = Int32
type Index = Int32

data TableInst m elem = TableInst
  { _tiContent :: GenHS (Mutable m (Vector (Maybe elem)))
  , _tiMax :: Maybe Size
  , _tiElemType :: ElemType
  }

instance Show (TableInst m elem) where
  showsPrec _d TableInst {..} = showString "TableInst"

makeLenses ''TableInst

data TableError
  = TableBoundsError
  | TableSizeOverflow
  | TableSizeLimit
  deriving (Show, Eq)

withinLimits :: Size -> Maybe Size -> Bool
withinLimits sz = \case
  Nothing -> True
  Just m  -> sz <= m

create :: Size -> Vector (Maybe a)
create sz = V.replicate (fromIntegral sz) Nothing

alloc :: TableType ->
         (TableInst IO a -> GenHS (ExceptT e IO r))
         -> GenHS (ExceptT e IO r)
alloc (TableType elemType (Limits min' mmax)) k = GenHS $ [|| do
  tbl <- lift $ newMut (create min')
  $$(runHS $ k $ TableInst
          { _tiContent = (GenHS [|| tbl ||])
          , _tiMax = mmax
          , _tiElemType = elemType
          }) ||]

size :: TableInst IO a -> GenHS (IO Size)
size tab = GenHS [|| do
  content <- getMut $$(runHS $ tab^.tiContent)
  pure $ fromIntegral $ V.length content ||]

typeOf :: TableInst IO a -> GenHS (IO TableType)
typeOf tab = GenHS [|| do
  sz <- $$(runHS $ size tab)
  pure $ TableType $$(liftTy $ tab^.tiElemType) (Limits sz $$(liftTy $ tab^.tiMax)) ||]

grow :: TableInst IO a -> Size -> GenHS (IO ())
grow tab delta = GenHS [|| do
  oldSize <- $$(runHS $ size tab)
  let newSize = oldSize + delta
  if oldSize > newSize
    then $$(fail (show TableSizeOverflow))
    else if not (withinLimits newSize $$(liftTy (tab^.tiMax)))
         then $$(fail (show TableSizeLimit))
         else modifyMut $$(runHS $ tab^.tiContent) $ \v -> V.create $ do
           mv <- V.thaw v
           VM.grow mv (fromIntegral (newSize - oldSize)) ||]

load :: TableInst IO a -> GenHS Index -> GenHS (IO (Maybe a))
load tab i = GenHS [|| do
  content <- getMut $$(runHS $ tab^.tiContent)
  pure $ join $ content V.!? fromIntegral $$(runHS i) ||]

store :: TableInst IO a -> GenHS Index -> GenHS a -> GenHS (IO ())
store tab i v = GenHS [||
  modifyMut $$(runHS $ tab^.tiContent) $
    V.modify (\vec -> VM.write vec (fromIntegral $$(runHS i)) (Just $$(runHS v))) ||]

blit :: TableInst IO a -> GenHS Index -> GenHS (Vector a) -> GenHS (IO ())
blit tab offset elems = GenHS [||
  -- V.blit dat 0l (tab^.tiContent) offset (V.length dat)
  modifyMut $$(runHS $ tab^.tiContent)
    (V.// zip [fromIntegral $$(runHS offset)..] (map Just (V.toList $$(runHS elems)))) ||]
