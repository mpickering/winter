{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Wasm.Runtime.Mutable where

import Data.IORef
import Wasm.Syntax.Types

class MonadRef m where
  data Mutable m a :: *

  newMut    :: a -> m (Mutable m a)
  getMut    :: Mutable m a -> m a
  setMut    :: Mutable m a -> a -> m ()
  modifyMut :: Mutable m a -> (a -> a) -> m ()

instance MonadRef IO where
  newtype Mutable IO a = IOMutable { getIOMutable :: IORef a }

  newMut    = fmap IOMutable . newIORef
  getMut    = readIORef . getIOMutable
  setMut   m s = writeIORef (getIOMutable m) s
  modifyMut m f= modifyIORef (getIOMutable m) f
