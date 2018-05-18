{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Lifted 'TArray' operations.
module Control.Concurrent.STM.TArray.Lifted
  ( TArray
  ) where

import Data.Array.Base (MArray(..))
import Control.Monad.Base (MonadBase(..))
import Control.Monad.STM (STM)
import Control.Concurrent.STM.TArray (TArray)

instance {-# OVERLAPPABLE #-} MonadBase STM μ ⇒ MArray TArray e μ where
  getBounds = liftBase . getBounds
  {-# INLINE getBounds #-}
  newArray = (liftBase .) . newArray
  {-# INLINE newArray #-}
  newArray_ = liftBase . newArray_
  {-# INLINE newArray_ #-}
  unsafeRead = (liftBase .) . unsafeRead
  {-# INLINE unsafeRead #-}
  unsafeWrite = ((liftBase .) .) . unsafeWrite
  {-# INLINE unsafeWrite #-}
  getNumElements = liftBase . getNumElements
  {-# INLINE getNumElements #-}
