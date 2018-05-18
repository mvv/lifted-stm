{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Lifted 'STM.TMVar' operations.
module Control.Concurrent.STM.TMVar.Lifted
  ( TMVar
  , newTMVar
  , newEmptyTMVar
  , newTMVarIO
  , newEmptyTMVarIO
  , takeTMVar
  , tryTakeTMVar
  , putTMVar
  , tryPutTMVar
  , readTMVar
  , tryReadTMVar
  , swapTMVar
  , isEmptyTMVar
  , mkWeakTMVar
  ) where

import Control.Monad.Base (MonadBase(..))
import Control.Monad.STM (STM)
import Control.Concurrent.STM.TMVar (TMVar)
import qualified Control.Concurrent.STM.TMVar as STM
import System.Mem.Weak (Weak)

-- | A lifted version of 'STM.newTMVar'.
newTMVar ∷ MonadBase STM μ ⇒ α → μ (TMVar α)
newTMVar = liftBase . STM.newTMVar
{-# INLINE newTMVar #-}

-- | A lifted version of 'STM.newEmptyTMVar'.
newEmptyTMVar ∷ MonadBase STM μ ⇒ μ (TMVar α)
newEmptyTMVar = liftBase STM.newEmptyTMVar
{-# INLINE newEmptyTMVar #-}

-- | A lifted version of 'STM.newTMVarIO'.
newTMVarIO ∷ MonadBase IO μ ⇒ α → μ (TMVar α)
newTMVarIO = liftBase . STM.newTMVarIO
{-# INLINE newTMVarIO #-}

-- | A lifted version of 'STM.newEmptyTMVarIO'.
newEmptyTMVarIO ∷ MonadBase IO μ ⇒ μ (TMVar α)
newEmptyTMVarIO = liftBase STM.newEmptyTMVarIO
{-# INLINE newEmptyTMVarIO #-}

-- | A lifted version of 'STM.takeTMVar'.
takeTMVar ∷ MonadBase STM μ ⇒ TMVar α → μ α
takeTMVar = liftBase . STM.takeTMVar
{-# INLINE takeTMVar #-}

-- | A lifted version of 'STM.tryTakeTMVar'.
tryTakeTMVar ∷ MonadBase STM μ ⇒ TMVar α → μ (Maybe α)
tryTakeTMVar = liftBase . STM.tryTakeTMVar
{-# INLINE tryTakeTMVar #-}

-- | A lifted version of 'STM.putTMVar'.
putTMVar ∷ MonadBase STM μ ⇒ TMVar α → α → μ ()
putTMVar = (liftBase .) . STM.putTMVar
{-# INLINE putTMVar #-}

-- | A lifted version of 'STM.tryPutTMVar'.
tryPutTMVar ∷ MonadBase STM μ ⇒ TMVar α → α → μ Bool
tryPutTMVar = (liftBase .) . STM.tryPutTMVar
{-# INLINE tryPutTMVar #-}

-- | A lifted version of 'STM.readTMVar'.
readTMVar ∷ MonadBase STM μ ⇒ TMVar α → μ α
readTMVar = liftBase . STM.readTMVar
{-# INLINE readTMVar #-}

-- | A lifted version of 'STM.tryReadTMVar'.
tryReadTMVar ∷ MonadBase STM μ ⇒ TMVar α → μ (Maybe α)
tryReadTMVar = liftBase . STM.tryReadTMVar
{-# INLINE tryReadTMVar #-}

-- | A lifted version of 'STM.swapTMVar'.
swapTMVar ∷ MonadBase STM μ ⇒ TMVar α → α → μ α
swapTMVar = (liftBase .) . STM.swapTMVar
{-# INLINE swapTMVar #-}

-- | A lifted version of 'STM.isEmptyTMVar'.
isEmptyTMVar ∷ MonadBase STM μ ⇒ TMVar α → μ Bool
isEmptyTMVar = liftBase . STM.isEmptyTMVar
{-# INLINE isEmptyTMVar #-}

-- | A lifted version of 'STM.mkWeakTMVar'.
mkWeakTMVar ∷ MonadBase IO μ ⇒ TMVar α → IO () → μ (Weak (TMVar α))
mkWeakTMVar = (liftBase .) . STM.mkWeakTMVar
{-# INLINE mkWeakTMVar #-}
