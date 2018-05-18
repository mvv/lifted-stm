{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Lifted 'TBQueue' operations.
module Control.Concurrent.STM.TBQueue.Lifted
  ( TBQueue
  , newTBQueue
  , newTBQueueIO
  , readTBQueue
  , tryReadTBQueue
  , peekTBQueue
  , tryPeekTBQueue
  , flushTBQueue
  , writeTBQueue
  , unGetTBQueue
  , isEmptyTBQueue
  , isFullTBQueue
  ) where

import Control.Monad.Base (MonadBase(..))
import Control.Monad.STM (STM)
import Control.Concurrent.STM.TBQueue (TBQueue)
import qualified Control.Concurrent.STM.TBQueue as STM

-- | A lifted version of 'STM.newTBQueue'.
newTBQueue ∷ MonadBase STM μ ⇒ Int → μ (TBQueue α)
newTBQueue = liftBase . STM.newTBQueue
{-# INLINE newTBQueue #-}

-- | A lifted version of 'STM.newTBQueueIO'.
newTBQueueIO ∷ MonadBase IO μ ⇒ Int → μ (TBQueue α)
newTBQueueIO = liftBase . STM.newTBQueueIO
{-# INLINE newTBQueueIO #-}

-- | A lifted version of 'STM.readTBQueue'.
readTBQueue ∷ MonadBase STM μ ⇒ TBQueue α → μ α
readTBQueue = liftBase . STM.readTBQueue
{-# INLINE readTBQueue #-}

-- | A lifted version of 'STM.tryReadTBQueue'.
tryReadTBQueue ∷ MonadBase STM μ ⇒ TBQueue α → μ (Maybe α)
tryReadTBQueue = liftBase . STM.tryReadTBQueue
{-# INLINE tryReadTBQueue #-}

-- | A lifted version of 'STM.peekTBQueue'.
peekTBQueue ∷ MonadBase STM μ ⇒ TBQueue α → μ α
peekTBQueue = liftBase . STM.peekTBQueue
{-# INLINE peekTBQueue #-}

-- | A lifted version of 'STM.tryPeekTBQueue'.
tryPeekTBQueue ∷ MonadBase STM μ ⇒ TBQueue α → μ (Maybe α)
tryPeekTBQueue = liftBase . STM.tryPeekTBQueue
{-# INLINE tryPeekTBQueue #-}

-- | A lifted version of 'STM.flushTBQueue'.
flushTBQueue ∷ MonadBase STM μ ⇒ TBQueue α → μ [α]
flushTBQueue = liftBase . STM.flushTBQueue
{-# INLINE flushTBQueue #-}

-- | A lifted version of 'STM.writeTBQueue'.
writeTBQueue ∷ MonadBase STM μ ⇒ TBQueue α → α → μ ()
writeTBQueue = (liftBase .) . STM.writeTBQueue
{-# INLINE writeTBQueue #-}

-- | A lifted version of 'STM.unGetTBQueue'.
unGetTBQueue ∷ MonadBase STM μ ⇒ TBQueue α → α → μ ()
unGetTBQueue = (liftBase .) . STM.unGetTBQueue
{-# INLINE unGetTBQueue #-}

-- | A lifted version of 'STM.isEmptyTBQueue'.
isEmptyTBQueue ∷ MonadBase STM μ ⇒ TBQueue α → μ Bool
isEmptyTBQueue = liftBase . STM.isEmptyTBQueue
{-# INLINE isEmptyTBQueue #-}

-- | A lifted version of 'STM.isFullTBQueue'.
isFullTBQueue ∷ MonadBase STM μ ⇒ TBQueue α → μ Bool
isFullTBQueue = liftBase . STM.isFullTBQueue
{-# INLINE isFullTBQueue #-}
