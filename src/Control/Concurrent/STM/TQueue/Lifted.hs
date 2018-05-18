{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Lifted 'TQueue' operations.
module Control.Concurrent.STM.TQueue.Lifted
  ( TQueue
  , newTQueue
  , newTQueueIO
  , readTQueue
  , tryReadTQueue
  , peekTQueue
  , tryPeekTQueue
  , flushTQueue
  , writeTQueue
  , unGetTQueue
  , isEmptyTQueue
  ) where

import Control.Monad.Base (MonadBase(..))
import Control.Monad.STM (STM)
import Control.Concurrent.STM.TQueue (TQueue)
import qualified Control.Concurrent.STM.TQueue as STM

-- | A lifted version of 'STM.newTQueue'.
newTQueue ∷ MonadBase STM μ ⇒ μ (TQueue α)
newTQueue = liftBase STM.newTQueue
{-# INLINE newTQueue #-}

-- | A lifted version of 'STM.newTQueueIO'.
newTQueueIO ∷ MonadBase IO μ ⇒ μ (TQueue α)
newTQueueIO = liftBase STM.newTQueueIO
{-# INLINE newTQueueIO #-}

-- | A lifted version of 'STM.readTQueue'.
readTQueue ∷ MonadBase STM μ ⇒ TQueue α → μ α
readTQueue = liftBase . STM.readTQueue
{-# INLINE readTQueue #-}

-- | A lifted version of 'STM.tryReadTQueue'.
tryReadTQueue ∷ MonadBase STM μ ⇒ TQueue α → μ (Maybe α)
tryReadTQueue = liftBase . STM.tryReadTQueue
{-# INLINE tryReadTQueue #-}

-- | A lifted version of 'STM.peekTQueue'.
peekTQueue ∷ MonadBase STM μ ⇒ TQueue α → μ α
peekTQueue = liftBase . STM.peekTQueue
{-# INLINE peekTQueue #-}

-- | A lifted version of 'STM.tryPeekTQueue'.
tryPeekTQueue ∷ MonadBase STM μ ⇒ TQueue α → μ (Maybe α)
tryPeekTQueue = liftBase . STM.tryPeekTQueue
{-# INLINE tryPeekTQueue #-}

-- | A lifted version of 'STM.flushTQueue'.
flushTQueue ∷ MonadBase STM μ ⇒ TQueue α → μ [α]
flushTQueue = liftBase . STM.flushTQueue
{-# INLINE flushTQueue #-}

-- | A lifted version of 'STM.writeTQueue'.
writeTQueue ∷ MonadBase STM μ ⇒ TQueue α → α → μ ()
writeTQueue = (liftBase .) . STM.writeTQueue
{-# INLINE writeTQueue #-}

-- | A lifted version of 'STM.unGetTQueue'.
unGetTQueue ∷ MonadBase STM μ ⇒ TQueue α → α → μ ()
unGetTQueue = (liftBase .) . STM.unGetTQueue
{-# INLINE unGetTQueue #-}

-- | A lifted version of 'STM.isEmptyTQueue'.
isEmptyTQueue ∷ MonadBase STM μ ⇒ TQueue α → μ Bool
isEmptyTQueue = liftBase . STM.isEmptyTQueue
{-# INLINE isEmptyTQueue #-}
