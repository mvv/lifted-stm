{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Lifted 'TChan' operations.
module Control.Concurrent.STM.TChan.Lifted
  ( TChan
  , newTChan
  , newTChanIO
  , newBroadcastTChan
  , newBroadcastTChanIO
  , dupTChan
  , cloneTChan
  , readTChan
  , tryReadTChan
  , peekTChan
  , tryPeekTChan
  , writeTChan
  , unGetTChan
  , isEmptyTChan
  ) where

import Control.Monad.Base (MonadBase(..))
import Control.Monad.STM (STM)
import Control.Concurrent.STM.TChan (TChan)
import qualified Control.Concurrent.STM.TChan as STM

-- | A lifted version of 'STM.newTChan'.
newTChan ∷ MonadBase STM μ ⇒ μ (TChan α)
newTChan = liftBase STM.newTChan
{-# INLINE newTChan #-}

-- | A lifted version of 'STM.newTChanIO'.
newTChanIO ∷ MonadBase IO μ ⇒ μ (TChan α)
newTChanIO = liftBase STM.newTChanIO
{-# INLINE newTChanIO #-}

-- | A lifted version of 'STM.newBroadcastTChan'.
newBroadcastTChan ∷ MonadBase STM μ ⇒ μ (TChan α)
newBroadcastTChan = liftBase STM.newBroadcastTChan
{-# INLINE newBroadcastTChan #-}

-- | A lifted version of 'STM.newBroadcastTChanIO'.
newBroadcastTChanIO ∷ MonadBase IO μ ⇒ μ (TChan α)
newBroadcastTChanIO = liftBase STM.newBroadcastTChanIO
{-# INLINE newBroadcastTChanIO #-}

-- | A lifted version of 'STM.dupTChan'.
dupTChan ∷ MonadBase STM μ ⇒ TChan α → μ (TChan α)
dupTChan = liftBase . STM.dupTChan
{-# INLINE dupTChan #-}

-- | A lifted version of 'STM.cloneTChan'.
cloneTChan ∷ MonadBase STM μ ⇒ TChan α → μ (TChan α)
cloneTChan = liftBase . STM.cloneTChan
{-# INLINE cloneTChan #-}

-- | A lifted version of 'STM.readTChan'.
readTChan ∷ MonadBase STM μ ⇒ TChan α → μ α
readTChan = liftBase . STM.readTChan
{-# INLINE readTChan #-}

-- | A lifted version of 'STM.tryReadTChan'.
tryReadTChan ∷ MonadBase STM μ ⇒ TChan α → μ (Maybe α)
tryReadTChan = liftBase . STM.tryReadTChan
{-# INLINE tryReadTChan #-}

-- | A lifted version of 'STM.peekTChan'.
peekTChan ∷ MonadBase STM μ ⇒ TChan α → μ α
peekTChan = liftBase . STM.peekTChan
{-# INLINE peekTChan #-}

-- | A lifted version of 'STM.tryPeekTChan'.
tryPeekTChan ∷ MonadBase STM μ ⇒ TChan α → μ (Maybe α)
tryPeekTChan = liftBase . STM.tryPeekTChan
{-# INLINE tryPeekTChan #-}

-- | A lifted version of 'STM.writeTChan'.
writeTChan ∷ MonadBase STM μ ⇒ TChan α → α → μ ()
writeTChan = (liftBase .) . STM.writeTChan
{-# INLINE writeTChan #-}

-- | A lifted version of 'STM.unGetTChan'.
unGetTChan ∷ MonadBase STM μ ⇒ TChan α → α → μ ()
unGetTChan = (liftBase .) . STM.unGetTChan
{-# INLINE unGetTChan #-}

-- | A lifted version of 'STM.isEmptyTChan'.
isEmptyTChan ∷ MonadBase STM μ ⇒ TChan α → μ Bool
isEmptyTChan = liftBase . STM.isEmptyTChan
{-# INLINE isEmptyTChan #-}
