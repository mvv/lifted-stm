{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Lifted 'STM.TVar' operations.
module Control.Concurrent.STM.TVar.Lifted
  ( TVar
  , newTVar
  , newTVarIO
  , readTVar
  , readTVarIO
  , writeTVar
  , modifyTVar
  , modifyTVar'
  , swapTVar
  , registerDelay
  , mkWeakTVar
  ) where

import Control.Monad.Base (MonadBase(..))
import Control.Monad.STM (STM)
import Control.Concurrent.STM.TVar (TVar)
import qualified Control.Concurrent.STM.TVar as STM
import System.Mem.Weak (Weak)

-- | A lifted version of 'STM.newTVar'.
newTVar ∷ MonadBase STM μ ⇒ α → μ (TVar α)
newTVar = liftBase . STM.newTVar
{-# INLINE newTVar #-}

-- | A lifted version of 'STM.newTVarIO'.
newTVarIO ∷ MonadBase IO μ ⇒ α → μ (TVar α)
newTVarIO = liftBase . STM.newTVarIO
{-# INLINE newTVarIO #-}

-- | A lifted version of 'STM.readTVar'.
readTVar ∷ MonadBase STM μ ⇒ TVar α → μ α
readTVar = liftBase . STM.readTVar
{-# INLINE readTVar #-}

-- | A lifted version of 'STM.readTVarIO'.
readTVarIO ∷ MonadBase IO μ ⇒ TVar α → μ α
readTVarIO = liftBase . STM.readTVarIO
{-# INLINE readTVarIO #-}

-- | A lifted version of 'STM.writeTVar'.
writeTVar ∷ MonadBase STM μ ⇒ TVar α → α → μ ()
writeTVar = (liftBase .) . STM.writeTVar
{-# INLINE writeTVar #-}

-- | A lifted version of 'STM.modifyTVar'.
modifyTVar ∷ MonadBase STM μ ⇒ TVar α → (α → α) → μ ()
modifyTVar = (liftBase .) . STM.modifyTVar
{-# INLINE modifyTVar #-}

-- | A lifted version of 'STM.modifyTVar''.
modifyTVar' ∷ MonadBase STM μ ⇒ TVar α → (α → α) → μ ()
modifyTVar' = (liftBase .) . STM.modifyTVar'
{-# INLINE modifyTVar' #-}

-- | A lifted version of 'STM.swapTVar'.
swapTVar ∷ MonadBase STM μ ⇒ TVar α → α → μ α
swapTVar = (liftBase .) . STM.swapTVar
{-# INLINE swapTVar #-}

-- | A lifted version of 'STM.registerDelay'.
registerDelay ∷ MonadBase IO μ ⇒ Int → μ (TVar Bool)
registerDelay = liftBase . STM.registerDelay
{-# INLINE registerDelay #-}

-- | A lifted version of 'STM.mkWeakTVar'.
mkWeakTVar ∷ MonadBase IO μ ⇒ TVar α → IO () → μ (Weak (TVar α))
mkWeakTVar = (liftBase .) . STM.mkWeakTVar
{-# INLINE mkWeakTVar #-}
