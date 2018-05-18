{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Lifted 'STM.TSem' operations.
module Control.Concurrent.STM.TSem.Lifted
  ( TSem
  , newTSem
  , waitTSem
  , signalTSem
  , signalTSemN
  ) where

import Control.Monad.Base (MonadBase(..))
import Control.Monad.STM (STM)
import Control.Concurrent.STM.TSem (TSem)
import qualified Control.Concurrent.STM.TSem as STM

-- | A lifted version of 'STM.newTSem'.
newTSem ∷ MonadBase STM μ ⇒ Int → μ TSem
newTSem = liftBase . STM.newTSem
{-# INLINE newTSem #-}

-- | A lifted version of 'STM.waitTSem'.
waitTSem ∷ MonadBase STM μ ⇒ TSem → μ ()
waitTSem = liftBase . STM.waitTSem
{-# INLINE waitTSem #-}

-- | A lifted version of 'STM.signalTSem'.
signalTSem ∷ MonadBase STM μ ⇒ TSem → μ ()
signalTSem = liftBase . STM.signalTSem
{-# INLINE signalTSem #-}

-- | A lifted version of 'STM.signalTSemN'.
signalTSemN ∷ MonadBase STM μ ⇒ Word → TSem → μ ()
signalTSemN = (liftBase .) . STM.signalTSemN
{-# INLINE signalTSemN #-}
