{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Lifted 'STM' operations.
module Control.Monad.STM.Lifted
  ( STM
  , MonadSTM(..)
  , atomically
  , retry
  , check
  , throwSTM
  , catchSTM
  ) where

import Data.Monoid (Monoid)
import Control.Monad.Base
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Accum
import qualified Control.Monad.Trans.State.Lazy as L
import qualified Control.Monad.Trans.State.Strict as S
import qualified Control.Monad.Trans.Writer.Lazy as L
import qualified Control.Monad.Trans.Writer.Strict as S
import qualified Control.Monad.Trans.RWS.Lazy as L
import qualified Control.Monad.Trans.RWS.Strict as S
import Control.Monad.Trans.Finish
import Control.Monad.Exception
import Control.Monad.STM (STM)
import qualified Control.Monad.STM as STM

-- | Class of monads that have 'STM' as their base and support composing
-- alternative (vs retries) actions.
class (MonadBase STM μ, MonadCatch μ) ⇒ MonadSTM μ where
  -- | Equivalent to the first action, unless it retries. It which case the second
  -- action is performed in its place.
  orElse ∷ μ α → μ α → μ α

instance MonadSTM STM where
  orElse = STM.orElse
  {-# INLINE orElse #-}

instance MonadSTM μ ⇒ MonadSTM (IdentityT μ) where
  orElse m₁ m₂ = IdentityT $ orElse (runIdentityT m₁) (runIdentityT m₂)
  {-# INLINE orElse #-}

instance MonadSTM μ ⇒ MonadSTM (MaybeT μ) where
  orElse m₁ m₂ = MaybeT $ orElse (runMaybeT m₁) (runMaybeT m₂)
  {-# INLINE orElse #-}

instance MonadSTM μ ⇒ MonadSTM (ReaderT w μ) where
  orElse m₁ m₂ = ReaderT $ \r → orElse (runReaderT m₁ r) (runReaderT m₂ r)
  {-# INLINE orElse #-}

instance (Monoid w, MonadSTM μ) ⇒ MonadSTM (AccumT w μ) where
  orElse m₁ m₂ = AccumT $ \w → orElse (runAccumT m₁ w) (runAccumT m₂ w)
  {-# INLINE orElse #-}

instance MonadSTM μ ⇒ MonadSTM (L.StateT s μ) where
  orElse m₁ m₂ = L.StateT $ \s → orElse (L.runStateT m₁ s) (L.runStateT m₂ s)
  {-# INLINE orElse #-}

instance MonadSTM μ ⇒ MonadSTM (S.StateT s μ) where
  orElse m₁ m₂ = S.StateT $ \s → orElse (S.runStateT m₁ s) (S.runStateT m₂ s)
  {-# INLINE orElse #-}

instance (Monoid w, MonadSTM μ) ⇒ MonadSTM (L.WriterT w μ) where
  orElse m₁ m₂ = L.WriterT $ orElse (L.runWriterT m₁) (L.runWriterT m₂)
  {-# INLINE orElse #-}

instance (Monoid w, MonadSTM μ) ⇒ MonadSTM (S.WriterT w μ) where
  orElse m₁ m₂ = S.WriterT $ orElse (S.runWriterT m₁) (S.runWriterT m₂)
  {-# INLINE orElse #-}

instance (Monoid w, MonadSTM μ) ⇒ MonadSTM (L.RWST r w s μ) where
  orElse m₁ m₂ = L.RWST $ \r s → orElse (L.runRWST m₁ r s) (L.runRWST m₂ r s)
  {-# INLINE orElse #-}

instance (Monoid w, MonadSTM μ) ⇒ MonadSTM (S.RWST r w s μ) where
  orElse m₁ m₂ = S.RWST $ \r s → orElse (S.runRWST m₁ r s) (S.runRWST m₂ r s)
  {-# INLINE orElse #-}

instance MonadSTM μ ⇒ MonadSTM (FinishT r μ) where
  orElse m₁ m₂ = FinishT $ orElse (runFinishT m₁) (runFinishT m₂)
  {-# INLINE orElse #-}

-- | A lifted version of 'STM.atomically'.
atomically ∷ MonadBase IO μ ⇒ STM α → μ α
atomically = liftBase . STM.atomically
{-# INLINE atomically #-}

-- | A lifted version of 'STM.retry'.
retry ∷ MonadBase STM μ ⇒ μ α
retry = liftBase STM.retry
{-# INLINE retry #-}

-- | A lifted version of 'STM.check'.
check ∷ MonadBase STM μ ⇒ Bool → μ ()
check = liftBase . STM.check
{-# INLINE check #-}

-- | A lifted version of 'STM.throwSTM'.
throwSTM ∷ (Exception e, MonadBase STM μ) ⇒ e → μ α
throwSTM = liftBase . STM.throwSTM
{-# INLINE throwSTM #-}

-- | A specialized version of 'catch'.
catchSTM ∷ (Exception e, MonadSTM μ) ⇒ μ α → (e → μ α) → μ α
catchSTM = catch
{-# INLINE catchSTM #-}
