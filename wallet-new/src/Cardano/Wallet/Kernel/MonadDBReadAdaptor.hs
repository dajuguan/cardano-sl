{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}

module Cardano.Wallet.Kernel.MonadDBReadAdaptor (
    WithMonadDBRead    -- opaque
  , MonadDBReadAdaptor -- opaque
  , newMonadDBReadAdaptor
  , fromNodeResources
  , withMonadDBRead
  , rocksDBNotAvailable
  , LockContext(..)
    -- * Convenience re-exports
  , Priority(..)
  ) where

import           Universum

import           Control.Lens.TH (makeLenses)
import           Control.Monad.IO.Unlift (MonadUnliftIO, UnliftIO (UnliftIO),
                     askUnliftIO, unliftIO, withUnliftIO)
import           Control.Monad.Trans.Resource (transResourceT)
import           Data.Conduit (transPipe)

import           Pos.Chain.Update (HasUpdateConfiguration)
import           Pos.Context (NodeContext (..))
import           Pos.Core.Block (HeaderHash, headerHash)
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Core.StateLock (StateLock, withStateLockNoMetrics)
import           Pos.DB.Block (getSerializedBlock, getSerializedUndo)
import           Pos.DB.BlockIndex (getTipHeader)
import           Pos.DB.Class (MonadDBRead (..), Serialized (Serialized))
import           Pos.DB.Rocks.Functions (dbGetDefault, dbIterSourceDefault)
import           Pos.DB.Rocks.Types (NodeDBs)
import           Pos.Launcher.Resource (NodeResources (..))
import           Pos.Util (HasLens (..))
import           Pos.Util.Concurrent.PriorityLock (Priority (..))

-- | Internal state (not exported)
data InternalState = IS {
      _isNodeDBs   :: NodeDBs
    , _isStateLock :: StateLock
    }

makeLenses ''InternalState

instance HasLens NodeDBs   InternalState NodeDBs   where lensOf = isNodeDBs
instance HasLens StateLock InternalState StateLock where lensOf = isStateLock

-- | @WithMonadDBRead m@ is a monad in which we have a 'MonadDBRead' instance
-- available (as well as a whole bunch of other instances).
--
-- See 'MonadDBReadAdapter' for running 'WithMonadDBRead' actions.
newtype WithMonadDBRead m a = Wrap { unwrap :: ReaderT InternalState m a }
  deriving (
      Functor
    , Applicative
    , Monad
    , MonadCatch
    , MonadThrow
    , MonadTrans
    , MonadIO
    )

instance MonadUnliftIO m => MonadUnliftIO (WithMonadDBRead m) where
  askUnliftIO = Wrap $ withUnliftIO $ \u ->
                  pure $ UnliftIO (unliftIO u . unwrap)

instance ( HasConfiguration -- silly superclass constraint in core, cannot avoid
         , MonadThrow m
         , MonadIO    m
         , MonadCatch m
         ) => MonadDBRead (WithMonadDBRead m) where
  dbGet tag bs       = Wrap $ dbGetDefault tag bs
  dbGetSerBlock hh   = Wrap $ fmap Serialized <$> getSerializedBlock hh
  dbGetSerUndo hh    = Wrap $ fmap Serialized <$> getSerializedUndo  hh
  dbIterSource tag p = transPipe (transResourceT Wrap) $
                         dbIterSourceDefault tag p

-- | Do we need to take the lock?
--
-- DB locks are important for consistency, but of course come with provisos.
-- In particular, we should not take a lock when we already have it (e.g.,
-- in 'MonadBListener'). Code that needs a lock but is unaware of the context
-- in which it is run can pass 'LockContext' up to the caller:
--
-- > foo :: MonadDBReadAdaptor m -> LockContext -> m ..
-- > foo db lc = withMonadDBRead db $ \withLock -> do
-- >     ..
-- >     x <- withLock lc LowPriority $ \tip -> ..
-- >     ..
--
-- which would then be called as
--
-- > foo db NotYetLocked
--
-- or
--
-- > foo db AlreadyLocked
data LockContext = AlreadyLocked | NotYetLocked

-- | Take the state lock (cf. 'withStateLockNoMetrics', 'ncStateLock').
type Lock m = forall a. LockContext -> Priority -> (HeaderHash -> m a) -> m a

-- | Internal wrapper around 'withStateLockNoMetrics'
--
-- NOTE: If we wanted to use 'withStateLock' instead we would need to
-- capture additional node context.
withLock :: ( HasConfiguration
            , MonadIO   m
            , MonadMask m
            )
         => Lock (WithMonadDBRead m)
withLock AlreadyLocked _ f = headerHash <$> getTipHeader >>= f
withLock NotYetLocked  p f = Wrap $ withStateLockNoMetrics p $ unwrap . f

-- | Adaptor for running 'WithMonadDBRead' actions. See 'newMonadBDReadAdaptor'
newtype MonadDBReadAdaptor m = Adaptor {
      -- | Run an action in the 'WithMonadDBRead' monad.
      withMonadDBRead :: forall a.
                         (    (HasConfiguration, HasUpdateConfiguration)
                           => Lock (WithMonadDBRead m)
                           -> WithMonadDBRead m a
                         )
                      -> m a
    }

-- | Constructor for 'MonadDBReadAdaptor'
--
-- See also 'fromNodeResources'.
--
-- NOTE: This captures the 'HasConfiguration' and 'HasUpdateConfiguration'
-- constraints in the closure so that the adaptor can be used in a place where
-- this constraint is not available.
newMonadDBReadAdaptor :: ( HasConfiguration
                         , HasUpdateConfiguration
                         , MonadIO   m
                         , MonadMask m
                         )
                      => NodeDBs -> StateLock -> MonadDBReadAdaptor m
newMonadDBReadAdaptor ndbs lock =
    Adaptor $ \act -> runReaderT (unwrap (act withLock)) IS {
        _isNodeDBs   = ndbs
      , _isStateLock = lock
      }

-- | Convenience wrapper around 'newMonadDBReadAdaptor' that pulls its
-- arguments from 'NodeResources'
fromNodeResources :: ( HasConfiguration
                     , HasUpdateConfiguration
                     , MonadIO   m
                     , MonadMask m
                     )
                  => NodeResources ext -> MonadDBReadAdaptor m
fromNodeResources nr =
    newMonadDBReadAdaptor
      (nrDBs nr)
      (ncStateLock (nrContext nr))

-- | Drop-in replacement for the 'MonadDBReadAdaptor' for when rocks DB is
-- not available (throws an exception when used).
--
-- Used only for tests.
rocksDBNotAvailable :: MonadThrow m => MonadDBReadAdaptor m
rocksDBNotAvailable = Adaptor $ \_act -> throwM RocksDBNotAvailable

-- | Thrown when using the 'rocksDBNotAvailable' adaptor.
data RocksDBNotAvailable = RocksDBNotAvailable
  deriving (Show)

instance Exception RocksDBNotAvailable
