{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Kernel.DB.InDb (
    InDb(..)
  , fromDb
  ) where

import           Universum

import           Control.Lens.TH (makeLenses)
import           Data.Coerce (coerce)
import qualified Data.SafeCopy as SC
import qualified Data.Set as Set
import qualified Data.Serialize as Cereal

import qualified Pos.Chain.Txp as Core
import qualified Pos.Core as Core
import qualified Pos.Core.Txp as Txp
import qualified Pos.Crypto as Core

{-------------------------------------------------------------------------------
  Wrap core types so that we can make independent serialization decisions
-------------------------------------------------------------------------------}

-- | Wrapped type (with potentially different 'SC' instance)
newtype InDb a = InDb { _fromDb :: a }
  deriving (Eq, Ord)

instance Functor InDb where
  fmap f = InDb . f . _fromDb

instance Applicative InDb where
  pure = InDb
  InDb f <*> InDb x = InDb (f x)

makeLenses ''InDb

--------------------------------------------------------------------------------

getCopyVia
  :: forall a b
  .  ( SC.SafeCopy a
     , Coercible (SC.Contained (Cereal.Get a))
                 (SC.Contained (Cereal.Get b)) )
  => Proxy a
  -> SC.Contained (Cereal.Get b)
getCopyVia _ = coerce @(SC.Contained (Cereal.Get a)) SC.getCopy

putCopyVia
  :: forall a b
   . (SC.SafeCopy a, Coercible b a)
  => Proxy a
  -> b
  -> SC.Contained Cereal.Put
putCopyVia _ = \b -> SC.putCopy (coerce b :: a)

--------------------------------------------------------------------------------
-- | This instance relays all of its work to 'InDb_Utxo_1'.
instance SC.SafeCopy (InDb Core.Utxo) where
    getCopy = getCopyVia (Proxy @InDb_Utxo_1)
    putCopy = putCopyVia (Proxy @InDb_Utxo_1)

newtype InDb_Utxo_1
  = InDb_Utxo_1 Core.Utxo
SC.deriveSafeCopy 0 'SC.base ''InDb_Utxo_1

--------------------------------------------------------------------------------
-- | This instance relays all of its work to 'InDb_NonEmpty_TxIn_TxOutAux_1'.
instance SC.SafeCopy (InDb (NonEmpty (Txp.TxIn, Core.TxOutAux))) where
    getCopy = getCopyVia (Proxy @InDb_NonEmpty_TxIn_TxOutAux_1)
    putCopy = putCopyVia (Proxy @InDb_NonEmpty_TxIn_TxOutAux_1)

newtype InDb_NonEmpty_TxIn_TxOutAux_1
  = InDb_NonEmpty_TxIn_TxOutAux_1 (NonEmpty (Txp.TxIn, Core.TxOutAux))
SC.deriveSafeCopy 0 'SC.base ''InDb_NonEmpty_TxIn_TxOutAux_1

--------------------------------------------------------------------------------
-- | This instance relays all of its work to 'InDb_Address_1'.
instance SC.SafeCopy (InDb Core.Address) where
    getCopy = getCopyVia (Proxy @InDb_Address_1)
    putCopy = putCopyVia (Proxy @InDb_Address_1)

newtype InDb_Address_1
  = InDb_Address_1 Core.Address
SC.deriveSafeCopy 0 'SC.base ''InDb_Address_1

--------------------------------------------------------------------------------

-- | This instance relays all of its work to 'InDb_AddressHash_PublicKey_1'.
instance SC.SafeCopy (InDb (Core.AddressHash Core.PublicKey)) where
    getCopy = getCopyVia (Proxy @InDb_AddressHash_PublicKey_1)
    putCopy = putCopyVia (Proxy @InDb_AddressHash_PublicKey_1)

newtype InDb_AddressHash_PublicKey_1
  = InDb_AddressHash_PublicKey_1 (Core.AddressHash Core.PublicKey)
SC.deriveSafeCopy 0 'SC.base ''InDb_AddressHash_PublicKey_1

--------------------------------------------------------------------------------
-- | This instance relays all of its work to 'InDb_Coin_1'.
instance SC.SafeCopy (InDb Core.Coin) where
    getCopy = getCopyVia (Proxy @InDb_Coin_1)
    putCopy = putCopyVia (Proxy @InDb_Coin_1)

newtype InDb_Coin_1
  = InDb_Coin_1 Core.Coin
SC.deriveSafeCopy 0 'SC.base ''InDb_Coin_1

--------------------------------------------------------------------------------
-- | This instance relays all of its work to 'InDb_SlotId_1'.
instance SC.SafeCopy (InDb Core.SlotId) where
    getCopy = getCopyVia (Proxy @InDb_SlotId_1)
    putCopy = putCopyVia (Proxy @InDb_SlotId_1)

newtype InDb_SlotId_1
  = InDb_SlotId_1 Core.SlotId
SC.deriveSafeCopy 0 'SC.base ''InDb_SlotId_1

--------------------------------------------------------------------------------
-- | This instance relays all of its work to 'InDb_Timestamp_1'.
instance SC.SafeCopy (InDb Core.Timestamp) where
    getCopy = getCopyVia (Proxy @InDb_Timestamp_1)
    putCopy = putCopyVia (Proxy @InDb_Timestamp_1)

newtype InDb_Timestamp_1
  = InDb_Timestamp_1 Core.Timestamp
SC.deriveSafeCopy 0 'SC.base ''InDb_Timestamp_1

-- TODO this instance shouldn't be here.
SC.deriveSafeCopy 0 'SC.base ''Core.Timestamp

--------------------------------------------------------------------------------
-- | This instance relays all of its work to 'InDb_TxAux_1'.
instance SC.SafeCopy (InDb Txp.TxAux) where
    getCopy = getCopyVia (Proxy @InDb_TxAux_1)
    putCopy = putCopyVia (Proxy @InDb_TxAux_1)

newtype InDb_TxAux_1
  = InDb_TxAux_1 Txp.TxAux
SC.deriveSafeCopy 0 'SC.base ''InDb_TxAux_1

-- TODO this instance shouldn't be here.
SC.deriveSafeCopy 0 'SC.base ''Txp.TxAux

--------------------------------------------------------------------------------
-- | This instance relays all of its work to 'InDb_Map_TxId_TxAux_1'.
instance SC.SafeCopy (InDb (Map Txp.TxId Txp.TxAux)) where
    getCopy = getCopyVia (Proxy @InDb_Map_TxId_TxAux_1)
    putCopy = putCopyVia (Proxy @InDb_Map_TxId_TxAux_1)

newtype InDb_Map_TxId_TxAux_1
  = InDb_Map_TxId_TxAux_1 (Map Txp.TxId Txp.TxAux)
SC.deriveSafeCopy 0 'SC.base ''InDb_Map_TxId_TxAux_1

--------------------------------------------------------------------------------
-- | This instance relays all of its work to 'InDb_TxId_1'.
instance SC.SafeCopy (InDb Txp.TxId) where
    getCopy = getCopyVia (Proxy @InDb_TxId_1)
    putCopy = putCopyVia (Proxy @InDb_TxId_1)

newtype InDb_TxId_1
  = InDb_TxId_1 Txp.TxId
SC.deriveSafeCopy 0 'SC.base ''InDb_TxId_1

-- | This instance relays all of its work to 'InDb_TxIn_1'.
instance SC.SafeCopy (InDb Txp.TxIn) where
    getCopy = getCopyVia (Proxy @InDb_TxIn_1)
    putCopy = putCopyVia (Proxy @InDb_TxIn_1)

newtype InDb_TxIn_1
  = InDb_TxIn_1 Txp.TxIn
SC.deriveSafeCopy 0 'SC.base ''InDb_TxIn_1

-- | This instance relays all of its work to 'InDb_Set_1'.
instance forall a. (SC.SafeCopy (InDb a), Ord a) => SC.SafeCopy (InDb (Set a)) where
    getCopy = getCopyVia (Proxy @(InDb_Set_1 a))
    putCopy = putCopyVia (Proxy @(InDb_Set_1 a))

newtype InDb_Set_1 a
  = InDb_Set_1 (Set a)

instance (SC.SafeCopy (InDb a), Ord a) => SC.SafeCopy (InDb_Set_1 a) where
    getCopy = SC.contain (fmap (InDb_Set_1 . Set.fromList . map _fromDb) SC.safeGet)
    putCopy (InDb_Set_1 x) = SC.contain (SC.safePut (map InDb (Set.toList x)))

--------------------------------------------------------------------------------
-- | This instance relays all of its work to 'InDb_Map_TxId_SlotId_1'.
instance SC.SafeCopy (InDb (Map Txp.TxId Core.SlotId)) where
    getCopy = getCopyVia (Proxy @InDb_Map_TxId_SlotId_1)
    putCopy = putCopyVia (Proxy @InDb_Map_TxId_SlotId_1)

newtype InDb_Map_TxId_SlotId_1
  = InDb_Map_TxId_SlotId_1 (Map Txp.TxId Core.SlotId)
SC.deriveSafeCopy 0 'SC.base ''InDb_Map_TxId_SlotId_1

