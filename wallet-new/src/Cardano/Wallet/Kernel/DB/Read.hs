module Cardano.Wallet.Kernel.DB.Read (
    -- * Read-only, pure getters
    accountUtxo
  , accountAvailableUtxo
  , accountAvailableBalance
  , accountTotalBalance
  , accountAddresses
  , accountIsTxPending
  , accountTxSlot
  , hdWallets
  , readAddressMeta
  ) where

import           Universum

import qualified Data.Map.Strict as Map
import           Formatting (build, sformat)
import           Formatting.Buildable (Buildable)

import           Pos.Chain.Txp (TxId, Utxo)
import           Pos.Core (Address, Coin, SlotId)

import           Cardano.Wallet.Kernel.DB.AcidState (DB, dbHdWallets)
import           Cardano.Wallet.Kernel.DB.BlockMeta (AddressMeta,
                     blockMetaSlotId)
import           Cardano.Wallet.Kernel.DB.HdWallet (HdAccountId, HdAddress,
                     HdWallets, UnknownHdAccount)
import           Cardano.Wallet.Kernel.DB.HdWallet.Read (HdQueryErr,
                     readAddressesByAccountId, readHdAccountCurrentCheckpoint)
import           Cardano.Wallet.Kernel.DB.InDb (fromDb)
import           Cardano.Wallet.Kernel.DB.Spec (Checkpoint,
                     checkpointAddressMeta, checkpointBlockMeta,
                     checkpointPending, pendingTransactions)
import qualified Cardano.Wallet.Kernel.DB.Spec.Read as Spec
import           Cardano.Wallet.Kernel.DB.Util.IxSet (IxSet)

{-------------------------------------------------------------------------------
                              Wallet getters

  The @only@ effectful function we expose is 'getWalletSnapshot', which reads
  the full DB 'Snapshot' and returns it.
  All the other getters are completely pure and take the 'Snapshot' as input,
  so that users of the wallet are forced to re-use the same 'Snapshot' in case
  they want to read the state of the wallet multiple times within the same
  code block / handler.

-------------------------------------------------------------------------------}

walletQuery' :: forall e a. (Buildable e)
             => DB
             -> HdQueryErr e a
             -> a
walletQuery' snapshot qry= do
    let res = qry (snapshot ^. dbHdWallets)
    either err identity res
    where
        err = error . sformat build

{-------------------------------------------------------------------------------
  Pure getters on the 'DbSnapshot'.
-------------------------------------------------------------------------------}

-- | Returns the Utxo for the input 'HdAccountId'.
accountUtxo :: DB -> HdAccountId -> Utxo
accountUtxo snapshot accountId
    = walletQuery' snapshot (Spec.queryAccountUtxo accountId)

-- | Returns the available Utxo for the input 'HdAccountId'.
accountAvailableUtxo :: DB -> HdAccountId -> Utxo
accountAvailableUtxo snapshot accountId
    = walletQuery' snapshot (Spec.queryAccountAvailableUtxo accountId)

-- | Returns the available balance for the input 'HdAccountId'.
accountAvailableBalance :: DB -> HdAccountId -> Coin
accountAvailableBalance snapshot accountId
    = walletQuery' snapshot (Spec.queryAccountAvailableBalance accountId)

-- | Returns the total balance for this 'HdAccountId'.
accountTotalBalance :: DB -> HdAccountId -> Coin
accountTotalBalance snapshot accountId
    = walletQuery' snapshot (Spec.queryAccountTotalBalance accountId)

-- | Returns the total balance for this 'HdAccountId'.
accountAddresses :: DB -> HdAccountId -> IxSet HdAddress
accountAddresses snapshot accountId
    = walletQuery' snapshot (readAddressesByAccountId accountId)

-- | Returns the total balance for this 'HdAccountId'.
hdWallets :: DB -> HdWallets
hdWallets snapshot = snapshot ^. dbHdWallets

-- | Reads the given 'Address' in the 'HdAccount' current checkpoint.
readAddressMeta :: DB -> HdAccountId -> Address -> AddressMeta
readAddressMeta snapshot accountId cardanoAddress
    = view (checkpointAddressMeta cardanoAddress) (walletQuery' snapshot checkpoint)
    where
        checkpoint = readHdAccountCurrentCheckpoint accountId
accountTxSlot :: DB -> HdAccountId -> TxId -> Maybe SlotId
accountTxSlot snapshot accountId txId
    = walletQuery' snapshot (queryTxSlotId txId accountId)

accountIsTxPending :: DB -> HdAccountId -> TxId -> Bool
accountIsTxPending snapshot accountId txId
    = walletQuery' snapshot (queryTxIsPending txId accountId)

{-------------------------------------------------------------------------------
  Pure functions that support read-only operations on an account Checkpoint.
-------------------------------------------------------------------------------}
txSlot :: TxId -> Checkpoint -> (Maybe SlotId)
txSlot txId checkpoint = Map.lookup txId slots
  where
    slots = view (checkpointBlockMeta . blockMetaSlotId . fromDb) checkpoint

isTxPending :: TxId -> Checkpoint -> Bool
isTxPending txId checkpoint = Map.member txId pendingTxs
  where
    pendingTxs = view (checkpointPending . pendingTransactions . fromDb) checkpoint

{-------------------------------------------------------------------------------
  Public queries on an account.
-------------------------------------------------------------------------------}
queryTxSlotId :: TxId -> HdAccountId -> HdQueryErr UnknownHdAccount (Maybe SlotId)
queryTxSlotId txId accountId db
    = txSlot txId <$> checkpoint
    where
        checkpoint = readHdAccountCurrentCheckpoint accountId db

queryTxIsPending :: TxId -> HdAccountId -> HdQueryErr UnknownHdAccount Bool
queryTxIsPending txId accountId db
    = isTxPending txId <$> checkpoint
    where
        checkpoint = readHdAccountCurrentCheckpoint accountId db

