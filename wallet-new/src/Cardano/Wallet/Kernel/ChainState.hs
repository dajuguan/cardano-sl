module Cardano.Wallet.Kernel.ChainState (
    -- * Updates
    ChainStateModifier(..)
  , fromCPS
    -- * State
  , ChainState(..)
  , applyChainStateModifier
  , getCurrentChainState
    -- * Restoration
  , ChainStateRestoration(..)
  , getChainStateRestoration
  ) where

import           Universum

import qualified Data.Map.Strict as Map

import           Pos.Chain.Update (BlockVersionData (..),
                     ConfirmedProposalState (..), HasUpdateConfiguration,
                     genesisBlockVersion, genesisSoftwareVersions, ourAppName)
import           Pos.Core (HasConfiguration, ScriptVersion)
import           Pos.Core.Block (HeaderHash)
import           Pos.Core.Configuration (genesisBlockVersionData)
import           Pos.Core.Update (ApplicationName, BlockVersion,
                     BlockVersionModifier (..), NumSoftwareVersion,
                     SoftwareVersion (..), UpdateProposal (..))
import           Pos.DB.Update (getAdoptedBVFull, getConfirmedProposals,
                     getConfirmedSV)

import           Formatting (bprint, build, (%), shown)
import qualified Formatting.Buildable
import           Serokell.Data.Memory.Units (Byte)
import           Serokell.Util (mapJson)

import           Cardano.Wallet.Kernel.MonadDBReadAdaptor (LockContext,
                     MonadDBReadAdaptor, Priority (..), withMonadDBRead)

{-------------------------------------------------------------------------------
  Updates
-------------------------------------------------------------------------------}

-- | This is a summary of a 'ConfirmedProposalState'
data ChainStateModifier = ChainStateModifier {
      csmBlockVersion    :: !BlockVersion
    , csmSoftwareVersion :: !SoftwareVersion
    , csmScriptVersion   :: !(Maybe ScriptVersion)
    , csmMaxTxSize       :: !(Maybe Byte)
    }

-- | The header of the block the proposal got confirmed in and the corresponding
-- 'ChainStateModifier'
fromCPS :: ConfirmedProposalState -> (HeaderHash, ChainStateModifier)
fromCPS ConfirmedProposalState{..} = (cpsConfirmed, ChainStateModifier {
      csmBlockVersion    = upBlockVersion
    , csmSoftwareVersion = upSoftwareVersion
    , csmScriptVersion   = bvmScriptVersion
    , csmMaxTxSize       = bvmMaxTxSize
    })
  where
    UnsafeUpdateProposal{..} = cpsUpdateProposal
    BlockVersionModifier{..} = upBlockVersionMod

{-------------------------------------------------------------------------------
  State
-------------------------------------------------------------------------------}

data ChainState = ChainState {
      csBlockVersion    :: !BlockVersion
    , csSoftwareVersion :: !SoftwareVersion
    , csScriptVersion   :: !ScriptVersion
    , csMaxTxSize       :: !Byte
    }

applyChainStateModifier :: ChainStateModifier -> ChainState -> ChainState
applyChainStateModifier ChainStateModifier{..} ChainState{..} = ChainState{
      csBlockVersion    =                           csmBlockVersion
    , csSoftwareVersion =                           csmSoftwareVersion
    , csScriptVersion   = fromMaybe csScriptVersion csmScriptVersion
    , csMaxTxSize       = fromMaybe csMaxTxSize     csmMaxTxSize
    }

getCurrentChainState :: HasCallStack
                     => MonadDBReadAdaptor IO
                     -> LockContext
                     -> IO (HeaderHash, ChainState)
getCurrentChainState rocksDB lc = withMonadDBRead rocksDB $ \withLock -> do
    (tip, (bv, bvd), mSV) <- withLock lc LowPriority $ \tip ->
            (tip,,)
        <$> getAdoptedBVFull
        <*> getConfirmedSV ourAppName
    sv <- case mSV of
            Nothing -> throwM $ MissingSoftwareVersion callStack ourAppName
            Just sv -> return sv
    return (tip, ChainState {
          csBlockVersion    = bv
        , csScriptVersion   = bvdScriptVersion bvd
        , csMaxTxSize       = bvdMaxTxSize     bvd
        , csSoftwareVersion = SoftwareVersion ourAppName sv
        })

{-------------------------------------------------------------------------------
  Restoration
-------------------------------------------------------------------------------}

data ChainStateRestoration = ChainStateRestoration {
      -- | Initial chain state
      csrGenesis :: !ChainState

      -- | Current tip
    , csrTip     :: !HeaderHash

      -- | Current chain state (consistent with 'csrTip')
    , csrCurrent :: !ChainState

      -- | All updates, indexed by the block in which they were confirmed
    , csrUpdates :: !(Map HeaderHash ChainStateModifier)
    }

-- | Get all information needed for restoration
getChainStateRestoration :: HasCallStack
                         => MonadDBReadAdaptor IO
                         -> LockContext
                         -> IO ChainStateRestoration
getChainStateRestoration rocksDB lc = do
    (tip, current) <- getCurrentChainState rocksDB lc
    withMonadDBRead rocksDB $ \_lock -> do
      proposals <- getConfirmedProposals allVersions
      sv <- case genesisSoftwareVersion of
              Just sv -> return sv
              Nothing -> throwM $ MissingSoftwareVersion callStack ourAppName
      return ChainStateRestoration{
            csrGenesis = initChainState sv
          , csrTip     = tip
          , csrCurrent = current
          , csrUpdates = Map.fromList $ map fromCPS proposals
          }
  where
    -- We want all updates across all versions
    allVersions :: Maybe NumSoftwareVersion
    allVersions = Nothing

    -- Initial chain state (at the start of the blockchain)
    --
    -- At the moment this gets determined by the configuration.yaml file
    initChainState :: HasConfiguration => SoftwareVersion -> ChainState
    initChainState sv = ChainState{
          csBlockVersion    = genesisBlockVersion
        , csSoftwareVersion = sv
        , csScriptVersion   = bvdScriptVersion
        , csMaxTxSize       = bvdMaxTxSize
        }
      where
        BlockVersionData{..} = genesisBlockVersionData

    genesisSoftwareVersion :: HasUpdateConfiguration => Maybe SoftwareVersion
    genesisSoftwareVersion = listToMaybe $ filter isOurs genesisSoftwareVersions

    isOurs :: HasUpdateConfiguration => SoftwareVersion -> Bool
    isOurs sv = svAppName sv == ourAppName

{-------------------------------------------------------------------------------
  Custom exceptions
-------------------------------------------------------------------------------}

data MissingSoftwareVersion = MissingSoftwareVersion CallStack ApplicationName
  deriving Show

instance Exception MissingSoftwareVersion

{-------------------------------------------------------------------------------
  Pretty printing
-------------------------------------------------------------------------------}

instance Buildable ChainState where
  build ChainState{..} = bprint
    ( "ChainState "
    % "{ blockVersion:    " % build
    % ", softwareVersion: " % build
    % ", scriptVersion:   " % build
    % ", maxTxSize:       " % shown
    % "}"
    )
    csBlockVersion
    csSoftwareVersion
    csScriptVersion
    csMaxTxSize

instance Buildable ChainStateModifier where
  build ChainStateModifier{..} = bprint
    ( "ChainStateModifier "
    % "{ blockVersion:    " % build
    % ", softwareVersion: " % build
    % ", scriptVersion:   " % build
    % ", maxTxSize:       " % shown
    % "}"
    )
    csmBlockVersion
    csmSoftwareVersion
    csmScriptVersion
    csmMaxTxSize

instance Buildable ChainStateRestoration where
  build ChainStateRestoration{..} = bprint
    ( "ChainStateRestoration "
    % "{ genesis: " % build
    % ", tip:     " % build
    % ", current: " % build
    % ", updates: " % mapJson
    % "}"
    )
    csrGenesis
    csrTip
    csrCurrent
    csrUpdates
