-- | VSS related functions.

module Pos.Core.Ssc.Vss
       (
         -- * Types
         VssCertificate (..)
       , VssCertificatesMap (..)
       -- * Certificates
       , mkVssCertificate
       , checkVssCertificate
       , checkCertSign
       , getCertId

       -- * Certificate maps
       -- ** Creating maps
       , checkVssCertificatesMap
       , mkVssCertificatesMap
       , mkVssCertificatesMapLossy
       , mkVssCertificatesMapSingleton
       -- ** Working with maps
       , validateVssCertificatesMap
       , memberVss
       , lookupVss
       , insertVss
       , deleteVss
       ) where

import           Universum

import           Control.Monad.Except (MonadError (throwError))
import qualified Data.HashMap.Strict as HM
import           Serokell.Util (allDistinct)

import           Pos.Binary.Class (AsBinary (..), Bi)
import           Pos.Core.Common (StakeholderId)
import           Pos.Core.Slotting.Types (EpochIndex)
import           Pos.Core.Ssc.Types (VssCertificate (..), VssCertificatesMap (..), getCertId,
                                     mkVssCertificatesMap, mkVssCertificatesMapLossy,
                                     mkVssCertificatesMapSingleton, validateVssCertificatesMap)
import           Pos.Crypto (ProtocolMagic, SecretKey, SignTag (SignVssCert), VssPublicKey,
                             checkSig, sign, toPublic)

-- | Make VssCertificate valid up to given epoch using 'SecretKey' to sign
-- data.
mkVssCertificate
    :: (Bi EpochIndex)
    => ProtocolMagic
    -> SecretKey
    -> AsBinary VssPublicKey
    -> EpochIndex
    -> VssCertificate
mkVssCertificate pm sk vk expiry =
    UnsafeVssCertificate vk expiry signature (toPublic sk)
  where
    signature = sign pm SignVssCert sk (vk, expiry)

-- | Check a 'VssCertificate' for validity.
checkVssCertificate
    :: (Bi EpochIndex, MonadError Text m)
    => ProtocolMagic
    -> VssCertificate
    -> m ()
checkVssCertificate pm it =
    unless (checkCertSign pm it) $ throwError "checkVssCertificate: invalid sign"

-- CHECK: @checkCertSign
-- | Check that the VSS certificate is signed properly
-- #checkPubKeyAddress
-- #checkSig
checkCertSign :: (Bi EpochIndex) => ProtocolMagic -> VssCertificate -> Bool
checkCertSign pm UnsafeVssCertificate {..} =
    checkSig pm SignVssCert vcSigningKey (vcVssKey, vcExpiryEpoch) vcSignature

-- | Guard against certificates with duplicate signing keys or with duplicate
-- 'vcVssKey's. Also checks every VssCertificate in the map (see
-- 'checkVssCertificate').
checkVssCertificatesMap
    :: (Bi EpochIndex, MonadError Text m)
    => ProtocolMagic
    -> VssCertificatesMap
    -> m ()
checkVssCertificatesMap pm vssCertsMap = do
    forM_ certs (checkVssCertificate pm)
    unless (allDistinct (map vcSigningKey certs))
        (throwError "VssCertificatesMap: two certs have the same signing key")
    unless (allDistinct (map vcVssKey certs))
        (throwError "VssCertificatesMap: two certs have the same VSS key")
  where
    certs = HM.elems (getVssCertificatesMap vssCertsMap)

memberVss :: StakeholderId -> VssCertificatesMap -> Bool
memberVss id (UnsafeVssCertificatesMap m) = HM.member id m

lookupVss :: StakeholderId -> VssCertificatesMap -> Maybe VssCertificate
lookupVss id (UnsafeVssCertificatesMap m) = HM.lookup id m

-- | Insert a certificate into the map.
--
-- In order to preserve invariants, this function removes certificates with
-- our certificate's signing key / VSS key, if they exist. It also returns a
-- list of deleted certificates' keys.
insertVss :: VssCertificate
          -> VssCertificatesMap
          -> (VssCertificatesMap, [StakeholderId])
insertVss c (UnsafeVssCertificatesMap m) =
    ( UnsafeVssCertificatesMap $
      HM.insert (getCertId c) c $
      HM.filter (not . willBeDeleted) m
    , deleted
    )
  where
    willBeDeleted c2 = vcVssKey     c2 == vcVssKey     c
                    || vcSigningKey c2 == vcSigningKey c
    deleted = HM.keys $ HM.filter willBeDeleted m

deleteVss :: StakeholderId -> VssCertificatesMap -> VssCertificatesMap
deleteVss id (UnsafeVssCertificatesMap m) = UnsafeVssCertificatesMap (HM.delete id m)