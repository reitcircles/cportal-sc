{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Instances where

import           PlutusTx.Prelude       as TxPrelude hiding (unless)
import qualified Plutus.Script.Utils.V2.Scripts  as Scripts
import qualified Ledger                 as L
import           Ledger.Value
import           Plutus.V2.Ledger.Api

import Config
import Registry
import Minting


-- Concrete instances of Registry script and minting polic --

-- | Administrator's public key-hash.  Obtained with:
-- cardano-cli address key-hash --payment-verification-key-file wallet1.vkey
pkhA :: L.PaymentPubKeyHash
pkhA = L.PaymentPubKeyHash . PubKeyHash . hexConvert $ "0e454ff759f367e7fd5b2d0ed30b46b95cf56396af700a3f9971d7a2"

-- | Registry's validator.  To be serialized into file 'registry.plutus' and its address
-- written into 'registry.addr'
registryA :: Scripts.Validator
registryA = registry pkhA

-- | Registry's validator hash.
registryHashA :: ValidatorHash
registryHashA = validatorHash' registryA

-- | UTxO's reference that parametrizes NFT (to be consumed at minting). Also stored in
-- file 'oref1a.tmp'.
oref1_tmp :: TxOutRef
oref1_tmp = TxOutRef
  { txOutRefId = TxId $ hexConvert "4a16c4f23a1855392496650deac62eb6c1fa1887f7a2e59ee2a3a3c7b67fa733"
  , txOutRefIdx = 1
  }

-- | Parameters that determine a concrete instance of the minting policy.
params_1 :: MintingParams
params_1 = MintingParams
  { mpUtxo = oref1_tmp
  , mpPKH  = pkhA
  , mpValHash = registryHashA
  , mpRefName = referenceTokenName
  , mpUsrName = userTokenName
  }

-- | Concrete instance of a minting policy.
policy_1 :: MintingPolicy
policy_1 = policy params_1

-- | The Policy Id.
curSymbol_1 :: CurrencySymbol
curSymbol_1 = curSymbol params_1

-- Datum --

-- | A Datum example.
datum_1 :: RegDatum
datum_1 = RegDatum
  { rdURL  = "ipfs://QmVUSyxrvSEwATqGNefKYVtx5ixJrBnD9XjMRJx9JK9Nuh"
  , rdHash = "b5d4045c3f466fa91fe2cc6abe79232a1a57cdf104f7a26e716e0a1e2789df78"
  }


