{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
--{-# LANGUAGE TemplateHaskell       #-}
--{-# LANGUAGE TypeApplications      #-}
--{-# LANGUAGE TypeFamilies          #-}
--{-# LANGUAGE TypeOperators         #-}

module Instances where

--import           Data.Aeson          (ToJSON, FromJSON)
--import           GHC.Generics        (Generic)
--import qualified PlutusTx
import           PlutusTx.Prelude       as TxPrelude hiding (unless)
-- import           PlutusTx.Builtins.Class
import qualified Plutus.Script.Utils.V2.Scripts  as Scripts
--import           Plutus.Script.Utils.Typed (mkUntypedValidator, mkUntypedMintingPolicy)
-- import           Ledger.Ada             as Ada
import qualified Ledger                 as L
import           Ledger.Value
import           Plutus.V2.Ledger.Api
--import           Plutus.V2.Ledger.Contexts as V2LC
--import           Plutus.Script.Utils.V2.Typed.Scripts.Validators as V2V

import Config
import Registry
import Minting


-- Concrete instances of Registry script and minting policy.

-- | Administrator's public key-hash.  Obtained with:
-- cardano-cli address key-hash --payment-verification-key-file wallet1.vkey
pkhA :: L.PaymentPubKeyHash
pkhA = L.PaymentPubKeyHash . PubKeyHash . hexConvert $ "0e454ff759f367e7fd5b2d0ed30b46b95cf56396af700a3f9971d7a2"

-- | Registry's validator.  To be serialized into file 'registry.plutus' and its address
-- written into 'registry.addr'
registryA :: Scripts.Validator
registryA = registry pkhA

-- | Registry's validator hash.  Obtained with:
-- cardano-cli transaction policyid --script-file registry.plutus
registryHashA :: ValidatorHash
registryHashA = ValidatorHash $ hexConvert "dd2725f7c2919d01c7e80b3f3f7fdb374ad5854f9269b1d90caf9d82"

-- | UTxO's reference that parametrizes NFT (to be consumed at minting)
oref1_tmp :: TxOutRef
oref1_tmp = TxOutRef
  { txOutRefId = TxId $ hexConvert "4a16c4f23a1855392496650deac62eb6c1fa1887f7a2e59ee2a3a3c7b67fa733"
  , txOutRefIdx = 1
  }

params_1 :: MintingParams
params_1 = MintingParams
  { mpUtxo = oref1_tmp
  , mpPKH  = pkhA
  , mpValHash = registryHashA
  , mpRefName = referenceTokenName
  , mpUsrName = userTokenName
  }

policy_1 :: MintingPolicy
policy_1 = policy params_1

curSymbol_1 :: CurrencySymbol
curSymbol_1 = curSymbol params_1

-- Datum example.

datum_1 :: RegDatum
datum_1 = RegDatum
  { rdURL  = "ipfs://QmVUSyxrvSEwATqGNefKYVtx5ixJrBnD9XjMRJx9JK9Nuh"
  , rdHash = "b5d4045c3f466fa91fe2cc6abe79232a1a57cdf104f7a26e716e0a1e2789df78"
  }


