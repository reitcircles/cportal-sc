{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module OffChain where

import           Control.Monad       hiding (fmap)
import qualified Data.Map            as Map
import           Data.Text           (Text)
import           Plutus.Contract
import           Ledger.Constraints  as Constraints
import           Prelude             (Semigroup(..), String)
import           Text.Printf         (printf)
--import           Data.Aeson          (ToJSON, FromJSON)
--import           GHC.Generics        (Generic)
import qualified PlutusTx
import           PlutusTx.Prelude    as TxPrelude hiding (Semigroup(..), unless)
--import qualified Ledger.Typed.Scripts      as Scripts
import           Ledger.Ada          as Ada
import           Ledger              hiding (singleton)
--import           Ledger.Value
import qualified Plutus.Script.Utils.V1.Scripts  as V1Scripts
import qualified Plutus.V1.Ledger.Address  as V1Address

import ToySC


toyValHash :: V1Scripts.ValidatorHash
toyValHash = V1Scripts.validatorHash toyValidator

toyValAddress :: V1Address.Address
toyValAddress = V1Address.scriptHashAddress toyValHash

type GiftSchema =
            Endpoint "give" Integer
        .\/ Endpoint "grab" Integer

give :: AsContractError e => Integer -> Contract w GiftSchema e ()
give amount = do
  let lookups = typedValidatorLookups (typedToyValidator)
      tx      = mustPayToTheScriptWithDatumHash () $ Ada.lovelaceValueOf amount
  ledgerTx <- submitTxConstraintsWith @TypedToy lookups tx
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String $ printf "locked %d lovelace at script" amount

grab :: AsContractError e => Integer -> Contract w GiftSchema e ()
grab n = do
    utxos <- utxosAt toyValAddress
    let orefs   = Map.keys utxos
        red     = ToyRedeemer { key = n }
        red'    = Redeemer $ PlutusTx.toBuiltinData red
        lookups = Constraints.unspentOutputs utxos             <>
                  Constraints.otherData unitDatum              <>
                  Constraints.plutusV1OtherScript toyValidator
        tx :: TxConstraints ToyRedeemer ()
        tx      = mconcat [mustSpendScriptOutput oref red' | oref <- orefs]
    ledgerTx <- submitTxConstraintsWith @TypedToy lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ "collected gift"

endpoints :: Contract () GiftSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" give
    grab' = endpoint @"grab" grab
