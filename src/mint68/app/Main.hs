{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Prelude
import qualified Data.ByteString.Lazy as B
import           Data.Aeson
import           Plutus.V2.Ledger.Api
import qualified Ledger               as L

import Config
import Registry
import Minting
import Deploy


data PolicyParams = PolicyParams { utxoRefId :: String, utxoRefIdx :: Integer, pkh :: String }
  deriving Show

instance FromJSON PolicyParams where
  parseJSON (Object v) = PolicyParams <$> v .: "utxoRefId" <*> v .: "utxoRefIdx" <*> v .: "pkh"
  parseJSON _          = fail "Expected object with policy parameters"

main :: IO ()
main = do
  -- Read JSON file with policy parameters into a bytestring
  input <- B.readFile "ledger/policy-params.json"

  -- Parse JSON bytestring into corresponding Haskell value
  let result = eitherDecode input :: Either String PolicyParams
  case result of
    Left err -> putStrLn err
    Right pparams -> do
      -- Administrator's pub-key-hash
      let pkhA = L.PaymentPubKeyHash . PubKeyHash . hexConvert . pkh $ pparams

      -- Registry's validator
      let registryA = registry pkhA

      -- Parameters determining a concrete instance of the minting policy
      let params_1 = MintingParams
            { mpUtxo = TxOutRef
                { txOutRefId = TxId . hexConvert . utxoRefId $ pparams
                , txOutRefIdx = utxoRefIdx pparams
                }
            , mpPKH = pkhA
            , mpValHash = validatorHash' registryA
            , mpRefName = referenceTokenName
            , mpUsrName = userTokenName
            }

      -- Instance of minting policy
      let policy_1 = policy params_1

      -- Instance of policy id
      let curSymbol_1 = curSymbol params_1

      -- Write serialized registry script and minting policy
      putStrLn "Writing serialized registry script in 'registry.plutus'..."
      writeValidator "ledger/registry.plutus" registryA
      putStrLn "Writing serialized minting policy in 'mint.plutus'..."
      writePolicy "ledger/mint.plutus" policy_1
      putStrLn "Writing policy id in 'mint.policy'..."
      writeCS "ledger/mint.policy" $ curSymbol_1
      putStrLn "Done"
