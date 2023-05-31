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


newtype PlatformParams = PlatformParams { pkh :: String }

instance FromJSON PlatformParams where
  parseJSON (Object v) = PlatformParams <$> v .: "pkh"
  parseJSON _          = fail "Expected object with policy parameters"

main :: IO ()
main = do
  -- Read JSON file with platform parameters into a bytestring
  input <- B.readFile "ledger/platform-params.json"

  -- Parse JSON bytestring into corresponding Haskell value
  let result = eitherDecode input :: Either String PlatformParams
  case result of
    Left err -> putStrLn err
    Right pparams -> do
      -- Administrator's pub-key-hash
      let pkhAdmin = L.PaymentPubKeyHash . PubKeyHash . hexConvert . pkh $ pparams

      -- Platform's Registry validator
      let registryPlatform = registry pkhAdmin

      -- Parameters determining a pre-minting policy
      let premint_params = PreMintParams
            { mpPKH     = pkhAdmin
            , mpValHash = validatorHash' registryPlatform
            , mpRefName = referenceTokenName
            , mpUsrName = userTokenName
            }

      -- Instance of minting policy
      let premint_policy = preMintPolicy premint_params

      -- Write serialized registry script and minting policy
      putStrLn "Writing serialized registry script in 'registry.plutus'..."
      writeValidator "ledger/registry.plutus" registryPlatform
      putStrLn "Writing serialized pre-minting policy in 'premint.plutus'..."
      writePolicy "ledger/premint.plutus" premint_policy
      putStrLn "Done"
