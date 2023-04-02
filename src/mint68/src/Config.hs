{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Config where

import PlutusTx.Prelude
import Data.Maybe              (fromJust)
import Data.Text               (pack)
import Prelude                 (String)
import Text.Hex                (decodeHex)
import Plutus.Script.Utils.V2.Scripts -- as Scripts
import Plutus.V2.Ledger.Api           -- as PV2

import Registry (RegDatum(..))

-- Helper Functions

hexConvert :: String -> BuiltinByteString
hexConvert = toBuiltin . fromJust . decodeHex . pack

validatorHash' :: Validator -> ValidatorHash
validatorHash' = ValidatorHash . getScriptHash . scriptHash . unValidatorScript

-- Token Names

referenceTokenName :: TokenName
referenceTokenName = TokenName $ "(100)REFERENCE"

userTokenName :: TokenName
userTokenName = TokenName $ "(222)USER"

-- Datum

-- | A Datum example
datum_1 :: RegDatum
datum_1 = RegDatum
  { rdURL  = "ipfs://QmVUSyxrvSEwATqGNefKYVtx5ixJrBnD9XjMRJx9JK9Nuh"
  , rdHash = "b5d4045c3f466fa91fe2cc6abe79232a1a57cdf104f7a26e716e0a1e2789df78"
  }
