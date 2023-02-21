{-# LANGUAGE NoImplicitPrelude     #-}

module Config where

import Ledger.Value
import PlutusTx.Prelude
import PlutusTx.Builtins.Class
import Data.Maybe              (fromJust)
import Data.Text               (pack)
import Prelude                 (String)
import Text.Hex                (decodeHex)
import Plutus.Script.Utils.V2.Scripts -- as Scripts
import Plutus.V2.Ledger.Api           -- as PV2


-- Helper Functions

hexConvert :: String -> BuiltinByteString
hexConvert = toBuiltin . fromJust . decodeHex . pack

validatorHash' :: Validator -> ValidatorHash
validatorHash' = ValidatorHash . getScriptHash . scriptHash . unValidatorScript

-- Token Names

referenceTokenName :: TokenName
referenceTokenName = TokenName $ stringToBuiltinByteString "(100)REFERENCE"

userTokenName :: TokenName
userTokenName = TokenName $ stringToBuiltinByteString "(222)USER"

