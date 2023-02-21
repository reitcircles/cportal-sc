{-# LANGUAGE NoImplicitPrelude     #-}

module Config where

-- import Ledger                  as L
import Ledger.Value
-- import Ledger.Ada
import PlutusTx.Prelude
import PlutusTx.Builtins.Class

import Data.Maybe (fromJust)
import Data.Text (pack)
--import PlutusTx.Builtins.Class (toBuiltin)
import Prelude (String)
import Text.Hex (decodeHex)


-- Auxiliary Functions

hexConvert :: String -> BuiltinByteString
hexConvert = toBuiltin . fromJust . decodeHex . pack

-- Token Names

referenceTokenName :: TokenName
referenceTokenName = TokenName $ stringToBuiltinByteString "(100)REFERENCE"

userTokenName :: TokenName
userTokenName = TokenName $ stringToBuiltinByteString "(222)USER"

-- Global parameters

-- valMinAda :: Value
-- valMinAda = toValue L.minAdaTxOut
