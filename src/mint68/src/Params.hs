{-# LANGUAGE NoImplicitPrelude     #-}

module Params where

-- import Ledger                  as L
import Ledger.Value
-- import Ledger.Ada
import PlutusTx.Prelude
import PlutusTx.Builtins.Class

-- Token Names

referenceTokenName :: TokenName
referenceTokenName = TokenName $ stringToBuiltinByteString "(100)REFERENCE"

userTokenName :: TokenName
userTokenName = TokenName $ stringToBuiltinByteString "(222)USER"

-- Tmp

txid1_tmp :: BuiltinByteString
txid1_tmp = stringToBuiltinByteString "9c087132a325f6483aca8398bab1a56eda1390e762984ba054c25cafd738486c"

-- Global parameters

-- valMinAda :: Value
-- valMinAda = toValue L.minAdaTxOut
