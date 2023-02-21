{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Registry where

--import           Data.Aeson          (ToJSON, FromJSON)
--import           GHC.Generics        (Generic)
import qualified PlutusTx
import           PlutusTx.Prelude       as TxPrelude hiding (unless)
-- import           PlutusTx.Builtins.Class
import qualified Plutus.Script.Utils.V2.Scripts  as Scripts
import           Plutus.Script.Utils.Typed (mkUntypedValidator)
-- import           Ledger.Ada             as Ada
import qualified Ledger                 as L
--import           Ledger.Value
import           Plutus.V2.Ledger.Api
import           Plutus.V2.Ledger.Contexts as V2LC
import           Plutus.Script.Utils.V2.Typed.Scripts.Validators as V2V


data RegDatum = RegDatum
  { rdURL  :: BuiltinByteString
  , rdHash :: BuiltinByteString
  }

PlutusTx.unstableMakeIsData ''RegDatum

{-# INLINABLE mkRegistry #-}
-- | The Registry script stores the 'reference NFT'
mkRegistry :: L.PaymentPubKeyHash -> RegDatum -> () -> V2LC.ScriptContext -> Bool
mkRegistry pkh _ () ctx = traceIfFalse "unauthoroized" authorized
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    authorized :: Bool
    authorized = txSignedBy info $ L.unPaymentPubKeyHash pkh

data TypedReg
instance V2V.ValidatorTypes TypedReg where
  type instance DatumType TypedReg    = RegDatum
  type instance RedeemerType TypedReg = ()

-- | Typed registry script compiled to Plutus Core
typedRegistry :: L.PaymentPubKeyHash -> V2V.TypedValidator TypedReg
typedRegistry pkh = V2V.mkTypedValidator @TypedReg
  ($$(PlutusTx.compile [|| mkRegistry ||])
     `PlutusTx.applyCode` PlutusTx.liftCode pkh)
  $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = mkUntypedValidator @ScriptContext @RegDatum @()

-- | Registry script
registry :: L.PaymentPubKeyHash -> Scripts.Validator
registry = V2V.validatorScript . typedRegistry

