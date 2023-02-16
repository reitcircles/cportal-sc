{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module ToySC where

import qualified PlutusTx
import           PlutusTx.Prelude    as TxPrelude hiding (unless)
import qualified Ledger.Typed.Scripts      as Scripts
import           Ledger              hiding (singleton)


newtype ToyRedeemer = ToyRedeemer { key :: Integer }

PlutusTx.unstableMakeIsData ''ToyRedeemer

{-# INLINABLE mkToyValidator #-}
mkToyValidator :: () -> ToyRedeemer -> ScriptContext -> Bool
mkToyValidator _ red _ =
  traceIfFalse "wrong guess, you are out of luck!" $ (key red) == 43

data TypedToy
instance Scripts.ValidatorTypes TypedToy where
  type instance DatumType TypedToy = ()
  type instance RedeemerType TypedToy = ToyRedeemer

typedToyValidator :: Scripts.TypedValidator TypedToy
typedToyValidator = Scripts.mkTypedValidator @TypedToy
    $$(PlutusTx.compile [|| mkToyValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.mkUntypedValidator @() @ToyRedeemer

toyValidator :: Validator
toyValidator = Scripts.validatorScript typedToyValidator

