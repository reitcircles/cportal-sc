{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module ToySC where

--import           Data.Aeson          (ToJSON, FromJSON)
--import           GHC.Generics        (Generic)
import qualified PlutusTx
import           PlutusTx.Prelude    as TxPrelude hiding (unless)
import qualified Ledger.Typed.Scripts      as Scripts
--import           Ledger.Ada          as Ada
import           Ledger              hiding (singleton)
--import           Ledger.Value


data ToyRedeemer = ToyRedeemer { key :: Integer }

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

