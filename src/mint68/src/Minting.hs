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

module Minting where

import           Data.Aeson          (ToJSON, FromJSON)
import           GHC.Generics        (Generic)
import qualified PlutusTx
import           PlutusTx.Prelude       as TxPrelude hiding (unless)
import qualified Plutus.Script.Utils.V2.Scripts  as Scripts
import           Plutus.Script.Utils.Typed (mkUntypedMintingPolicy)
import qualified Ledger                 as L
import           Ledger.Value
import           Plutus.V2.Ledger.Api
import           Plutus.V2.Ledger.Contexts as V2LC

import Registry (RegDatum(..))


data MintingParams = MintingParams
  { mpUtxo    :: TxOutRef             -- UTxO to spend
  , mpPKH     :: L.PaymentPubKeyHash  -- Authorized minting PubKeyHash
  , mpValHash :: ValidatorHash        -- Registry's validator hash
  , mpRefName :: TokenName            -- Reference token name
  , mpUsrName :: TokenName            -- User token name
  } deriving (Generic, FromJSON, ToJSON)

PlutusTx.makeLift ''MintingParams

{-# INLINABLE mkPolicy #-}
-- | Minting Policy ensures that NFT is minted in pairs: reference and user NFT's
mkPolicy :: MintingParams -> () -> V2LC.ScriptContext -> Bool
mkPolicy mp () ctx = traceIfFalse "UTxO not consumed" hasUTxO                     &&
                     traceIfFalse "unauthenticated" authenticated                 &&
                     traceIfFalse "wrong amount minted" checkMintedAmount         &&
                     traceIfFalse "unrecognized ref token address" refTokenLocked &&
                     traceIfFalse "user token not sent properly" usrTokenSent
  where
    -- Script's context info
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- Transaction outputs
    outputs :: [TxOut]
    outputs = txInfoOutputs info

    -- Checks whether the chosen UTxO was spent
    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == (mpUtxo mp)) $ txInfoInputs info

    -- Only the "administrator" can mint
    authenticated :: Bool
    authenticated = txSignedBy info $ L.unPaymentPubKeyHash (mpPKH mp)

    refTkNm, usrTkNm :: TokenName
    (refTkNm, usrTkNm) = (mpRefName mp, mpUsrName mp)

    -- Checks whether the pair of reference and user NFT's was minted
    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
      [(_, tn1, amt1), (_, tn2, amt2)] -> amt1 == 1 && amt2 == 1 &&
                                          ((tn1, tn2) == (refTkNm, usrTkNm) ||
                                           (tn2, tn1) == (refTkNm, usrTkNm))
      _                                -> False

    -- Checks whether reference token with valid datum was locked at the Registry's script
    refTokenLocked :: Bool
    refTokenLocked = case filter (hasOneToken refTkNm) outputs of
      [refOut] -> case txOutDatum refOut of
        OutputDatum dat -> if checkDatum dat
          then case addressCredential $ txOutAddress refOut of
                 ScriptCredential vh -> vh == (mpValHash mp)
                 _                   -> traceError "ref token not sent to a script"
          else traceError "unexpected error"
        _               -> traceError "inline Datum missing"
      _        -> traceError "expected output with ref token"

    -- Checks whether user token was sent to a wallet
    usrTokenSent :: Bool
    usrTokenSent = case filter (hasOneToken usrTkNm) outputs of
      [usrOut] -> case addressCredential $ txOutAddress usrOut of
        PubKeyCredential _ -> True
        _                  -> False
      _        -> False

    -- 'hasOneToken tn' is used as a filter function to get the list of outputs containing
    -- exactly one token whose name is 'tn'.
    hasOneToken :: TokenName -> TxOut -> Bool
    hasOneToken tn o = valueOf (txOutValue o) (ownCurrencySymbol ctx) tn == 1

    -- 'checkDatum dat' is always True, unless datum is corrupted in which case returns
    -- error.
    checkDatum :: Datum -> Bool
    checkDatum dat = case unsafeFromBuiltinData $ getDatum dat of
      RegDatum {} -> True

-- | Minting policy compiled to Plutus Core
policy :: MintingParams -> MintingPolicy
policy mp = mkMintingPolicyScript $
  $$(PlutusTx.compile [|| mkUntypedMintingPolicy . mkPolicy ||])
  `PlutusTx.applyCode`
  PlutusTx.liftCode mp

-- | Currency symbol of minting policy
curSymbol :: MintingParams -> CurrencySymbol
curSymbol = Scripts.scriptCurrencySymbol . policy
