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
-- import qualified Plutus.Script.Utils.V2.Scripts  as Scripts
import           Plutus.Script.Utils.V2.Typed.Scripts (mkUntypedMintingPolicy)
import qualified Ledger                 as L
import           Plutus.V1.Ledger.Value
import           Plutus.V2.Ledger.Api
import           Plutus.V2.Ledger.Contexts as V2LC

import Registry (RegDatum(..))


data PreMintParams = PreMintParams
  { mpPKH     :: L.PaymentPubKeyHash  -- Authorized minting PubKeyHash
  , mpValHash :: ValidatorHash        -- Registry's validator hash
  , mpRefName :: TokenName            -- Reference token name
  , mpUsrName :: TokenName            -- User token name
  } deriving (Generic, FromJSON, ToJSON)

newtype MintInstanceParams = MintInstanceParams { mpUtxo :: TxOutRef }  -- UTxO to spend
  deriving (Generic, FromJSON, ToJSON)

PlutusTx.makeLift ''PreMintParams
PlutusTx.unstableMakeIsData ''MintInstanceParams

{-# INLINABLE mkPolicyT #-}
-- | Minting Policy ensures, among other things, that NFT is minted in pairs: reference
-- and user NFT's
mkPolicyT :: PreMintParams -> MintInstanceParams -> () -> V2LC.ScriptContext -> Bool
mkPolicyT pmp mip () ctx = traceIfFalse "UTxO not consumed" hasUTxO                     &&
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
    hasUTxO = any (\i -> txInInfoOutRef i == (mpUtxo mip)) $ txInfoInputs info

    -- Only the "administrator" can mint
    authenticated :: Bool
    authenticated = txSignedBy info $ L.unPaymentPubKeyHash (mpPKH pmp)

    refTkNm, usrTkNm :: TokenName
    (refTkNm, usrTkNm) = (mpRefName pmp, mpUsrName pmp)

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
                 ScriptCredential vh -> vh == (mpValHash pmp)
                 _                   -> traceError "ref token not sent to a script"
          else traceError "unexpected error"
        _               -> traceError "inline Datum missing"
      _        -> traceError "expected output with exactly one ref token"

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

{-# INLINABLE mkPolicy #-}
-- | Untyped minting policy
mkPolicy :: PreMintParams -> BuiltinData -> BuiltinData -> V2LC.ScriptContext -> Bool
mkPolicy pmp mipbd ubd = mkPolicyT pmp mip u
  where
    mip = PlutusTx.unsafeFromBuiltinData mipbd
    u   = PlutusTx.unsafeFromBuiltinData ubd

-- | Minting policy compiled to Plutus Core
preMintPolicy :: PreMintParams -> MintingPolicy
preMintPolicy pmp = MintingPolicy $ fromCompiledCode
  ($$(PlutusTx.compile [|| occurrence ||])
     `PlutusTx.applyCode` PlutusTx.liftCode pmp)
  where
    occurrence pmp' mipbd = mkUntypedMintingPolicy $ mkPolicy pmp' mipbd
    
-- -- | Currency symbol of minting policy
-- curSymbol :: MintingParams -> CurrencySymbol
-- curSymbol = Scripts.scriptCurrencySymbol . policy
