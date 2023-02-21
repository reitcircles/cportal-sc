{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Deploy
    ( writeJSON
    , writeValidator
    , writePolicy
    , writePolicy'
    , writePolicy''
    , writeCS
    , writeUnit
    ) where

import           Cardano.Api
import           Cardano.Api.Shelley   (PlutusScript (..))
import           Codec.Serialise       (serialise)
import           Data.Aeson            (encode)
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import           PlutusTx              (Data (..))
import qualified PlutusTx
-- import qualified Ledger
import qualified Plutus.V2.Ledger.Api as V2L
import qualified Plutus.Script.Utils.V2.Scripts as Scripts

dataToScriptData :: Data -> ScriptData
dataToScriptData (Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (Map xs)      = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (List xs)     = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (I n)         = ScriptDataNumber n
dataToScriptData (B bs)        = ScriptDataBytes bs

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . PlutusTx.toData

writeUnit :: FilePath -> IO ()
writeUnit file = writeJSON file ()

writeValidator :: FilePath -> Scripts.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . V2L.unValidatorScript

writePolicy' :: FilePath -> Scripts.MintingPolicy -> IO (Either (FileError ()) ())
writePolicy' file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . V2L.unMintingPolicyScript

writePolicy'' :: FilePath -> Scripts.MintingPolicy -> IO (Either (FileError ()) ())
writePolicy'' file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . V2L.Validator . V2L.unMintingPolicyScript

writeCS :: FilePath -> V2L.CurrencySymbol -> IO ()
writeCS file cs = writeFile file (show cs)

type FilePathWithoutExt = String

-- | Please omit extension.  Generates .plutus & .hex files (script & currency symbol).
writePolicy :: FilePathWithoutExt -> Scripts.MintingPolicy -> IO ()
writePolicy file mp = do
  let cs = Scripts.scriptCurrencySymbol mp
      fileP = file <> ".plutus"
      fileH = file <> ".hex"
  _ <- writeFileTextEnvelope @(PlutusScript PlutusScriptV2) fileP Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . V2L.unMintingPolicyScript $ mp
  writeFile fileH (show cs)

