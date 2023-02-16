{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Deploy
    ( writeJSON
    , writeValidator
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
import qualified Ledger

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

writeValidator :: FilePath -> Ledger.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Ledger.unValidatorScript

