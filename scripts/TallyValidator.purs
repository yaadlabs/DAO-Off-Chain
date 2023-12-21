module Scripts.TallyValidator
  ( unappliedTallyValidator
  ) where

import Contract.Prelude

import Contract.Monad (Contract, liftContractE)
import Contract.PlutusData (toData)
import Contract.Prim.ByteArray (ByteArray)
import Contract.Scripts (PlutusScript, Validator(Validator), applyArgs)
import Ctl.Internal.Types.Scripts (plutusV2Script)
import ScriptArguments.Types (ConfigurationValidatorConfig)
import Scripts.Utils (scriptStringToJson, toJsonBytes')

unappliedTallyValidator :: ConfigurationValidatorConfig -> Contract Validator
unappliedTallyValidator configParams = do
  appliedTallyValidator <- liftContractE $ tallyValidatorScript `applyArgs`
    [ toData configParams ]
  pure $ Validator appliedTallyValidator

tallyValidatorScript :: PlutusScript
tallyValidatorScript = plutusV2Script tallyValidatorByteArray

tallyValidatorByteArray :: ByteArray
tallyValidatorByteArray = toJsonBytes' $ scriptStringToJson
  tallyValidatorString

-- Generate tally validator string
tallyValidatorString :: String
tallyValidatorString =
  "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"