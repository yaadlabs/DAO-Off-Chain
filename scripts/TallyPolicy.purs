module Scripts.TallyPolicy
  ( unappliedTallyPolicy
  ) where

import Contract.Prelude

import Contract.Monad (Contract, liftContractE)
import Contract.PlutusData (toData)
import Contract.Prim.ByteArray (ByteArray)
import Contract.Scripts
  ( MintingPolicy(PlutusMintingPolicy)
  , PlutusScript
  , applyArgs
  )
import Ctl.Internal.Types.Scripts (plutusV2Script)
import ScriptArguments.Types (TallyNftConfig)
import Scripts.Utils (scriptStringToJson, toJsonBytes')

unappliedTallyPolicy :: TallyNftConfig -> Contract MintingPolicy
unappliedTallyPolicy configParams = do
  appliedTallyPolicy <- liftContractE $ tallyMintingPolicyScript `applyArgs`
    [ toData configParams ]
  pure $ PlutusMintingPolicy appliedTallyPolicy

tallyMintingPolicyScript :: PlutusScript
tallyMintingPolicyScript = plutusV2Script tallyPolicyByteArray

tallyPolicyByteArray :: ByteArray
tallyPolicyByteArray = toJsonBytes' $ scriptStringToJson tallyPolicyString

tallyPolicyString :: String
tallyPolicyString =
  "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"
