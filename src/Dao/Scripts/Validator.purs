-- Just for debugging, will remove later
module Dao.Scripts.Validator
  ( alwaysFailsValidatorScript
  , alwaysSucceedsValidatorScript
  , indexValidatorScript
  , unappliedConfigValidator
  , unappliedTallyValidator
  , unappliedTreasuryValidator
  , unappliedVoteValidator
  ) where

import Cardano.Types (PlutusScript)
import Contract.Monad (Contract)
import Contract.PlutusData (toData)
import Contract.Prelude (pure, ($))
import Dao.Scripts.Serialized.Debug as Debug
import Dao.Scripts.Serialized.Optimised as Optimised
import Dao.Scripts.Utils (mkScript')
import Dao.Scripts.Utils (mkUnappliedValidator')
import ScriptArguments.Types (ValidatorParams)

alwaysFailsValidatorScript :: Contract PlutusScript
alwaysFailsValidatorScript = pure $ mkScript'
  Optimised.alwaysFailsValidator

alwaysSucceedsValidatorScript :: Contract PlutusScript
alwaysSucceedsValidatorScript = pure $ mkScript'
  Optimised.alwaysSucceedsValidator

unappliedConfigValidator :: ValidatorParams -> Contract PlutusScript
unappliedConfigValidator = mkUnappliedValidator' Optimised.configValidator

indexValidatorScript :: Contract PlutusScript
indexValidatorScript = pure $ mkScript' Optimised.indexValidator

unappliedTallyValidator :: ValidatorParams -> Contract PlutusScript
unappliedTallyValidator = mkUnappliedValidator' Optimised.tallyValidator

unappliedTreasuryValidator :: ValidatorParams -> Contract PlutusScript
unappliedTreasuryValidator = mkUnappliedValidator' Debug.treasuryValidator

unappliedVoteValidator :: ValidatorParams -> Contract PlutusScript
unappliedVoteValidator = mkUnappliedValidator' Optimised.voteValidator
