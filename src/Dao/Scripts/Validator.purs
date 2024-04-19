-- Just for debugging, will remove later
module Dao.Scripts.Validator
  ( alwaysFailsValidatorScript
  , alwaysSucceedsValidatorScript
  , indexValidatorScript
  , unappliedConfigValidator
  , unappliedTallyValidator
  , unappliedTreasuryValidator
  , unappliedVoteValidator
  )
  where

import Contract.Monad (Contract)
import Contract.PlutusData (toData)
import Contract.Prelude (pure, ($))
import Contract.Scripts (Validator(Validator))
import Dao.Scripts.Serialized.Debug as Debug
import Dao.Scripts.Serialized.Optimised as Optimised
import Dao.Scripts.Utils (mkUnappliedValidator')
import Dao.Scripts.Utils (mkScript')
import ScriptArguments.Types (ValidatorParams)

alwaysFailsValidatorScript :: Contract Validator
alwaysFailsValidatorScript = pure $ Validator $ mkScript' Optimised.alwaysFailsValidator

alwaysSucceedsValidatorScript :: Contract Validator
alwaysSucceedsValidatorScript = pure $ Validator $ mkScript' Optimised.alwaysSucceedsValidator

unappliedConfigValidator :: ValidatorParams -> Contract Validator
unappliedConfigValidator = mkUnappliedValidator' Optimised.configValidator

indexValidatorScript :: Contract Validator
indexValidatorScript = pure $ Validator $ mkScript' Optimised.indexValidator

unappliedTallyValidator :: ValidatorParams -> Contract Validator
unappliedTallyValidator = mkUnappliedValidator' Optimised.tallyValidator

unappliedTreasuryValidator :: ValidatorParams -> Contract Validator
unappliedTreasuryValidator = mkUnappliedValidator' Debug.treasuryValidator

unappliedVoteValidator :: ValidatorParams -> Contract Validator
unappliedVoteValidator = mkUnappliedValidator' Optimised.voteValidator
