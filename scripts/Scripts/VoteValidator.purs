module Scripts.VoteValidator
  ( unappliedVoteValidator
  , unappliedVoteValidatorDebug
  ) where

import Contract.Monad (Contract)
import Contract.Scripts (Validator)
import ScriptArguments.Types
  ( ConfigurationValidatorConfig(ConfigurationValidatorConfig)
  )
import Scripts.Utils (mkUnappliedValidator)

unappliedVoteValidator :: ConfigurationValidatorConfig -> Contract Validator
unappliedVoteValidator = mkUnappliedValidator
  "./scripts/Json/Optimised/VoteValidator.json"

unappliedVoteValidatorDebug ::
  ConfigurationValidatorConfig -> Contract Validator
unappliedVoteValidatorDebug = mkUnappliedValidator
  "./scripts/Json/Debug/VoteValidator.json"
