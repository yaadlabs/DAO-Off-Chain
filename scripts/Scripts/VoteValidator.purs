module Scripts.VoteValidator
  ( unappliedVoteValidator
  ) where

import Contract.Monad (Contract)
import Contract.Scripts (Validator)
import LambdaBuffers.ApplicationTypes.Arguments (ConfigurationValidatorConfig)
import Scripts.Utils (mkUnappliedValidator)

unappliedVoteValidator :: ConfigurationValidatorConfig -> Contract Validator
unappliedVoteValidator = mkUnappliedValidator
  "./scripts/Json/Optimised/VoteValidator.json"
