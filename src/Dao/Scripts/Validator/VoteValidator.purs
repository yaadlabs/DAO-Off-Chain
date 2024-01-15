module Dao.Scripts.Validator.VoteValidator
  ( unappliedVoteValidator
  , unappliedVoteValidatorDebug
  ) where

import Contract.Monad (Contract)
import Contract.Scripts (Validator)
import Dao.Scripts.Utils (mkUnappliedValidator)
import LambdaBuffers.ApplicationTypes.Arguments (ConfigurationValidatorConfig)

unappliedVoteValidator :: ConfigurationValidatorConfig -> Contract Validator
unappliedVoteValidator = mkUnappliedValidator
  "./src/Dao/Scripts/Json/Optimised/VoteValidator.json"

unappliedVoteValidatorDebug ::
  ConfigurationValidatorConfig -> Contract Validator
unappliedVoteValidatorDebug = mkUnappliedValidator
  "./src/Dao/Scripts/Json/Debug/VoteValidator.json"
