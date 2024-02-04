module Dao.Scripts.Validator.Vote
  ( unappliedVoteValidator
  , unappliedVoteValidatorDebug
  ) where

import Contract.Monad (Contract)
import Contract.Scripts (Validator)
import Dao.Scripts.Utils (mkUnappliedValidator)
import ScriptArguments.Types (ValidatorParams)

unappliedVoteValidator :: ValidatorParams -> Contract Validator
unappliedVoteValidator = mkUnappliedValidator
  "./src/Dao/Scripts/Json/Optimised/VoteValidator.json"

unappliedVoteValidatorDebug ::
  ValidatorParams -> Contract Validator
unappliedVoteValidatorDebug = mkUnappliedValidator
  "./src/Dao/Scripts/Json/Debug/VoteValidator.json"
