module Dao.Utils.Error (guardContract) where

import Contract.Monad (Contract, throwContractError)
import Contract.Prelude (Unit, pure, unit)

guardContract :: String -> Boolean -> Contract Unit
guardContract s cond = if cond then pure unit else throwContractError s
