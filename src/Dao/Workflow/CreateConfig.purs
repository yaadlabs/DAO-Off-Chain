module Dao.Workflow.CreateConfig where

import Contract.Prelude
import LambdaBuffers.ApplicationTypes.Configuration (DynamicConfigDatum)
import Contract.Monad (Contract, liftContractE, liftContractM, liftedM)
import Contract.Log (logInfo')
import Contract.Wallet (ownPaymentPubKeyHashes)
import LambdaBuffers.ApplicationTypes.Vote
import Scripts.ConfigPolicy (configMintingPolicyScript)
import Scripts.ConfigValidator (configValidatorScript)

createConfig :: Contract Unit
createConfig = pure unit
