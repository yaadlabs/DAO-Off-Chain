module Scripts.VoteValidator
  ( unappliedVoteValidator
  ) where

import Contract.Prelude

import Contract.Monad (Contract, liftContractE)
import Contract.PlutusData (toData)
import Contract.Prim.ByteArray (ByteArray)
import Contract.Scripts (PlutusScript, Validator(Validator), applyArgs)
import Ctl.Internal.Types.Scripts (plutusV2Script)
import ScriptArguments.Types (ConfigurationValidatorConfig)
import Scripts.Utils (scriptStringToJson, toJsonBytes')

unappliedVoteValidator :: ConfigurationValidatorConfig -> Contract Validator
unappliedVoteValidator configParams = do
  appliedVoteValidator <- liftContractE $ voteValidatorScript `applyArgs`
    [ toData configParams ]
  pure $ Validator appliedVoteValidator

voteValidatorScript :: PlutusScript
voteValidatorScript = plutusV2Script voteValidatorByteArray

voteValidatorByteArray :: ByteArray
voteValidatorByteArray = toJsonBytes' $ scriptStringToJson
  voteValidatorString

-- Generated validator string
voteValidatorString :: String
voteValidatorString =
  "WQzrAQAAMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMwISIiUzUzMzM1dIAGQEZARkpmpgVm6wAQB4iFTNQAQICIVM1ABAiIhUzUAECQiFTNQARUzUwKQCCFTNTMzMzV0gA5GZqYGYGhgfG6oAgDYgLiAtIC0jM1MDMDQ3WgEGBKJEZgBGYEyQABgYAXmYARmBMkAEYGAHAAJAXEBaBmQqZqZmZmaukAGIyUzNXNGB2YIAAImRmZqrnwASAxIyMzNVc+ACQGZGZmqufNXRABEpmpgUmroQBSEyUzUzMzM1dIACRkZKZmrmjBFABEzM1Vz5gkgBEB0RmZqrnzV0RglABkZKZqZmZmaukABIyMlMzVzRgmAAiZmaq58wTwAiBAIyMzNVc+ACQIRGRmZqrnwASBEIzM1Vz5q6IAIlM1MEQ1dCYKoBBCpmpgimroQBiFTNTBGNXQgCkJgkmZghABgBAAghAgggECKCWCUauiACBINXRGCgAGCMKmZq5owSwARMzNVc+YJ4ARAgEZmaq581dEYKAAZKZqYGxq6EwUQBCEwQzBDABA8IEEEcEYDowTwATdUAEQHpAekB6QHoIZCYHxgfAAgbmroTBLAEIDsEEEAVMzVzRgjAAiZmaq58wSQAiMDsDogOgQANDBJABN1QARAbkBuQG5AbgekJgcGYFwAYAIGJq6EAQC8gNAOgOTV0QAQG5gfgAgVG6oAcgLiAuIC4gLgNCFTNTAtAFITAwEjMzABAFAEADACApAoAnAmIgKCAjICMCkhUzUzMzM1dIAGRmamBUBWYGpuqAEAtICUgJCAkIzNTAqArN1oAhgOCRGYARmA6kAAYE4ExmAEZgOpABGBOBeACQEpASAVEJkpmpmYA4AYASmZq5owMTA2ABEyMjISMwAQAwAjASNXQmrojA3ADUzNXNGBkYG4AImRkZGRkZGRkZGRkZGRkZGRkZGRkZGRkJGZmZmZmZgAgMALAKAJAIAHAGAEgDgCgBgBGBeauhNXRABGZgSOuIAE1dCACauiACMzAiAkIAE1dCACauiACMwIXXGroQATV0QAamZq5owQTBGABEyMjIyEjMAEAQAJTM1c0YIhgkgAiZGQkZgAgBgBGA4auhNXRGCSAEZgOOtNXQmCQACLG6o1dCauiMEcANTM1c0YIRgjgAiZGQkZgAgBgBGA0auhNXRGCOAEZgNOtNXQmCMACLG6o1dCYIoAIsbqjV0IAJq6IAIzMBsB91pq6EAE1dEAEZgNAOGroQATV0QARmYC7rgFjV0IAJq6IAIzMBV1wChq6EAE1dEAEZgKAIGroQATV0QARmAkAaauhABNXRGBuAEZgIAFmroTA2ABFjdUauhMDUAEWESABFjdUAGLCxERGoAhEagCkREagDkRqAERERERERGRKZqAuJkZKZqZgXpIBLlRyYW5zYWN0aW9uIHNob3VsZCBiZSBzaWduZWQgYnkgdGhlIHZvdGUgb3duZXIAMzArMD0EAzA8IzAoABNQFARAOgBxABA0MwLkkSBBbGwgdm90ZSB0b2tlbnMgc2hvdWxkIGJlIGJ1cm5lZABTNTMwKjA8A/MwOzBKIlM1ABA1IhNQAiJTM1c0ZuPACAHA6EwBgAyNQASA3AMAzA0NVACIiIiIiIiIiIiIiIAYTMC1JAEdTWlzc2luZyBUYWxseSBWYWxpZGF0b3IgaW5wdXQAMzApMDsD4zA6IzAmABMDo1UAMiIiIiIiIiIiIiIgFjMDojUAEEIzA6I1ABIiIAQjUAED0A0VM1MzVTMEciESIlM1ABEAIiEzAFACMzVTAHBDAFAEABA8IlM1MwSSJTNQAQNCITUAIiUzNXNGbjwAgGxMwTyJTNQAQOiITUAIiUzNXNGbjwAgIBE1c0YKAAImAMAGACJgDABmpqAEB4QGomYIQAQAIgAgfAFCxEJqAERGoAJERKZqAOKmZqAELEKmamBuACQgAixCZgoESmagAixEJqAERKZmrmjNx4AQA4qZqYHoAJCACLCYAwAYBRELEZKZmrmjAsABAbFTM1c0YFYAIDQsYF5uqABIjIyUzNXNGBcACIkRAAipmauaMC0AETISIjADAEMAQ1dCYGAAQqZmrmjAsABESIgAhYwMAATdUACRkpmauaMCkwLgARMjISMwAQAwAjAENXQmrojAuACMA41dCYFoAIsbqgASMlMzVzRgUGBaACJkZGRkZGRkZGQkZmYAIBIA4AYARmAW641dCauiAEUzNXNGBiACJkJERgBACGroTAzACFTM1c0YGAAImQkRGACAIbrjV0JgZgBCpmauaMC8AERIiADFjAzABN1Rq6EAE1dEAEZmAQ64Ac1dCACauiMC4ANTM1c0YFJgXAAiZgKGAYauhMC0AEzAEAKNXQmrojAtABFjdUauhMCwAEWN1QAJEZGSmZq5owKQARMB4wBDV0JgWgBCpmauaMCoAEBgWMC0AE3VAAkZmAE651oAJERmBYRGZmqufABICYjIzApMwFTAHMC8AEwBjAuABMAQ1dEAGauhACAjN1YAJEZgVERmZqrnwASAkIzAmMAU1dCAEYAZq6IAICE3WAAkZGSmZq5owJwARMhIiIwBABTAENXQmBQAEKmZq5owJgARMhIiIwAgBTAFNXQmBQAEKmZq5owJQARMhIiIwAQBTAJNXQmBQAEKmZq5owJAARMhIiIwAwBTdcauhMCgAIWMCgAE3VAAkZGSmZq5ozcOkAYACIkREAwKmZq5ozcOkAUACIkREREAIKmZq5ozcOkAQACJkZCRERERmACASAQbrTV0Jq6IwKAAzdcauhMCcAIVMzVzRgTAAiZGQkREREZgBAEgEG641dCauiMCgAM3XGroTAnACFTM1c0YEoAImRkJERERGYAwBIBBuuNXQmrojAoADMAQ1dCYE4AQqZmrmjAkABEyEiIiIjAHAIMAQ1dCYE4AQqZmrmjAjABEyEiIiIjAFAIMAQ1dCYE4AQsYE4AJuqABIyMlMzVzRgRgAiZGZgIm601dCYE4AZutNXQgAm601dCauiABNXRGBMAEKmZq5owIgARMBcwBTV0JgTABCxgTAAm6oAEiM1ACIzUAIjMAYAIAEgFiM1ACIBYjMAYAIAEjIyUzNXNGBCACJgLG641dCYEgAQqZmrmjAgABEwFTdcauhMCQAIWMCQAE3VAAkQmrmjNx4AQAJGSmZq5owHTAiABEyMhIzABADACN1pq6E1dEYEQARgCGroTAhABFjdUACRGRmqgBiRmACRGZgEAPABAAgBABGAIAqRkpmauaMBswIAARN1xq6EwHwARY3VAAkZgAkSmagBAFiACAaRmZmZq6QAEjIyUzNXNGA4ACJmZqrnzAfACIBAjMzVXPmrojAgADJTNTAPNXQmBCAIQmAmYCgAIBhAIgLgLCpmauaMBsAETMzVXPmA+AEQCBGZmqufNXRGBAAGSmamAeauhMCEAQhMBMwEwAQDCARAXAWAKMB8AE3VABEAaQBpAGkAaAmRKZqACAQIBhEJGYAIAYARGagAgKAJEZmZmaukABIAkgCSUzUwETdYAEAIRCpmoAIAxEKmagAgEEQqZqACAURCpmoAIBhEKmagAgHEQqZqACAgRCpmoAICREKmagAgKEQqZqACAsRCpmoAIDBEKmagAgNEQqZqACA4RCpmoAIDxEKmagAgQEQqZqACBERCpmoAIEhEKmagAgTEQqZqACBQRCpmoAIFREKmagAgWEQqZqACBcRCpmoAIqZqYGYFhCpmpgaAVkKmamBqBUQqZqYGwFJCpmpgcAUEKmamByBOQqZqYHQExCpmpgdgSkKmamB4BIQqZqYHoEZCpmpgfAREKmamB+BCQqZqYIAEBCpmpgggPkKmamCEA8QqZqYIQDpCpmpghgOEKmamCIA2QqZqYIoDRCpmpgjAMkKmamCOAwQqZqYJIC5CYJgkZmZmZmZmZmZmZmYAIC4CwCoCgCYCQCICAB4BwBoBgBYBQBIBAA4AwAoAgAYAQIoIgIYIQIIIAH4HwHoHgHYHQHIHAG4GwGoGgGYGQGIGBEBkQBJAEgHkRCRGZgAgCgCABiAKIgHERABkZmZmaukABIAQgBCAEIAQjAFN1wAQBRGZmZmrpAASADIAMgAyMAQ3WgBEAGASIApCRGAEAGQkRgAgBkRGAGYAQAJgHEQiREpmoAImoAYAxEJmagCgGmAIAEZmqmAOAUAKAIACRAAmAYRCREpmoAQmACAGRCagBESmZq5ozcOAOAEIAImZqYBIBQA4AYAokACYBREZmaq58AEgBCMwBjV0IARgBmrogAkmEAEiACIhIjMAEAQAM3DpAAG4dIAI3DpACG4dIAZVc8qudIyMAEAEiMwAzACACABQ=="
