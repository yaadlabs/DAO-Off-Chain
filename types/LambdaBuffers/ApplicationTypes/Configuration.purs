module LambdaBuffers.ApplicationTypes.Configuration (DynamicConfigDatum(..)) where

import Cardano.FromData (class FromData, fromData)
import Cardano.ToData (class ToData, toData)
import Cardano.Types (AssetName, ScriptHash)
import Cardano.Types.PlutusData (PlutusData(List)) as PlutusData
import Data.Generic.Rep as Data.Generic.Rep
import Data.Maybe as Data.Maybe
import Data.Newtype as Data.Newtype
import Data.Show as Data.Show
import Data.Show.Generic as Data.Show.Generic
import JS.BigInt (BigInt)
import LambdaBuffers.Runtime.Plutus as LambdaBuffers.Runtime.Plutus
import Prelude as Prelude

newtype DynamicConfigDatum = DynamicConfigDatum
  { tallyValidator :: ScriptHash
  , treasuryValidator :: ScriptHash
  , configurationValidator :: ScriptHash
  , voteValidator :: ScriptHash
  , upgradeMajorityPercent :: BigInt
  , upgradeRelativeMajorityPercent :: BigInt
  , generalMajorityPercent :: BigInt
  , generalRelativeMajorityPercent :: BigInt
  , tripMajorityPercent :: BigInt
  , tripRelativeMajorityPercent :: BigInt
  , totalVotes :: BigInt
  , maxGeneralDisbursement :: BigInt
  , maxTripDisbursement :: BigInt
  , agentDisbursementPercent :: BigInt
  , proposalTallyEndOffset :: BigInt
  , tallyNft :: ScriptHash
  , voteCurrencySymbol :: ScriptHash
  , voteTokenName :: AssetName
  , voteNft :: ScriptHash
  , voteFungibleCurrencySymbol :: ScriptHash
  , voteFungibleTokenName :: AssetName
  , fungibleVotePercent :: BigInt
  }

derive instance Data.Newtype.Newtype DynamicConfigDatum _
derive instance Data.Generic.Rep.Generic DynamicConfigDatum _
instance Data.Show.Show DynamicConfigDatum where
  show x = Data.Show.Generic.genericShow x

instance Prelude.Eq DynamicConfigDatum where
  eq =
    ( \x0 ->
        ( \x1 -> Prelude.(&&)
            ( Prelude.(&&)
                ( Prelude.(&&)
                    ( Prelude.(&&)
                        ( Prelude.(&&)
                            ( Prelude.(&&)
                                ( Prelude.(&&)
                                    ( Prelude.(&&)
                                        ( Prelude.(&&)
                                            ( Prelude.(&&)
                                                ( Prelude.(&&)
                                                    ( Prelude.(&&)
                                                        ( Prelude.(&&)
                                                            ( Prelude.(&&)
                                                                ( Prelude.(&&)
                                                                    ( Prelude.(&&)
                                                                        ( Prelude.(&&)
                                                                            ( Prelude.(&&)
                                                                                ( Prelude.(&&)
                                                                                    ( Prelude.(&&)
                                                                                        ( Prelude.(&&)
                                                                                            ( Prelude.(==)
                                                                                                ( ( Data.Newtype.unwrap
                                                                                                      x0
                                                                                                  ).tallyValidator
                                                                                                )
                                                                                                ( ( Data.Newtype.unwrap
                                                                                                      x1
                                                                                                  ).tallyValidator
                                                                                                )
                                                                                            )
                                                                                            ( Prelude.(==)
                                                                                                ( ( Data.Newtype.unwrap
                                                                                                      x0
                                                                                                  ).treasuryValidator
                                                                                                )
                                                                                                ( ( Data.Newtype.unwrap
                                                                                                      x1
                                                                                                  ).treasuryValidator
                                                                                                )
                                                                                            )
                                                                                        )
                                                                                        ( Prelude.(==)
                                                                                            ( ( Data.Newtype.unwrap
                                                                                                  x0
                                                                                              ).configurationValidator
                                                                                            )
                                                                                            ( ( Data.Newtype.unwrap
                                                                                                  x1
                                                                                              ).configurationValidator
                                                                                            )
                                                                                        )
                                                                                    )
                                                                                    ( Prelude.(==)
                                                                                        ( ( Data.Newtype.unwrap
                                                                                              x0
                                                                                          ).voteValidator
                                                                                        )
                                                                                        ( ( Data.Newtype.unwrap
                                                                                              x1
                                                                                          ).voteValidator
                                                                                        )
                                                                                    )
                                                                                )
                                                                                ( Prelude.(==)
                                                                                    ( ( Data.Newtype.unwrap
                                                                                          x0
                                                                                      ).upgradeMajorityPercent
                                                                                    )
                                                                                    ( ( Data.Newtype.unwrap
                                                                                          x1
                                                                                      ).upgradeMajorityPercent
                                                                                    )
                                                                                )
                                                                            )
                                                                            ( Prelude.(==)
                                                                                ( ( Data.Newtype.unwrap
                                                                                      x0
                                                                                  ).upgradeRelativeMajorityPercent
                                                                                )
                                                                                ( ( Data.Newtype.unwrap
                                                                                      x1
                                                                                  ).upgradeRelativeMajorityPercent
                                                                                )
                                                                            )
                                                                        )
                                                                        ( Prelude.(==)
                                                                            ( ( Data.Newtype.unwrap
                                                                                  x0
                                                                              ).generalMajorityPercent
                                                                            )
                                                                            ( ( Data.Newtype.unwrap
                                                                                  x1
                                                                              ).generalMajorityPercent
                                                                            )
                                                                        )
                                                                    )
                                                                    ( Prelude.(==)
                                                                        ( ( Data.Newtype.unwrap
                                                                              x0
                                                                          ).generalRelativeMajorityPercent
                                                                        )
                                                                        ( ( Data.Newtype.unwrap
                                                                              x1
                                                                          ).generalRelativeMajorityPercent
                                                                        )
                                                                    )
                                                                )
                                                                ( Prelude.(==)
                                                                    ( ( Data.Newtype.unwrap
                                                                          x0
                                                                      ).tripMajorityPercent
                                                                    )
                                                                    ( ( Data.Newtype.unwrap
                                                                          x1
                                                                      ).tripMajorityPercent
                                                                    )
                                                                )
                                                            )
                                                            ( Prelude.(==)
                                                                ( ( Data.Newtype.unwrap
                                                                      x0
                                                                  ).tripRelativeMajorityPercent
                                                                )
                                                                ( ( Data.Newtype.unwrap
                                                                      x1
                                                                  ).tripRelativeMajorityPercent
                                                                )
                                                            )
                                                        )
                                                        ( Prelude.(==)
                                                            ( ( Data.Newtype.unwrap
                                                                  x0
                                                              ).totalVotes
                                                            )
                                                            ( ( Data.Newtype.unwrap
                                                                  x1
                                                              ).totalVotes
                                                            )
                                                        )
                                                    )
                                                    ( Prelude.(==)
                                                        ( ( Data.Newtype.unwrap
                                                              x0
                                                          ).maxGeneralDisbursement
                                                        )
                                                        ( ( Data.Newtype.unwrap
                                                              x1
                                                          ).maxGeneralDisbursement
                                                        )
                                                    )
                                                )
                                                ( Prelude.(==)
                                                    ( (Data.Newtype.unwrap x0).maxTripDisbursement
                                                    )
                                                    ( (Data.Newtype.unwrap x1).maxTripDisbursement
                                                    )
                                                )
                                            )
                                            ( Prelude.(==)
                                                ( (Data.Newtype.unwrap x0).agentDisbursementPercent
                                                )
                                                ( (Data.Newtype.unwrap x1).agentDisbursementPercent
                                                )
                                            )
                                        )
                                        ( Prelude.(==)
                                            ( (Data.Newtype.unwrap x0).proposalTallyEndOffset
                                            )
                                            ( (Data.Newtype.unwrap x1).proposalTallyEndOffset
                                            )
                                        )
                                    )
                                    ( Prelude.(==)
                                        ((Data.Newtype.unwrap x0).tallyNft)
                                        ((Data.Newtype.unwrap x1).tallyNft)
                                    )
                                )
                                ( Prelude.(==)
                                    ( (Data.Newtype.unwrap x0).voteCurrencySymbol
                                    )
                                    ( (Data.Newtype.unwrap x1).voteCurrencySymbol
                                    )
                                )
                            )
                            ( Prelude.(==)
                                ((Data.Newtype.unwrap x0).voteTokenName)
                                ((Data.Newtype.unwrap x1).voteTokenName)
                            )
                        )
                        ( Prelude.(==) ((Data.Newtype.unwrap x0).voteNft)
                            ((Data.Newtype.unwrap x1).voteNft)
                        )
                    )
                    ( Prelude.(==)
                        ((Data.Newtype.unwrap x0).voteFungibleCurrencySymbol)
                        ((Data.Newtype.unwrap x1).voteFungibleCurrencySymbol)
                    )
                )
                ( Prelude.(==) ((Data.Newtype.unwrap x0).voteFungibleTokenName)
                    ((Data.Newtype.unwrap x1).voteFungibleTokenName)
                )
            )
            ( Prelude.(==) ((Data.Newtype.unwrap x0).fungibleVotePercent)
                ((Data.Newtype.unwrap x1).fungibleVotePercent)
            )
        )
    )

instance ToData DynamicConfigDatum where
  toData =
    ( \x0 -> PlutusData.List
        ( [ toData ((Data.Newtype.unwrap x0).tallyValidator)
          , toData
              ((Data.Newtype.unwrap x0).treasuryValidator)
          , toData
              ((Data.Newtype.unwrap x0).configurationValidator)
          , toData ((Data.Newtype.unwrap x0).voteValidator)
          , toData
              ((Data.Newtype.unwrap x0).upgradeMajorityPercent)
          , toData
              ((Data.Newtype.unwrap x0).upgradeRelativeMajorityPercent)
          , toData
              ((Data.Newtype.unwrap x0).generalMajorityPercent)
          , toData
              ((Data.Newtype.unwrap x0).generalRelativeMajorityPercent)
          , toData
              ((Data.Newtype.unwrap x0).tripMajorityPercent)
          , toData
              ((Data.Newtype.unwrap x0).tripRelativeMajorityPercent)
          , toData ((Data.Newtype.unwrap x0).totalVotes)
          , toData
              ((Data.Newtype.unwrap x0).maxGeneralDisbursement)
          , toData
              ((Data.Newtype.unwrap x0).maxTripDisbursement)
          , toData
              ((Data.Newtype.unwrap x0).agentDisbursementPercent)
          , toData
              ((Data.Newtype.unwrap x0).proposalTallyEndOffset)
          , toData ((Data.Newtype.unwrap x0).tallyNft)
          , toData
              ((Data.Newtype.unwrap x0).voteCurrencySymbol)
          , toData ((Data.Newtype.unwrap x0).voteTokenName)
          , toData ((Data.Newtype.unwrap x0).voteNft)
          , toData
              ((Data.Newtype.unwrap x0).voteFungibleCurrencySymbol)
          , toData
              ((Data.Newtype.unwrap x0).voteFungibleTokenName)
          , toData
              ((Data.Newtype.unwrap x0).fungibleVotePercent)
          ]
        )
    )

instance FromData DynamicConfigDatum where
  fromData =
    ( \x0 -> LambdaBuffers.Runtime.Plutus.casePlutusData
        ((\x1 -> (\x2 -> Data.Maybe.Nothing)))
        ( ( \x3 -> case x3 of
              [ x4
              , x5
              , x6
              , x7
              , x8
              , x9
              , x10
              , x11
              , x12
              , x13
              , x14
              , x15
              , x16
              , x17
              , x18
              , x19
              , x20
              , x21
              , x22
              , x23
              , x24
              , x25
              ] -> Prelude.(>>=) (fromData (x4))
                ( ( \x26 -> Prelude.(>>=) (fromData (x5))
                      ( ( \x27 -> Prelude.(>>=)
                            (fromData (x6))
                            ( ( \x28 -> Prelude.(>>=)
                                  (fromData (x7))
                                  ( ( \x29 -> Prelude.(>>=)
                                        (fromData (x8))
                                        ( ( \x30 -> Prelude.(>>=)
                                              ( fromData
                                                  (x9)
                                              )
                                              ( ( \x31 -> Prelude.(>>=)
                                                    ( fromData
                                                        (x10)
                                                    )
                                                    ( ( \x32 -> Prelude.(>>=)
                                                          ( fromData
                                                              (x11)
                                                          )
                                                          ( ( \x33 ->
                                                                Prelude.(>>=)
                                                                  ( fromData
                                                                      (x12)
                                                                  )
                                                                  ( ( \x34 ->
                                                                        Prelude.(>>=)
                                                                          ( fromData
                                                                              ( x13
                                                                              )
                                                                          )
                                                                          ( ( \x35 ->
                                                                                Prelude.(>>=)
                                                                                  ( fromData
                                                                                      ( x14
                                                                                      )
                                                                                  )
                                                                                  ( ( \x36 ->
                                                                                        Prelude.(>>=)
                                                                                          ( fromData
                                                                                              ( x15
                                                                                              )
                                                                                          )
                                                                                          ( ( \x37 ->
                                                                                                Prelude.(>>=)
                                                                                                  ( fromData
                                                                                                      ( x16
                                                                                                      )
                                                                                                  )
                                                                                                  ( ( \x38 ->
                                                                                                        Prelude.(>>=)
                                                                                                          ( fromData
                                                                                                              ( x17
                                                                                                              )
                                                                                                          )
                                                                                                          ( ( \x39 ->
                                                                                                                Prelude.(>>=)
                                                                                                                  ( fromData
                                                                                                                      ( x18
                                                                                                                      )
                                                                                                                  )
                                                                                                                  ( ( \x40 ->
                                                                                                                        Prelude.(>>=)
                                                                                                                          ( fromData
                                                                                                                              ( x19
                                                                                                                              )
                                                                                                                          )
                                                                                                                          ( ( \x41 ->
                                                                                                                                Prelude.(>>=)
                                                                                                                                  ( fromData
                                                                                                                                      ( x20
                                                                                                                                      )
                                                                                                                                  )
                                                                                                                                  ( ( \x42 ->
                                                                                                                                        Prelude.(>>=)
                                                                                                                                          ( fromData
                                                                                                                                              ( x21
                                                                                                                                              )
                                                                                                                                          )
                                                                                                                                          ( ( \x43 ->
                                                                                                                                                Prelude.(>>=)
                                                                                                                                                  ( fromData
                                                                                                                                                      ( x22
                                                                                                                                                      )
                                                                                                                                                  )
                                                                                                                                                  ( ( \x44 ->
                                                                                                                                                        Prelude.(>>=)
                                                                                                                                                          ( fromData
                                                                                                                                                              ( x23
                                                                                                                                                              )
                                                                                                                                                          )
                                                                                                                                                          ( ( \x45 ->
                                                                                                                                                                Prelude.(>>=)
                                                                                                                                                                  ( fromData
                                                                                                                                                                      ( x24
                                                                                                                                                                      )
                                                                                                                                                                  )
                                                                                                                                                                  ( ( \x46 ->
                                                                                                                                                                        Prelude.(>>=)
                                                                                                                                                                          ( fromData
                                                                                                                                                                              ( x25
                                                                                                                                                                              )
                                                                                                                                                                          )
                                                                                                                                                                          ( ( \x47 ->
                                                                                                                                                                                Data.Maybe.Just
                                                                                                                                                                                  ( DynamicConfigDatum
                                                                                                                                                                                      { tallyValidator:
                                                                                                                                                                                          x26
                                                                                                                                                                                      , treasuryValidator:
                                                                                                                                                                                          x27
                                                                                                                                                                                      , configurationValidator:
                                                                                                                                                                                          x28
                                                                                                                                                                                      , voteValidator:
                                                                                                                                                                                          x29
                                                                                                                                                                                      , upgradeMajorityPercent:
                                                                                                                                                                                          x30
                                                                                                                                                                                      , upgradeRelativeMajorityPercent:
                                                                                                                                                                                          x31
                                                                                                                                                                                      , generalMajorityPercent:
                                                                                                                                                                                          x32
                                                                                                                                                                                      , generalRelativeMajorityPercent:
                                                                                                                                                                                          x33
                                                                                                                                                                                      , tripMajorityPercent:
                                                                                                                                                                                          x34
                                                                                                                                                                                      , tripRelativeMajorityPercent:
                                                                                                                                                                                          x35
                                                                                                                                                                                      , totalVotes:
                                                                                                                                                                                          x36
                                                                                                                                                                                      , maxGeneralDisbursement:
                                                                                                                                                                                          x37
                                                                                                                                                                                      , maxTripDisbursement:
                                                                                                                                                                                          x38
                                                                                                                                                                                      , agentDisbursementPercent:
                                                                                                                                                                                          x39
                                                                                                                                                                                      , proposalTallyEndOffset:
                                                                                                                                                                                          x40
                                                                                                                                                                                      , tallyNft:
                                                                                                                                                                                          x41
                                                                                                                                                                                      , voteCurrencySymbol:
                                                                                                                                                                                          x42
                                                                                                                                                                                      , voteTokenName:
                                                                                                                                                                                          x43
                                                                                                                                                                                      , voteNft:
                                                                                                                                                                                          x44
                                                                                                                                                                                      , voteFungibleCurrencySymbol:
                                                                                                                                                                                          x45
                                                                                                                                                                                      , voteFungibleTokenName:
                                                                                                                                                                                          x46
                                                                                                                                                                                      , fungibleVotePercent:
                                                                                                                                                                                          x47
                                                                                                                                                                                      }
                                                                                                                                                                                  )
                                                                                                                                                                            )
                                                                                                                                                                          )
                                                                                                                                                                    )
                                                                                                                                                                  )
                                                                                                                                                            )
                                                                                                                                                          )
                                                                                                                                                    )
                                                                                                                                                  )
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                            )
                                                                                                                          )
                                                                                                                    )
                                                                                                                  )
                                                                                                            )
                                                                                                          )
                                                                                                    )
                                                                                                  )
                                                                                            )
                                                                                          )
                                                                                    )
                                                                                  )
                                                                            )
                                                                          )
                                                                    )
                                                                  )
                                                            )
                                                          )
                                                      )
                                                    )
                                                )
                                              )
                                          )
                                        )
                                    )
                                  )
                              )
                            )
                        )
                      )
                  )
                )
              x48 -> Data.Maybe.Nothing
          )
        )
        ((\x49 -> Data.Maybe.Nothing))
        ((\x50 -> Data.Maybe.Nothing))
        (x0)
    )
