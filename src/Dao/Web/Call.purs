module Dao.Web.Call
  ( contractCallTwoArgs
  , contractCallOneArg
  , contractCallNoArgs
  ) where

import Contract.Address (getNetworkId)
import Contract.Monad
  ( Contract
  , ContractEnv
  , liftContractE
  , runContractInEnv
  ) as Ctl
import Contract.Prelude (Effect, bind, ($))
import Control.Promise (Promise, fromAff)
import Dao.Web.Conversion
  ( class ConvertJsToPs
  , class ConvertPsToJs
  , runConvertJsToPs
  , runConvertPsToJs
  )
import Effect.Aff.Compat (EffectFn1, EffectFn2, mkEffectFn1, mkEffectFn2)

contractCallTwoArgs ::
  forall resJs resPurs arg1Js arg1Purs arg2Js arg2Purs.
  ConvertPsToJs resJs resPurs =>
  ConvertJsToPs arg1Js arg1Purs =>
  ConvertJsToPs arg2Js arg2Purs =>
  Ctl.ContractEnv ->
  (arg1Purs -> arg2Purs -> Ctl.Contract resPurs) ->
  EffectFn2 arg1Js arg2Js (Promise resJs)
contractCallTwoArgs env contractCall =
  mkEffectFn2 $ \arg1Js arg2Js -> do
    fromAff $ Ctl.runContractInEnv env $ do
      arg1Purs <- convertJsToPsContract arg1Js
      arg2Purs <- convertJsToPsContract arg2Js
      res <- contractCall arg1Purs arg2Purs
      convertPsToJsContract res

contractCallOneArg ::
  forall resJs resPurs arg1Js arg1Purs.
  ConvertPsToJs resJs resPurs =>
  ConvertJsToPs arg1Js arg1Purs =>
  Ctl.ContractEnv ->
  (arg1Purs -> Ctl.Contract resPurs) ->
  EffectFn1 arg1Js (Promise resJs)
contractCallOneArg env contractCall =
  mkEffectFn1 $ \arg1Js -> do
    fromAff $ Ctl.runContractInEnv env $ do
      arg1Purs <- convertJsToPsContract arg1Js
      res <- contractCall arg1Purs
      convertPsToJsContract res

contractCallNoArgs ::
  forall resJs resPurs.
  (ConvertPsToJs resJs resPurs) =>
  Ctl.ContractEnv ->
  Ctl.Contract resPurs ->
  Effect (Promise resJs)
contractCallNoArgs env contractCall = do
  fromAff $ Ctl.runContractInEnv env $ do
    res <- contractCall
    convertPsToJsContract res

convertPsToJsContract ::
  forall js ps. (ConvertPsToJs js ps) => ps -> Ctl.Contract js
convertPsToJsContract ps = do
  networkId <- getNetworkId
  Ctl.liftContractE $ runConvertPsToJs ps networkId

convertJsToPsContract ::
  forall js ps. (ConvertJsToPs js ps) => js -> Ctl.Contract ps
convertJsToPsContract js = do
  networkId <- getNetworkId
  Ctl.liftContractE $ runConvertJsToPs js networkId
