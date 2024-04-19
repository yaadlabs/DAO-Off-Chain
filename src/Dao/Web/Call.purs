module Dao.Web.Call
  ( mkContractCall1
  , mkContractCall2
  , mkContractCall3
  )
  where

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
import Data.Function.Uncurried (Fn1, Fn2, Fn3, mkFn1, mkFn2, mkFn3)
import Effect.Aff.Compat (EffectFn1, EffectFn2, mkEffectFn1, mkEffectFn2)
import Effect.Unsafe (unsafePerformEffect)

-- | Create a function that calls a contract with only the ContractEnv as an argument.
mkContractCall1 ::
  forall resJs resPurs.
  ConvertPsToJs resJs resPurs =>
  Ctl.Contract resPurs ->
  Fn1 Ctl.ContractEnv (Promise resJs)
mkContractCall1 contractCall = mkFn1 \env -> do
    unsafePerformEffect $ fromAff $ Ctl.runContractInEnv env $ do
      res <- contractCall
      convertPsToJsContract res

-- | Create an uncurried function that calls a contract with the ContractEnv and another argument.
mkContractCall2 ::
  forall resJs resPurs argJs argPurs.
  ConvertPsToJs resJs resPurs =>
  ConvertJsToPs argJs argPurs =>
  (argPurs -> Ctl.Contract resPurs) ->
  Fn2 Ctl.ContractEnv argJs (Promise resJs)
mkContractCall2 contractCall = mkFn2 \env argJs -> do
    unsafePerformEffect $ fromAff $ Ctl.runContractInEnv env $ do
      argPurs <- convertJsToPsContract argJs
      res <- contractCall argPurs
      convertPsToJsContract res

-- | Create an uncurried function that calls a contract with the ContractEnv and 2 other arguments.
mkContractCall3 ::
  forall resJs resPurs arg1Js arg1Purs arg2Js arg2Purs.
  ConvertPsToJs resJs resPurs =>
  ConvertJsToPs arg1Js arg1Purs =>
  ConvertJsToPs arg2Js arg2Purs =>
  (arg1Purs -> arg2Purs -> Ctl.Contract resPurs) ->
  Fn3 Ctl.ContractEnv arg1Js arg2Js (Promise resJs)
mkContractCall3 contractCall = mkFn3 \env arg1Js arg2Js -> do
    unsafePerformEffect $ fromAff $ Ctl.runContractInEnv env $ do
      arg1Purs <- convertJsToPsContract arg1Js
      arg2Purs <- convertJsToPsContract arg2Js
      res <- contractCall arg1Purs arg2Purs
      convertPsToJsContract res

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
