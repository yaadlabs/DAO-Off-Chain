module Test.Data.Address (dummyAddress) where

import Contract.Address
  ( Address
  , addressFromBech32
  )
import Contract.Monad (Contract)

dummyAddress :: Contract Address
dummyAddress = addressFromBech32
  "addr1qy6uzzqfg4jf50v6cw0nn7h7kntt6ftp5uuys6hdfltf0v2cz4j5aeq52yuygu69dw7grgq7sjlxgfwclpnzap4p0wtqyccedn"
