{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "ctl-package-example"
, dependencies =
  [ "aeson"
  , "aff"
  , "aff-promise"
  , "arraybuffer"
  , "arraybuffer-types"
  , "arrays"
  , "b64"
  , "cardano-transaction-lib"
  , "control"
  , "datetime"
  , "effect"
  , "either"
  , "encoding"
  , "enums"
  , "exceptions"
  , "foldable-traversable"
  , "foreign"
  , "foreign-object"
  , "gen"
  , "js-bigints"
  , "lambda-buffers"
  , "maybe"
  , "monad-logger"
  , "mote"
  , "newtype"
  , "node-buffer"
  , "node-fs"
  , "ordered-collections"
  , "partial"
  , "posix-types"
  , "prelude"
  , "quickcheck"
  , "quickcheck-utf8"
  , "spec"
  , "strings"
  , "transformers"
  , "tuples"
  , "uint"
  , "unfoldable"
  ]
, packages = ./packages.dhall
, sources =
  [ "src/**/*.purs"
  , "exe/**/*.purs"
  , "test/**/*.purs"
  , ".spago/lambda-buffers/617ac9202cd9e3bc67ff85bdc3d7e2fd6ec2d13a/runtimes/purescript/lbr-prelude/src/**/*.purs"
  , ".spago/lambda-buffers/617ac9202cd9e3bc67ff85bdc3d7e2fd6ec2d13a/runtimes/purescript/lbr-plutus/**/*.purs"
  , "types/LambdaBuffers/**/*.purs"
  , "types/ScriptArguments/**/*.purs"
  ]
}
