{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Variable
  ( variableIsNotDefined
  , fieldTypeMismatch
  , invalidEnumOption
  , variableValidationError
  ) where

import           Data.Morpheus.Error.Utils    (errorMessage)
import           Data.Morpheus.Types.Error    (GQLErrors, MetaError (..))
import           Data.Morpheus.Types.MetaInfo (MetaInfo (..))
import qualified Data.Text                    as T (Text, concat, pack)

{-|
VARIABLES:

Variable -> Error (position Query Head)
  data E = EN | DE
  query M ( $v : E ){...}

case type does not exists
  query Q ($a: D) ->  "Unknown type \"D\"."


case String
  - { "v" : "EN" }  ->  no error converts as enum

case type mismatch
  - { "v": { "a": "v1" ... } } -> "Variable \"$v\" got invalid value { "a": "v1" ... } ; Expected type LANGUAGE."
  - { "v" : "v1" }  -> "Variable \"$v\" got invalid value \"v1\"; Expected type LANGUAGE."
  - { "v": 1  }        "Variable \"$v\" got invalid value 1; Expected type LANGUAGE."

case unused variable
  - query M ( $v : String ) { a } -> "Variable \"$bla\" is never used in operation \"MyMutation\".",

case variable does not match to argument type
  - query M ( $v : String ) { a(p:$v) } -> "Variable \"$v\" of type \"String\" used in position expecting type \"LANGUAGE\"."

|-}
variableValidationError :: MetaError -> GQLErrors
variableValidationError (TypeMismatch meta isType shouldType) = fieldTypeMismatch meta isType shouldType

variableIsNotDefined :: MetaInfo -> GQLErrors
variableIsNotDefined meta = errorMessage (position meta) text
  where
    text = T.concat ["Variable \"", key meta, "\" is not defined by operation \"", typeName meta, "\"."]

-- TODO: delete it GQL has no this kind of error
invalidEnumOption :: MetaInfo -> GQLErrors
invalidEnumOption meta = errorMessage (position meta) text
  where
    text = T.concat ["Expected type ", typeName meta, " found ", key meta, "."]

-- TODO: delete it GQL has no this kind of error
fieldTypeMismatch :: MetaInfo -> T.Text -> T.Text -> GQLErrors
fieldTypeMismatch meta isType should = errorMessage (position meta) text
  where
    text =
      T.concat
        [ "Variable field\""
        , key meta
        , "\"on type \""
        , typeName meta
        , "\" has a type \""
        , T.pack $ show isType
        , "\" but should have \""
        , should
        , "\"."
        ]
