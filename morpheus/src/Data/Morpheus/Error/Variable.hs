{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Variable
  ( variableIsNotDefined
  , unknownType
  , variableGotInvalidValue
  ) where

import           Data.Morpheus.Error.Utils    (errorMessage)
import           Data.Morpheus.Types.Error    (GQLErrors)
import           Data.Morpheus.Types.MetaInfo (MetaInfo (..), Position)
import           Data.Text                    (Text)
import qualified Data.Text                    as T (concat)

{-|
VARIABLES:

Variable -> Error (position Query Head)
  data E = EN | DE
  query M ( $v : E ){...}


query Q ($a: D) ->  "Unknown type \"D\"."

case String
  - { "v" : "EN" }  ->  no error converts as enum

case type mismatch
  - { "v": { "a": "v1" ... } } -> "Variable \"$v\" got invalid value { "a": "v1" ... } ; Expected type LANGUAGE."
  - { "v" : "v1" }  -> "Variable \"$v\" got invalid value \"v1\"; Expected type LANGUAGE."
  - { "v": 1  }        "Variable \"$v\" got invalid value 1; Expected type LANGUAGE."

TODO: unused variable
  - query M ( $v : String ) { a } -> "Variable \"$bla\" is never used in operation \"MyMutation\".",

TODO: variable does not match to argument type
  - query M ( $v : String ) { a(p:$v) } -> "Variable \"$v\" of type \"String\" used in position expecting type \"LANGUAGE\"."

|-}

variableGotInvalidValue :: Text -> Text -> Position -> GQLErrors
variableGotInvalidValue name' inputMessage' position' = errorMessage position' text
  where
    text = T.concat ["Variable \"$", name', "\" got invalid value; ", inputMessage']


unknownType :: Text -> Position -> GQLErrors
unknownType type' position' = errorMessage position' text
  where
    text = T.concat ["Unknown type \"", type', "\"."]

variableIsNotDefined :: MetaInfo -> GQLErrors
variableIsNotDefined meta = errorMessage (position meta) text
  where
    text = T.concat ["Variable \"", key meta, "\" is not defined by operation \"", typeName meta, "\"."]