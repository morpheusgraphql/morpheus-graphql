{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Variable
  ( uninitializedVariable
  , unusedVariables
  , incompatibleVariableType
  )
where

import           Data.Morpheus.Error.Utils      ( errorMessage )
import           Data.Morpheus.Types.Internal.AST.Base
                                                ( Ref(..)
                                                , Position
                                                , GQLError(..)
                                                , GQLErrors
                                                , Name
                                                )
import           Data.Semigroup                 ( (<>) )

-- query M ( $v : String ) { a(p:$v) } -> "Variable \"$v\" of type \"String\" used in position expecting type \"LANGUAGE\"."
incompatibleVariableType :: Name -> Name -> Name -> Position -> GQLErrors
incompatibleVariableType variableName variableType argType argPosition =
  errorMessage argPosition text
 where
  text =
    "Variable \"$"
      <> variableName
      <> "\" of type \""
      <> variableType
      <> "\" used in position expecting type \""
      <> argType
      <> "\"."

-- query M ( $v : String ) { a } -> "Variable \"$bla\" is never used in operation \"MyMutation\".",
unusedVariables :: Name -> [Ref] -> GQLErrors
unusedVariables operator' = map keyToError
 where
  keyToError (Ref key' position') =
    GQLError { message = text key', locations = [position'] }
  ---------------------------------
  text key' = "Variable \"$" <> key' 
    <> "\" is never used in operation \""
    <> operator' <> "\"."

uninitializedVariable :: Position -> Name -> Name -> GQLErrors
uninitializedVariable position' type' key' = errorMessage position' text
 where
  text = "Variable \"$" <> key' 
    <> "\" of required type \""
    <> type' <> "!\" was not provided."