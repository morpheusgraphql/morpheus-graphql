{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Variable
  ( uninitializedVariable
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

uninitializedVariable :: Position -> Name -> Name -> GQLErrors
uninitializedVariable position' type' key' = errorMessage position' text
 where
  text = "Variable \"$" <> key' 
    <> "\" of required type \""
    <> type' <> "!\" was not provided."