{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Error.Variable
  ( uninitializedVariable,
    incompatibleVariableType,
  )
where

import Data.Morpheus.Types.Internal.AST
  ( FieldName,
    GQLError,
    Ref (..),
    TypeRef,
    Variable (..),
    at,
    msg,
  )
import Relude

-- query M ( $v : String ) { a(p:$v) } -> "Variable \"$v\" of type \"String\" used in position expecting type \"LANGUAGE\"."
incompatibleVariableType :: Ref FieldName -> Variable s -> TypeRef -> GQLError
incompatibleVariableType
  (Ref variableName argPosition)
  Variable {variableType}
  argumentType =
    ( "Variable "
        <> msg ("$" <> variableName)
        <> " of type "
        <> msg variableType
        <> " used in position expecting type "
        <> msg argumentType
        <> "."
    )
      `at` argPosition

uninitializedVariable :: Variable s -> GQLError
uninitializedVariable Variable {variableName, variableType, variablePosition} =
  ( "Variable "
      <> msg ("$" <> variableName)
      <> " of required type "
      <> msg variableType
      <> " was not provided."
  )
    `at` variablePosition
