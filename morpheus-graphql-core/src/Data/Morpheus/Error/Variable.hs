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
    Ref (..),
    TypeRef,
    ValidationError,
    Variable (..),
    at,
    msgValidation,
  )
import Relude

-- query M ( $v : String ) { a(p:$v) } -> "Variable \"$v\" of type \"String\" used in position expecting type \"LANGUAGE\"."
incompatibleVariableType :: Ref FieldName -> Variable s -> TypeRef -> ValidationError
incompatibleVariableType
  (Ref variableName argPosition)
  Variable {variableType}
  argumentType =
    ( "Variable "
        <> msgValidation ("$" <> variableName)
        <> " of type "
        <> msgValidation variableType
        <> " used in position expecting type "
        <> msgValidation argumentType
        <> "."
    )
      `at` argPosition

uninitializedVariable :: Variable s -> ValidationError
uninitializedVariable Variable {variableName, variableType, variablePosition} =
  ( "Variable "
      <> msgValidation ("$" <> variableName)
      <> " of required type "
      <> msgValidation variableType
      <> " was not provided."
  )
    `at` variablePosition
