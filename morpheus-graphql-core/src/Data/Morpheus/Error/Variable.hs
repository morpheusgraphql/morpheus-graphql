{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Variable
  ( uninitializedVariable,
    incompatibleVariableType,
  )
where

import Data.Morpheus.Error.Utils (errorMessage)
import Data.Morpheus.Types.Internal.AST
  ( GQLErrors,
    Ref (..),
    TypeRef,
    Variable (..),
    msg,
  )
import Data.Semigroup ((<>))

-- query M ( $v : String ) { a(p:$v) } -> "Variable \"$v\" of type \"String\" used in position expecting type \"LANGUAGE\"."
incompatibleVariableType :: Ref -> Variable s -> TypeRef -> GQLErrors
incompatibleVariableType
  (Ref variableName argPosition)
  Variable {variableType}
  argumentType =
    errorMessage argPosition text
    where
      text =
        "Variable "
          <> msg ("$" <> variableName)
          <> " of type "
          <> msg variableType
          <> " used in position expecting type "
          <> msg argumentType
          <> "."

uninitializedVariable :: Variable s -> GQLErrors
uninitializedVariable Variable {variableName, variableType, variablePosition} =
  errorMessage
    variablePosition
    $ "Variable "
      <> msg ("$" <> variableName)
      <> " of required type "
      <> msg variableType
      <> " was not provided."
