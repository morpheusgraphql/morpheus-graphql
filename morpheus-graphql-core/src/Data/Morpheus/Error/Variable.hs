{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Error.Variable
  ( uninitializedVariable,
    incompatibleVariableType,
  )
where

import Data.Morpheus.Error.Utils (validationErrorMessage)
import Data.Morpheus.Types.Internal.AST
  ( GQLErrors,
    Ref (..),
    TypeRef,
    ValidationError,
    Variable (..),
    msg,
  )
import Data.Semigroup ((<>))
import Prelude (($), Maybe (..))

-- query M ( $v : String ) { a(p:$v) } -> "Variable \"$v\" of type \"String\" used in position expecting type \"LANGUAGE\"."
incompatibleVariableType :: Ref -> Variable s -> TypeRef -> ValidationError
incompatibleVariableType
  (Ref variableName argPosition)
  Variable {variableType}
  argumentType =
    validationErrorMessage (Just argPosition) text
    where
      text =
        "Variable "
          <> msg ("$" <> variableName)
          <> " of type "
          <> msg variableType
          <> " used in position expecting type "
          <> msg argumentType
          <> "."

uninitializedVariable :: Variable s -> ValidationError
uninitializedVariable Variable {variableName, variableType, variablePosition} =
  validationErrorMessage
    (Just variablePosition)
    $ "Variable "
      <> msg ("$" <> variableName)
      <> " of required type "
      <> msg variableType
      <> " was not provided."
