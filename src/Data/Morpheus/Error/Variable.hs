{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns    #-}

module Data.Morpheus.Error.Variable
  ( uninitializedVariable
  , incompatibleVariableType
  )
where

import           Data.Morpheus.Error.Utils      ( errorMessage )
import           Data.Morpheus.Types.Internal.AST
                                                ( GQLErrors
                                                , Ref(..)
                                                , Variable(..)
                                                , TypeRef
                                                )
import           Data.Semigroup                 ( (<>) )
import           Data.Morpheus.Rendering.RenderGQL
                                                ( RenderGQL(..) )

-- query M ( $v : String ) { a(p:$v) } -> "Variable \"$v\" of type \"String\" used in position expecting type \"LANGUAGE\"."
incompatibleVariableType :: Ref -> Variable s -> TypeRef -> GQLErrors
incompatibleVariableType 
  (Ref variableName argPosition) 
  Variable { variableType } 
  argumentType  =
  errorMessage argPosition text
 where
  text =
    "Variable \"$"
      <> variableName
      <> "\" of type \""
      <> render variableType
      <> "\" used in position expecting type \""
      <> render argumentType
      <> "\"."

uninitializedVariable :: Variable s -> GQLErrors
uninitializedVariable Variable { variableName, variableType, variablePosition} = 
  errorMessage 
    variablePosition 
    $ "Variable \"$" <> variableName 
    <> "\" of required type \""
    <> render variableType <> "\" was not provided."