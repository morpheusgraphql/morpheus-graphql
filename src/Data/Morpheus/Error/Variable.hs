{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns    #-}

module Data.Morpheus.Error.Variable
  ( uninitializedVariable
  , incompatibleVariableType
  )
where

import           Data.Morpheus.Error.Utils      ( errorMessage )
import           Data.Morpheus.Types.Internal.AST
                                                ( Position
                                                , GQLErrors
                                                , Name
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

uninitializedVariable :: Position -> Name -> Name -> GQLErrors
uninitializedVariable position' type' key' = errorMessage position' text
 where
  text = "Variable \"$" <> key' 
    <> "\" of required type \""
    <> type' <> "!\" was not provided."