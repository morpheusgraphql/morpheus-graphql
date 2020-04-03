{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}

module Data.Morpheus.Error.ErrorClass
  ( MissingRequired(..)
  )
  where

import           Data.Morpheus.Types.Internal.AST
                                                ( GQLError(..)
                                                , Ref(..)
                                                , VariableDefinitions
                                                , ValidationContext(..)
                                                , getOperationName
                                                )

class MissingRequired c where 
  missingRequired :: ValidationContext -> Ref -> c -> GQLError

instance MissingRequired (VariableDefinitions s) where
  missingRequired 
    ValidationContext { operationName } 
    Ref { refName , refPosition } _ 
    = GQLError 
      { message 
        = "Variable \"" <> refName
        <> "\" is not defined by operation \""
        <> getOperationName operationName <> "\"."
      , locations = [refPosition]
      }