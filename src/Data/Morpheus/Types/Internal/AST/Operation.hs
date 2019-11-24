{-# LANGUAGE DeriveLift         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}

module Data.Morpheus.Types.Internal.AST.Operation
  ( Operation(..)
  , Variable(..)
  , ValidOperation
  , RawOperation
  , VariableDefinitions
  , ValidVariables
  , DefaultValue
  , getOperationName
  , getOperationDataType
  )
where

import           Data.Maybe                     ( fromMaybe )
import           Language.Haskell.TH.Syntax     ( Lift(..) )

-- MORPHEUS
import           Data.Morpheus.Error.Mutation   ( mutationIsNotDefined )
import           Data.Morpheus.Error.Subscription
                                                ( subscriptionIsNotDefined )
import           Data.Morpheus.Types.Internal.AST.Selection
                                                ( Arguments
                                                , SelectionSet
                                                , RawSelectionSet
                                                )

import           Data.Morpheus.Types.Internal.Resolving.Core
                                                ( Validation ,Failure(..) )
import           Data.Morpheus.Types.Internal.AST.Base
                                                ( Collection
                                                , Key
                                                , Position
                                                )
import           Data.Morpheus.Types.Internal.AST.Data
                                                ( OperationType(..)
                                                , TypeWrapper
                                                , DataTypeLib(..)
                                                , DataObject
                                                )
import           Data.Morpheus.Types.Internal.AST.Value
                                                ( Value )

type DefaultValue = Maybe Value

type VariableDefinitions = Collection (Variable DefaultValue)

type ValidVariables = Collection (Variable Value)

type ValidOperation = Operation Arguments SelectionSet

type RawOperation = Operation VariableDefinitions RawSelectionSet

getOperationName :: Maybe Key -> Key
getOperationName = fromMaybe "AnonymousOperation"

data Operation args sel = Operation
  { operationName      :: Maybe Key
  , operationType      :: OperationType
  , operationArgs      :: args
  , operationSelection :: sel
  , operationPosition  :: Position
  } deriving (Show,Lift)

data Variable a = Variable
  { variableType         :: Key
  , isVariableRequired   :: Bool
  , variableTypeWrappers :: [TypeWrapper]
  , variablePosition     :: Position
  , variableValue        :: a
  } deriving (Show,Lift)

getOperationDataType :: Operation a b -> DataTypeLib -> Validation DataObject
getOperationDataType Operation { operationType = Query } lib =
  pure $ snd $ query lib
getOperationDataType Operation { operationType = Mutation, operationPosition } lib
  = case mutation lib of
    Just (_, mutation') -> pure mutation'
    Nothing             -> failure $ mutationIsNotDefined operationPosition
getOperationDataType Operation { operationType = Subscription, operationPosition } lib
  = case subscription lib of
    Just (_, subscription') -> pure subscription'
    Nothing                 -> failure $ subscriptionIsNotDefined operationPosition

