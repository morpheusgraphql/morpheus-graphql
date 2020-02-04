{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE DeriveLift         #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleInstances  #-}


module Data.Morpheus.Types.Internal.AST.Selection
  ( Argument(..)
  , Arguments
  , SelectionSet
  , SelectionContent(..)
  , ValidSelection
  , Selection(..)
  , RawSelection
  , FragmentLib
  , RawArguments
  , RawSelectionSet
  , Fragment(..)
  , RawArgument
  , ValidSelectionSet
  , ValidArgument
  , ValidArguments
  , RawSelectionRec
  , ValidSelectionRec
  , Operation(..)
  , Variable(..)
  , ValidOperation
  , RawOperation
  , VariableDefinitions
  , ValidVariables
  , DefaultValue
  , getOperationName
  , getOperationDataType
  , getOperationObject
  )
where


import           Data.Maybe                     ( fromMaybe )
import           Data.Semigroup                 ( (<>) )
import           Language.Haskell.TH.Syntax     ( Lift(..) )

-- MORPHEUS
import           Data.Morpheus.Error.Mutation   ( mutationIsNotDefined )
import           Data.Morpheus.Error.Subscription
                                                ( subscriptionIsNotDefined )
import           Data.Morpheus.Types.Internal.AST.Base
                                                ( Collection
                                                , Key
                                                , Position
                                                , Ref(..)
                                                , Name
                                                , VALID
                                                , RAW
                                                , Stage
                                                , OperationType(..)
                                                )
import           Data.Morpheus.Types.Internal.Resolving.Core
                                                ( Validation
                                                , Failure(..)
                                                )
import           Data.Morpheus.Types.Internal.AST.Data
                                                ( Schema(..)
                                                , TypeDefinition(..)
                                                , TypeContent(..)
                                                , FieldsDefinition
                                                )
import           Data.Morpheus.Types.Internal.AST.Value
                                                ( Value
                                                , Variable(..)
                                                , ResolvedValue
                                                )


data Fragment = Fragment
  { fragmentType      :: Key
  , fragmentPosition  :: Position
  , fragmentSelection :: RawSelectionSet
  } deriving (Show,Lift)

type FragmentLib = [(Key, Fragment)]

data Argument (valid :: Stage) = Argument {
    argumentValue    :: Value valid
  , argumentPosition :: Position
  } deriving (Show,Lift)

type RawArgument = Argument RAW

type ValidArgument = Argument VALID

type Arguments a = Collection (Argument a)

type RawArguments = Arguments RAW

type ValidArguments = Collection ValidArgument

data SelectionContent (valid :: Stage) where
  SelectionField ::SelectionContent valid
  SelectionSet   ::SelectionSet valid -> SelectionContent valid
  UnionSelection ::UnionSelection -> SelectionContent VALID

deriving instance Show (SelectionContent a)
deriving instance Lift (SelectionContent a)

type RawSelectionRec = SelectionContent RAW
type ValidSelectionRec = SelectionContent VALID
type UnionSelection = Collection (SelectionSet VALID)
type SelectionSet a = Collection (Selection a)
type RawSelectionSet = Collection RawSelection
type ValidSelectionSet = Collection ValidSelection


data Selection (valid:: Stage) where
    Selection ::{
      selectionArguments :: Arguments valid
    , selectionPosition  :: Position
    , selectionAlias     :: Maybe Key
    , selectionContent   :: SelectionContent valid
    } -> Selection valid
    InlineFragment ::Fragment -> Selection RAW
    Spread ::Ref -> Selection RAW

deriving instance Show (Selection a)
deriving instance Lift (Selection a)

type RawSelection = Selection RAW
type ValidSelection = Selection VALID

type DefaultValue = Maybe ResolvedValue

type VariableDefinitions = Collection (Variable RAW)

type ValidVariables = Collection (Variable VALID)

data Operation (stage:: Stage) = Operation
  { operationName      :: Maybe Key
  , operationType      :: OperationType
  , operationArguments :: Collection (Variable stage)
  , operationSelection :: SelectionSet stage
  , operationPosition  :: Position
  } deriving (Show,Lift)

type RawOperation = Operation RAW

type ValidOperation = Operation VALID


getOperationName :: Maybe Key -> Key
getOperationName = fromMaybe "AnonymousOperation"

getOperationObject
  :: Operation a -> Schema -> Validation (Name, FieldsDefinition)
getOperationObject op lib = do
  dt <- getOperationDataType op lib
  case dt of
    TypeDefinition { typeContent = DataObject { objectFields }, typeName } -> pure (typeName, objectFields)
    TypeDefinition { typeName } ->
      failure
        $  "Type Mismatch: operation \""
        <> typeName
        <> "\" must be an Object"

getOperationDataType :: Operation a -> Schema -> Validation TypeDefinition
getOperationDataType Operation { operationType = Query } lib = pure (query lib)
getOperationDataType Operation { operationType = Mutation, operationPosition } lib
  = case mutation lib of
    Just x -> pure x
    Nothing       -> failure $ mutationIsNotDefined operationPosition
getOperationDataType Operation { operationType = Subscription, operationPosition } lib
  = case subscription lib of
    Just x -> pure x
    Nothing -> failure $ subscriptionIsNotDefined operationPosition

