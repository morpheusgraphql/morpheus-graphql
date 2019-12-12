{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveLift        #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleInstances  #-}


module Data.Morpheus.Types.Internal.AST.Selection
  ( Argument(..)
  , Arguments
  , SelectionSet
  , SelectionRec(..)
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
                                                , RESOLVED
                                                )
import           Data.Morpheus.Types.Internal.Resolving.Core
                                                ( Validation
                                                , Failure(..)
                                                )
import           Data.Morpheus.Types.Internal.AST.Data
                                                ( OperationType(..)
                                                , DataTypeLib(..)
                                                , DataType(..)
                                                , DataTypeContent(..)
                                                , DataObject
                                                )
import           Data.Morpheus.Types.Internal.AST.Value
                                                ( ValidValue
                                                , Value
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

data SelectionRec (valid :: Stage) where
  SelectionField ::SelectionRec valid
  SelectionSet   ::SelectionSet valid -> SelectionRec valid
  UnionSelection ::UnionSelection -> SelectionRec VALID

instance Show (SelectionRec a) where

instance Lift (SelectionRec a) where
  lift (SelectionSet   s) = [| SelectionSet s |]
  lift (UnionSelection s) = [| UnionSelection s |]
  lift SelectionField     = [| SelectionField |]

type RawSelectionRec = SelectionRec RAW
type ValidSelectionRec = SelectionRec VALID
type UnionSelection = Collection (SelectionSet VALID)
type SelectionSet a = Collection (Selection a)
type RawSelectionSet = Collection RawSelection
type ValidSelectionSet = Collection ValidSelection


data Selection (valid:: Stage) where
    Selection ::{
      selectionArguments :: Arguments valid
    , selectionPosition  :: Position
    , selectionAlias     :: Maybe Key
    , selectionRec       :: SelectionRec valid
    } -> Selection valid
    InlineFragment ::Fragment -> Selection RAW
    Spread ::Ref -> Selection RAW

instance Show (Selection a) where

instance Lift (Selection a) where
  lift (Selection args pos alias cont) = [| Selection args pos alias cont |]
  lift (InlineFragment x             ) = [| InlineFragment x |]
  lift (Spread         x             ) = [| Spread x |]

type RawSelection = Selection RAW
type ValidSelection = Selection VALID

type DefaultValue = Maybe ResolvedValue

type VariableDefinitions = Collection (Variable DefaultValue)

type ValidVariables = Collection (Variable ValidValue)

data Operation args (valid:: Stage) = Operation
  { operationName      :: Maybe Key
  , operationType      :: OperationType
  , operationArgs      :: args
  , operationSelection :: SelectionSet valid
  , operationPosition  :: Position
  } deriving (Show,Lift)

type RawOperation = Operation VariableDefinitions RAW

type ValidOperation = Operation ValidArguments VALID


getOperationName :: Maybe Key -> Key
getOperationName = fromMaybe "AnonymousOperation"

getOperationObject
  :: Operation a b -> DataTypeLib -> Validation (Name, DataObject)
getOperationObject op lib = do
  dt <- getOperationDataType op lib
  case dt of
    DataType { typeContent = DataObject x, typeName } -> pure (typeName, x)
    DataType { typeName } ->
      failure
        $  "Type Mismatch: operation \""
        <> typeName
        <> "\" must be an Object"

getOperationDataType :: Operation a b -> DataTypeLib -> Validation DataType
getOperationDataType Operation { operationType = Query } lib =
  pure $ snd $ query lib
getOperationDataType Operation { operationType = Mutation, operationPosition } lib
  = case mutation lib of
    Just (_, mutation') -> pure mutation'
    Nothing             -> failure $ mutationIsNotDefined operationPosition
getOperationDataType Operation { operationType = Subscription, operationPosition } lib
  = case subscription lib of
    Just (_, subscription') -> pure subscription'
    Nothing -> failure $ subscriptionIsNotDefined operationPosition

