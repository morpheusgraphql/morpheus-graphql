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
  , ValueOrigin(..)
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
                                                )
import           Data.Morpheus.Types.Internal.Resolving.Core
                                                ( Validation
                                                , Failure(..)
                                                )
import           Data.Morpheus.Types.Internal.AST.Data
                                                ( OperationType(..)
                                                , TypeWrapper
                                                , DataTypeLib(..)
                                                , DataType(..)
                                                , DataTypeContent(..)
                                                , DataObject
                                                )
import           Data.Morpheus.Types.Internal.AST.Value
                                                ( Value )


type VALID = 'True
type RAW = 'False


data ValueOrigin
  = VARIABLE
  | INLINE
  deriving (Show, Lift)

data Fragment = Fragment
  { fragmentType      :: Key
  , fragmentPosition  :: Position
  , fragmentSelection :: RawSelectionSet
  } deriving (Show,Lift)

type FragmentLib = [(Key, Fragment)]


data Argument (valid :: Bool) where
  VariableRef ::Ref -> Argument RAW
  Argument ::{
    argumentValue    :: Value
  , argumentOrigin   :: ValueOrigin
  , argumentPosition :: Position
  } -> Argument valid

instance Lift (Argument a) where
  lift (Argument v o p) = [| Argument v o p |]
  lift (VariableRef x ) = [| VariableRef x |]

type RawArgument = Argument RAW
type ValidArgument = Argument VALID

type Arguments a = Collection (Argument a)

type RawArguments = Collection RawArgument
type ValidArguments = Collection ValidArgument

instance Show (Argument a) where

data SelectionRec (valid :: Bool) where
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


data Selection (valid:: Bool) where
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


data Variable a = Variable
  { variableType         :: Key
  , isVariableRequired   :: Bool
  , variableTypeWrappers :: [TypeWrapper]
  , variablePosition     :: Position
  , variableValue        :: a
  } deriving (Show,Lift)

type DefaultValue = Maybe Value

type VariableDefinitions = Collection (Variable DefaultValue)

type ValidVariables = Collection (Variable Value)

data Operation args (valid:: Bool) = Operation
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

