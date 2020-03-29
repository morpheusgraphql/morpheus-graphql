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
  , UnionSelection
  , ValidSelection
  , Selection(..)
  , RawSelection
  , Fragments
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
  , UnionTag(..)
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
                                                ( Key
                                                , Position
                                                , Ref(..)
                                                , Name
                                                , VALID
                                                , RAW
                                                , Stage
                                                , OperationType(..)
                                                , GQLError(..)
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
                                                , Argument(..)
                                                )
import           Data.Morpheus.Types.Internal.AST.Value
                                                ( Variable(..)
                                                , ResolvedValue
                                                )
import          Data.Morpheus.Types.Internal.AST.SelectionMap
                                                ( SelectionMap )
import          Data.Morpheus.Types.Internal.AST.OrderedMap
                                                ( OrderedMap )
import          Data.Morpheus.Types.Internal.Operation
                                                ( KeyOf(..) )
import          Data.Morpheus.Error.NameCollision
                                                ( NameCollision(..) )

data Fragment = Fragment
  { fragmentName      :: Name
  , fragmentType      :: Name
  , fragmentPosition  :: Position
  , fragmentSelection :: RawSelectionSet
  } deriving ( Show, Lift )

instance NameCollision Fragment where
  nameCollision _ Fragment { fragmentName , fragmentPosition } = GQLError
    { message   = "There can be only one fragment named \"" <> fragmentName <> "\"."
    , locations = [fragmentPosition]
    }

instance KeyOf Fragment where 
  keyOf = fragmentName

type Fragments = OrderedMap Fragment

type RawArgument = Argument RAW

type ValidArgument = Argument VALID

type Arguments a = OrderedMap (Argument a)

type RawArguments = Arguments RAW

type ValidArguments = Arguments VALID

data SelectionContent (valid :: Stage) where
  SelectionField :: SelectionContent valid
  SelectionSet   :: SelectionSet valid -> SelectionContent valid
  UnionSelection :: UnionSelection -> SelectionContent VALID

deriving instance Show (SelectionContent a)
deriving instance Lift (SelectionContent a)

type RawSelectionRec = SelectionContent RAW
type ValidSelectionRec = SelectionContent VALID

data UnionTag = UnionTag {
  unionTagName :: Name,
  unionTagSelection :: SelectionSet VALID
} deriving (Show, Lift)

instance KeyOf UnionTag where
  keyOf = unionTagName

instance NameCollision UnionTag where
  -- TODO: real error
  nameCollision _ UnionTag { unionTagName } = GQLError 
    { message   = "There can be only one Union named \"" <> unionTagName <> "\"."
    , locations = []
    }

type UnionSelection = SelectionMap UnionTag

type SelectionSet s = SelectionMap  (Selection s)

type RawSelectionSet = SelectionSet RAW
type ValidSelectionSet = SelectionSet VALID


data Selection (s :: Stage) where
    Selection ::
      { selectionName       :: Name
      , selectionArguments  :: Arguments s
      , selectionPosition   :: Position
      , selectionAlias      :: Maybe Key
      , selectionContent    :: SelectionContent s
      } -> Selection s
    InlineFragment :: Fragment -> Selection RAW
    Spread :: Ref -> Selection RAW

instance KeyOf (Selection s) where
  keyOf Selection { selectionName } = selectionName
  keyOf (InlineFragment fr) = fragmentName fr

instance NameCollision (Selection s) where
  -- TODO: real error
  nameCollision _ Selection { selectionName } = GQLError 
    { message   = "There can be only one Selection named \"" <> selectionName <> "\"."
    , locations = []
    }

deriving instance Show (Selection a)
deriving instance Lift (Selection a)

type RawSelection = Selection RAW
type ValidSelection = Selection VALID

type DefaultValue = Maybe ResolvedValue

type Variables s = OrderedMap (Variable s)

type VariableDefinitions = Variables RAW

type ValidVariables = Variables VALID

data Operation (s:: Stage) = Operation
  { operationName      :: Maybe Key
  , operationType      :: OperationType
  , operationArguments :: Variables s
  , operationSelection :: SelectionSet s
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

