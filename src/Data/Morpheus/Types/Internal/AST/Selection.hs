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
  , Selection(..)
  , SelectionContent(..)
  , SelectionSet
  , UnionTag(..)
  , UnionSelection
  , Fragment(..)
  , Fragments
  , Operation(..)
  , Variable(..)
  , VariableDefinitions
  , ValidVariables
  , DefaultValue
  , getOperationName
  , getOperationDataType
  , getOperationObject
  )
where


import           Data.Maybe                     ( fromMaybe , isJust )
import           Data.Semigroup                 ( (<>) )
import           Language.Haskell.TH.Syntax     ( Lift(..) )
import qualified Data.Text                  as  T

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
                                                , GQLErrors
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
import          Data.Morpheus.Types.Internal.AST.MergeSet
                                                ( MergeSet )
import          Data.Morpheus.Types.Internal.AST.OrderedMap
                                                ( OrderedMap )
import          Data.Morpheus.Types.Internal.Operation
                                                ( KeyOf(..)
                                                , Join(..)
                                                )
import          Data.Morpheus.Error.NameCollision
                                                ( NameCollision(..) )

data Fragment = Fragment
  { fragmentName      :: Name
  , fragmentType      :: Name
  , fragmentPosition  :: Position
  , fragmentSelection :: SelectionSet RAW
  } deriving ( Show, Eq, Lift)

instance NameCollision Fragment where
  nameCollision _ Fragment { fragmentName , fragmentPosition } = GQLError
    { message   = "There can be only one fragment named \"" <> fragmentName <> "\"."
    , locations = [fragmentPosition]
    }

instance KeyOf Fragment where 
  keyOf = fragmentName

type Fragments = OrderedMap Fragment

type Arguments a = OrderedMap (Argument a)

data SelectionContent (s :: Stage) where
  SelectionField :: SelectionContent s
  SelectionSet   :: SelectionSet s -> SelectionContent s
  UnionSelection :: UnionSelection -> SelectionContent VALID

instance Join (SelectionContent s) where
  merge xs (SelectionSet s1)  (SelectionSet s2) = SelectionSet <$> merge xs s1 s2
  merge path  oldC currC
    | oldC == currC = pure oldC
    | otherwise     = failure [
      GQLError {
        message = T.concat $ map refName path,
        locations = map refPosition path
      }
    ]

deriving instance Show (SelectionContent a)
deriving instance Eq   (SelectionContent a)
deriving instance Lift (SelectionContent a)

data UnionTag = UnionTag {
  unionTagName :: Name,
  unionTagSelection :: SelectionSet VALID
} deriving (Show, Eq, Lift)


mergeConflict :: [Ref] -> GQLError -> GQLErrors
mergeConflict [] err = [err]
mergeConflict refs@(rootField:xs) err = [
    GQLError {
      message =  renderSubfields <> message err,
      locations = map refPosition refs <> locations err
    }
  ]
  where 
    fieldConflicts ref = "\"" <> refName ref  <> "\" conflict because "
    renderSubfield ref txt = txt <> " subfields " <> fieldConflicts ref
    renderStart = "Fields " <> fieldConflicts rootField
    renderSubfields = 
        foldr
          renderSubfield
          renderStart
          xs

instance Join UnionTag where 
  merge path _ current = failure $ mergeConflict path $ nameCollision (keyOf current) current

instance KeyOf UnionTag where
  keyOf = unionTagName

instance NameCollision UnionTag where
  -- TODO: real error
  nameCollision _ UnionTag { unionTagName } = GQLError 
    { message   = "There can be only one Union named \"" <> unionTagName <> "\"."
    , locations = []
    }

type UnionSelection = MergeSet UnionTag

type SelectionSet s = MergeSet  (Selection s)

data Selection (s :: Stage) where
    Selection ::
      { selectionName       :: Name
      , selectionAlias      :: Maybe Name
      , selectionPosition   :: Position
      , selectionArguments  :: Arguments s
      , selectionContent    :: SelectionContent s
      } -> Selection s
    InlineFragment :: Fragment -> Selection RAW
    Spread :: Ref -> Selection RAW

instance KeyOf (Selection s) where
  keyOf Selection { selectionName , selectionAlias } = fromMaybe selectionName selectionAlias
  keyOf (InlineFragment fr) = fragmentType fr
  keyOf (Spread ref) = refName ref

instance Join (Selection a) where 
  merge path old@Selection{ selectionPosition = pos1 }  current@Selection{ selectionPosition = pos2 }
    = do
      selectionName <- mergeName
      selectionArguments <- mergeArguments
      selectionContent <- merge (path <> [Ref selectionName pos1]) (selectionContent old) (selectionContent current)
      pure $ Selection {
        selectionName,
        selectionAlias = mergeAlias,
        selectionPosition = pos1,
        selectionArguments,
        selectionContent
      }
    where 
      -- passes if: 
      -- * { user : user }
      -- * { user1: user
      --     user1: user
      --   }
      -- fails if:
      -- * { user1: user
      --     user1: product
      --   }
      mergeName 
        | selectionName old == selectionName current = pure $ selectionName current
        | otherwise = failure $ mergeConflict path $ GQLError {
          message = selectionName old <> " and " <> selectionName current 
              <> " are different fields. Use different aliases on the " 
              <> "fields to fetch both if this was intentional.",
          locations = [pos1, pos2]
        }
      ---------------------
      -- allias name is relevant only if they collide by allias like:
      --   { user1: user
      --     user1: user
      --   }
      mergeAlias 
        | all (isJust . selectionAlias) [old,current] = selectionAlias old
        | otherwise = Nothing
      --- arguments must be equal
      mergeArguments  
        | selectionArguments old == selectionArguments current = pure $ selectionArguments current
        | otherwise = failure $ mergeConflict path $ nameCollision (keyOf current) current
      --- merge content
  merge path _ current = failure $ mergeConflict path $ nameCollision (keyOf current) current

instance NameCollision (Selection s) where
  -- TODO: real error
  nameCollision _ Selection { selectionName , selectionPosition } = GQLError 
    { message   = "There can be only one Selection named \"" <> selectionName <> "\"."
    , locations = [selectionPosition]
    }
  nameCollision name _ = GQLError { 
      message   = "There can be only one Selection named \"" <> name <> "\"."
    , locations = []
    }

deriving instance Show (Selection a)
deriving instance Lift (Selection a)
deriving instance Eq (Selection a)

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