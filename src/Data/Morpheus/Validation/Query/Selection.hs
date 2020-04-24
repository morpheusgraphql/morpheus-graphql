{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Morpheus.Validation.Query.Selection
  ( validateOperation
  )
where


import           Data.Semigroup                 ( (<>) )

-- MORPHEUS
import           Data.Morpheus.Error.Selection  ( hasNoSubfields
                                                , subfieldsNotSelected
                                                )
import           Data.Morpheus.Types.Internal.AST
                                                ( Selection(..)
                                                , SelectionContent(..)
                                                , Fragment(..)
                                                , SelectionSet
                                                , FieldDefinition(..)
                                                , FieldsDefinition(..)
                                                , TypeContent(..)
                                                , TypeDefinition(..)
                                                , Operation(..)
                                                , Ref(..)
                                                , Name
                                                , RAW
                                                , VALID
                                                , Arguments
                                                , isEntNode
                                                , getOperationDataType
                                                , GQLError(..)
                                                , OperationType(..)
                                                )
import           Data.Morpheus.Types.Internal.AST.MergeSet
                                                ( concatTraverse )
import           Data.Morpheus.Types.Internal.Operation
                                                ( empty
                                                , singleton
                                                , Failure(..)
                                                , keyOf
                                                , toList
                                                )
import           Data.Morpheus.Types.Internal.Validation
                                                ( SelectionValidator
                                                , askFieldType
                                                , selectKnown
                                                , withScope
                                                , askSchema
                                                )
import           Data.Morpheus.Validation.Query.UnionSelection
                                                (validateUnionSelection)
import           Data.Morpheus.Validation.Query.Arguments
                                                ( validateArguments )
import           Data.Morpheus.Validation.Query.Fragment
                                                ( castFragmentType
                                                , resolveSpread
                                                )

type TypeDef = (Name, FieldsDefinition)


getOperationObject
  :: Operation a -> SelectionValidator (Name, FieldsDefinition)
getOperationObject operation = do
  dt <- askSchema >>= getOperationDataType operation 
  case dt of
    TypeDefinition { typeContent = DataObject { objectFields }, typeName } -> pure (typeName, objectFields)
    TypeDefinition { typeName } ->
      failure
        $  "Type Mismatch: operation \""
        <> typeName
        <> "\" must be an Object"


selectionsWitoutTypename :: SelectionSet VALID -> [Selection VALID] 
selectionsWitoutTypename = filter (("__typename" /=) . keyOf) . toList

singleTopLevelSelection :: Operation RAW -> SelectionSet VALID -> SelectionValidator ()
singleTopLevelSelection Operation { operationType = Subscription , operationName } selSet = 
   case selectionsWitoutTypename selSet of
  (_:xs) | not (null xs) -> failure $ map (singleTopLevelSelectionError  operationName) xs
  _ -> pure ()
singleTopLevelSelection _ _ = pure () 

singleTopLevelSelectionError :: Maybe Name -> Selection VALID -> GQLError
singleTopLevelSelectionError name Selection { selectionPosition } = GQLError 
      { message 
        = subscriptionName <> " must select " 
        <> "only one top level field."
      , locations = [selectionPosition]
      }
    where
      subscriptionName = maybe "Anonymous Subscription" (("Subscription \"" <>) . (<> "\""))  name

validateOperation
  :: Operation RAW
  -> SelectionValidator (Operation VALID)
validateOperation 
    rawOperation@Operation 
      { operationName
      , operationType
      , operationSelection
      , operationPosition 
      } 
    = do
      typeDef  <-  getOperationObject rawOperation
      selection <- validateSelectionSet typeDef operationSelection
      singleTopLevelSelection rawOperation selection
      pure $ Operation 
              { operationName
              , operationType
              , operationArguments = empty
              , operationSelection = selection
              , operationPosition
              }


validateSelectionSet
    :: TypeDef -> SelectionSet RAW -> SelectionValidator (SelectionSet VALID)
validateSelectionSet dataType@(typeName,fieldsDef) =
      concatTraverse validateSelection 
   where
    -- validate single selection: InlineFragments and Spreads will Be resolved and included in SelectionSet
    validateSelection :: Selection RAW -> SelectionValidator (SelectionSet VALID)
    validateSelection 
        sel@Selection 
          { selectionName
          , selectionArguments
          , selectionContent
          , selectionPosition 
          } 
      = withScope 
        typeName 
        selectionPosition $ 
        validateSelectionContent selectionContent
      where
        commonValidation :: SelectionValidator (TypeDefinition, Arguments VALID)
        commonValidation  = do
          (fieldDef :: FieldDefinition) <- selectKnown (Ref selectionName selectionPosition) fieldsDef
          -- validate field Argument -----
          arguments <- validateArguments
                        fieldDef
                        selectionArguments
          -- check field Type existence  -----
          (typeDef :: TypeDefinition) <- askFieldType fieldDef
          pure (typeDef, arguments)
        -----------------------------------------------------------------------------------
        validateSelectionContent :: SelectionContent RAW -> SelectionValidator (SelectionSet VALID)
        validateSelectionContent SelectionField 
            | null selectionArguments && selectionName == "__typename" 
              = pure $ singleton $ sel { selectionArguments = empty, selectionContent = SelectionField }
            | otherwise = do
              (datatype, validArgs) <- commonValidation
              isLeaf datatype
              pure $ singleton $ sel { selectionArguments = validArgs, selectionContent = SelectionField }
         where
          ------------------------------------------------------------
          isLeaf :: TypeDefinition -> SelectionValidator ()
          isLeaf TypeDefinition { typeName = typename, typeContent }
              | isEntNode typeContent = pure ()
              | otherwise = failure
              $ subfieldsNotSelected selectionName typename selectionPosition
        ----- SelectionSet
        validateSelectionContent (SelectionSet rawSelectionSet)
          = do
            (TypeDefinition { typeName = name , typeContent}, validArgs) <- commonValidation
            selContent <- withScope name selectionPosition $ validateByTypeContent name typeContent
            pure $ singleton $ sel { selectionArguments = validArgs, selectionContent = selContent }
           where
            validateByTypeContent :: Name -> TypeContent -> SelectionValidator (SelectionContent VALID)
            -- Validate UnionSelection  
            validateByTypeContent _ DataUnion { unionMembers } 
              = validateUnionSelection  
                    validateSelectionSet
                    rawSelectionSet 
                    unionMembers
            -- Validate Regular selection set
            validateByTypeContent typename DataObject { objectFields } 
              = SelectionSet 
                  <$> validateSelectionSet 
                        (typename, objectFields) 
                        rawSelectionSet
            validateByTypeContent typename _ 
              = failure 
                  $ hasNoSubfields 
                      (Ref selectionName selectionPosition) 
                      typename
    validateSelection (Spread ref) 
      = resolveSpread [typeName] ref 
        >>= validateFragment
    validateSelection (InlineFragment fragment') 
      = castFragmentType Nothing (fragmentPosition fragment') [typeName] fragment'
        >>= validateFragment
    --------------------------------------------------------------------------------
    validateFragment Fragment { fragmentSelection } = validateSelectionSet dataType fragmentSelection
