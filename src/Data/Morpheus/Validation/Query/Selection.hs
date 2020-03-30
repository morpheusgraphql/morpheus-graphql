{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Morpheus.Validation.Query.Selection
  ( validateSelectionSet
  )
where


-- MORPHEUS
import           Data.Morpheus.Error.Selection  ( hasNoSubfields
                                                , subfieldsNotSelected
                                                )
import           Data.Morpheus.Error.Variable   ( unknownType )
import           Data.Morpheus.Types.Internal.AST
                                                ( ValidVariables
                                                , Selection(..)
                                                , SelectionContent(..)
                                                , ValidSelection
                                                , ValidSelectionSet
                                                , Fragment(..)
                                                , Fragments
                                                , RawSelectionSet
                                                , FieldDefinition(..)
                                                , FieldsDefinition(..)
                                                , TypeContent(..)
                                                , TypeDefinition(..)
                                                , Schema(..)
                                                , TypeRef(..)
                                                , Name
                                                , RAW
                                                , VALID
                                                , Arguments
                                                , Position
                                                , isEntNode
                                                , lookupFieldAsSelectionSet
                                                , lookupSelectionField
                                                )
import           Data.Morpheus.Types.Internal.AST.MergeSet
                                                ( concatTraverse )
import           Data.Morpheus.Types.Internal.Operation
                                                ( selectBy 
                                                , empty
                                                , singleton
                                                )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( Validation
                                                , Failure(..)
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

validateSelectionSet
  :: Schema
  -> Fragments
  -> Name
  -> ValidVariables
  -> TypeDef
  -> RawSelectionSet
  -> Validation ValidSelectionSet
validateSelectionSet lib fragments operatorName variables = __validate
 where
  __validate
    :: TypeDef -> RawSelectionSet -> Validation ValidSelectionSet
  __validate dataType@(typeName,_) = concatTraverse validateSelection 
   where
    commonValidation :: Name -> Arguments RAW -> Position -> Validation (FieldDefinition, TypeContent, Arguments VALID)
    commonValidation key selectionArguments selectionPosition = do
      (fieldDef :: FieldDefinition) <- lookupSelectionField selectionPosition key dataType
      let feildTypeName = typeConName (fieldType fieldDef)
      -- validate field Argument -----
      (arguments ::Arguments VALID) <- validateArguments lib
                                     operatorName
                                     variables
                                     fieldDef
                                     selectionPosition
                                     selectionArguments
      -- check field Type existence  -----
      (typeCont :: TypeContent) <- typeContent <$> selectBy
            (unknownType feildTypeName selectionPosition) 
            feildTypeName 
            lib
      pure (fieldDef, typeCont, arguments)
    -- validate single selection: InlineFragments and Spreads will Be resolved and included in SelectionSet
    --
    validateSelection :: Selection RAW -> Validation ValidSelectionSet
    validateSelection sel@Selection { selectionName, selectionArguments = selArgs , selectionContent, selectionPosition } 
      = validateSelectionContent selectionContent
      where
        validateSelectionContent :: SelectionContent RAW -> Validation ValidSelectionSet
        validateSelectionContent SelectionField = singleton <$> selectField
         where
          selectField :: Validation ValidSelection
          selectField 
            | null selArgs && selectionName == "__typename" 
              = pure $ sel { selectionArguments = empty, selectionContent = SelectionField }
            | otherwise = do
              (dataField, datatypeContent, selectionArguments) <- commonValidation selectionName selArgs selectionPosition
              isLeaf datatypeContent dataField
              pure $ sel { selectionArguments, selectionContent = SelectionField }
          ------------------------------------------------------------
          isLeaf :: TypeContent -> FieldDefinition -> Validation ()
          isLeaf datatype FieldDefinition { fieldType = TypeRef { typeConName } }
              | isEntNode datatype = pure ()
              | otherwise = failure
              $ subfieldsNotSelected selectionName typeConName selectionPosition
        ----- SelectionSet
        validateSelectionContent (SelectionSet rawSelection)
          = do
            (dataField, datatype, selectionArguments) <- commonValidation selectionName selArgs selectionPosition
            selContent <- validateByTypeContent dataField datatype
            pure $ singleton $ sel { selectionArguments, selectionContent = selContent }
           where
            validateByTypeContent :: FieldDefinition -> TypeContent -> Validation (SelectionContent VALID)
            -- Validate UnionSelection  
            validateByTypeContent dataField DataUnion {} 
              = validateUnionSelection  
                    __validate
                    lib 
                    fragments
                    (selectionName,selectionPosition,selArgs) 
                    rawSelection 
                    (typeName,dataField)
            -- Validate Regular selection set
            validateByTypeContent dataField DataObject {} = do
                fieldType' <- lookupFieldAsSelectionSet selectionPosition
                                                        selectionName
                                                        lib
                                                        dataField
                SelectionSet <$> __validate fieldType' rawSelection

            validateByTypeContent dataField _ = failure $ hasNoSubfields selectionName
                                            (typeConName $fieldType dataField)
                                            selectionPosition
    validateSelection (Spread ref) =
      resolveSpread fragments [typeName] ref >>= validateFragment
    validateSelection (InlineFragment fragment') =
      castFragmentType Nothing (fragmentPosition fragment') [typeName] fragment'
        >>= validateFragment
    --------------------------------------------------------------------------------
    validateFragment Fragment { fragmentSelection } = __validate dataType fragmentSelection
