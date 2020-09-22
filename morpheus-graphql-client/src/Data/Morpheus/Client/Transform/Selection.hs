{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Transform.Selection
  ( toClientDefinition,
    ClientDefinition (..),
  )
where

--
-- MORPHEUS
import Data.Functor((<$>))
import Control.Monad((>>=))
import Control.Applicative(Applicative(..))
import Data.Maybe(Maybe(..))
import Control.Monad.Reader (asks, runReaderT)
import Data.Morpheus.Client.Internal.Types
  ( ClientDefinition (..),
    ClientTypeDefinition (..),
    TypeNameTH (..),
  )
import Data.Morpheus.Client.Transform.Core (Converter (..), compileError, deprecationWarning, getType, leafType, typeFrom)
import Data.Morpheus.Client.Transform.Inputs (renderNonOutputTypes, renderOperationArguments)
import Data.Morpheus.Internal.Utils
  ( Failure (..),
    elems,
    empty,
    keyOf,
    selectBy,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    ConsD (..),
    FieldDefinition (..),
    FieldName,
    Operation (..),
    RAW,
    Ref (..),
    Schema (..),
    Selection (..),
    SelectionContent (..),
    SelectionSet,
    TypeContent (..),
    TypeDefinition (..),
    TypeKind (..),
    TypeName,
    TypeRef (..),
    UnionTag (..),
    VALID,
    VariableDefinitions,
    getOperationDataType,
    getOperationName,
    msg,
    toAny,
    toFieldName,
    mkTypeRef,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
  )
import Data.Semigroup ((<>))
import Prelude
  ( flip,
    (.),
    fst,
    concat,
    traverse,
    unzip3,
    ($),
    otherwise,
    show,
    Eq(..)
  )

toClientDefinition ::
  Schema VALID ->
  VariableDefinitions RAW ->
  Operation VALID ->
  Eventless ClientDefinition
toClientDefinition schema vars = flip runReaderT (schema, vars) . runConverter . genOperation

genOperation :: Operation VALID -> Converter ClientDefinition
genOperation operation = do
  (clientArguments, outputTypes, enums) <- renderOperationType operation
  nonOutputTypes <- renderNonOutputTypes enums
  pure ClientDefinition {clientArguments, clientTypes = outputTypes <> nonOutputTypes}

renderOperationType ::
  Operation VALID ->
  Converter
    ( Maybe ClientTypeDefinition,
      [ClientTypeDefinition],
      [TypeName]
    )
renderOperationType op@Operation {operationName, operationSelection} = do
  datatype <- asks fst >>= getOperationDataType op
  arguments <- renderOperationArguments op
  (outputTypes, enums) <-
    genRecordType
      []
      (getOperationName operationName)
      (toAny datatype)
      operationSelection
  pure (arguments, outputTypes, enums)

-------------------------------------------------------------------------
-- generates selection Object Types
genRecordType ::
  [FieldName] ->
  TypeName ->
  TypeDefinition ANY VALID ->
  SelectionSet VALID ->
  Converter ([ClientTypeDefinition], [TypeName])
genRecordType path tName dataType recordSelSet = do
  (con, subTypes, requests) <- genConsD path tName dataType recordSelSet
  pure
    ( ClientTypeDefinition
        { clientTypeName = TypeNameTH path tName,
          clientCons = [con],
          clientKind = KindObject Nothing
        }
        : subTypes,
      requests
    )

genConsD ::
  [FieldName] ->
  TypeName ->
  TypeDefinition ANY VALID ->
  SelectionSet VALID ->
  Converter
    ( ConsD ANY VALID,
      [ClientTypeDefinition],
      [TypeName]
    )
genConsD path cName datatype selSet = do
  (cFields, subTypes, requests) <- unzip3 <$> traverse genField (elems selSet)
  pure (ConsD {cName, cFields}, concat subTypes, concat requests)
  where
    genField ::
      Selection VALID ->
      Converter (FieldDefinition ANY VALID, [ClientTypeDefinition], [TypeName])
    genField sel =
      do
        (fieldDataType, fieldType) <-
          getFieldType
            fieldPath
            datatype
            sel
        (subTypes, requests) <- subTypesBySelection fieldPath fieldDataType sel
        pure
          ( FieldDefinition
              { fieldName,
                fieldType,
                fieldContent = Nothing,
                fieldDescription = Nothing,
                fieldDirectives = empty
              },
            subTypes,
            requests
          )
      where
        fieldPath = path <> [fieldName]
        -------------------------------
        fieldName = keyOf sel

------------------------------------------
subTypesBySelection ::
  [FieldName] ->
  TypeDefinition ANY VALID ->
  Selection VALID ->
  Converter
    ( [ClientTypeDefinition],
      [TypeName]
    )
subTypesBySelection _ dType Selection {selectionContent = SelectionField} =
  leafType dType
subTypesBySelection path dType Selection {selectionContent = SelectionSet selectionSet} =
  genRecordType path (typeFrom [] dType) dType selectionSet
subTypesBySelection path dType Selection {selectionContent = UnionSelection unionSelections} =
  do
    (clientCons, subTypes, requests) <-
      unzip3 <$> traverse getUnionType (elems unionSelections)
    pure
      ( ClientTypeDefinition
          { clientTypeName = TypeNameTH path (typeFrom [] dType),
            clientCons,
            clientKind = KindUnion
          }
          : concat subTypes,
        concat requests
      )
  where
    getUnionType (UnionTag selectedTyName selectionVariant) = do
      conDatatype <- getType selectedTyName
      genConsD path selectedTyName conDatatype selectionVariant

getFieldType ::
  [FieldName] ->
  TypeDefinition ANY VALID ->
  Selection VALID ->
  Converter (TypeDefinition ANY VALID, TypeRef)
getFieldType
  path
  TypeDefinition {typeContent, typeName}
  Selection
    { selectionName,
      selectionPosition
    } 
    | selectionName == "__typename" 
        = processDeprecation FieldDefinition {
        fieldName = "__typename",
        fieldDescription = Nothing,
        fieldType = mkTypeRef "String",
        fieldDirectives = [],
        fieldContent = Nothing
      }
    | otherwise = withTypeContent typeContent
    where
      withTypeContent DataObject {objectFields} =
        selectBy selError selectionName objectFields >>= processDeprecation
      withTypeContent DataInterface {interfaceFields} =
        selectBy selError selectionName interfaceFields >>= processDeprecation
      withTypeContent dt =
        failure (compileError $ "Type should be output Object \"" <> msg (show dt))
      selError = compileError $ "can't find field " <> msg selectionName <> " on type: " <> msg (show typeContent)
      processDeprecation
        FieldDefinition
          { fieldType = alias@TypeRef {typeConName},
            fieldDirectives
          } =
          checkDeprecated *> (trans <$> getType typeConName)
          where
            trans x =
              (x, alias {typeConName = typeFrom path x, typeArgs = Nothing})
            ------------------------------------------------------------------
            checkDeprecated :: Converter ()
            checkDeprecated =
              deprecationWarning
                fieldDirectives
                (toFieldName typeName, Ref {refName = selectionName, refPosition = selectionPosition})
