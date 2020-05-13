{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Data.Morpheus.Client.Transform.Selection
  ( operationTypes,
  )
where

--
-- MORPHEUS
import Control.Monad.Reader (asks, runReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Morpheus.Client.Transform.Core (Converter (..), compileError, getType, leafType, typeFrom)
import Data.Morpheus.Client.Transform.Inputs (renderNonOutputTypes, renderOperationArguments)
import Data.Morpheus.Error
  ( deprecatedField,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    ArgumentsDefinition (..),
    ClientType (..),
    ConsD (..),
    DataTypeKind (..),
    FieldDefinition (..),
    Key,
    Name,
    Operation (..),
    RAW,
    Ref (..),
    Schema (..),
    Selection (..),
    SelectionContent (..),
    SelectionSet,
    TypeContent (..),
    TypeD (..),
    TypeDefinition (..),
    TypeRef (..),
    UnionTag (..),
    VALID,
    VariableDefinitions,
    getOperationDataType,
    getOperationName,
    lookupDeprecated,
    lookupDeprecatedReason,
    toAny,
  )
import Data.Morpheus.Types.Internal.Operation
  ( Failure (..),
    Listable (..),
    keyOf,
    selectBy,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
    Result (..),
  )
import Data.Semigroup ((<>))
import Data.Text
  ( Text,
    pack,
  )

renderOperationType :: Operation VALID -> Converter (Maybe TypeD, [ClientType], [Name])
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

operationTypes ::
  Schema ->
  VariableDefinitions RAW ->
  Operation VALID ->
  Eventless (Maybe TypeD, [ClientType])
operationTypes schema vars = flip runReaderT (schema, vars) . runConverter . genOperation

genOperation :: Operation VALID -> Converter (Maybe TypeD, [ClientType])
genOperation operation = do
  (arguments, outputTypes, enums) <- renderOperationType operation
  nonOutputTypes <- renderNonOutputTypes enums
  pure (arguments, outputTypes <> nonOutputTypes)

-------------------------------------------------------------------------
-- generates selection Object Types
genRecordType ::
  [Name] ->
  Name ->
  TypeDefinition ANY ->
  SelectionSet VALID ->
  Converter ([ClientType], [Name])
genRecordType path tName dataType recordSelSet = do
  (con, subTypes, requests) <- genConsD path tName dataType recordSelSet
  pure
    ( ClientType
        { clientType =
            TypeD
              { tName,
                tNamespace = path,
                tCons = [con],
                tMeta = Nothing
              },
          clientKind = KindObject Nothing
        }
        : subTypes,
      requests
    )

genConsD ::
  [Name] ->
  Name ->
  TypeDefinition ANY ->
  SelectionSet VALID ->
  Converter (ConsD, [ClientType], [Text])
genConsD path cName datatype selSet = do
  (cFields, subTypes, requests) <- unzip3 <$> traverse genField (toList selSet)
  pure (ConsD {cName, cFields}, concat subTypes, concat requests)
  where
    genField ::
      Selection VALID ->
      Converter (FieldDefinition, [ClientType], [Text])
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
                fieldArgs = NoArguments,
                fieldMeta = Nothing
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
  [Name] ->
  TypeDefinition ANY ->
  Selection VALID ->
  Converter ([ClientType], [Text])
subTypesBySelection _ dType Selection {selectionContent = SelectionField} =
  leafType dType
subTypesBySelection path dType Selection {selectionContent = SelectionSet selectionSet} =
  genRecordType path (typeFrom [] dType) dType selectionSet
subTypesBySelection path dType Selection {selectionContent = UnionSelection unionSelections} =
  do
    (tCons, subTypes, requests) <-
      unzip3 <$> traverse getUnionType (toList unionSelections)
    pure
      ( ClientType
          { clientType =
              TypeD
                { tNamespace = path,
                  tName = typeFrom [] dType,
                  tCons,
                  tMeta = Nothing
                },
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
  [Key] ->
  TypeDefinition ANY ->
  Selection VALID ->
  Converter (TypeDefinition ANY, TypeRef)
getFieldType
  path
  TypeDefinition {typeContent = DataObject {objectFields}, typeName}
  Selection
    { selectionName,
      selectionPosition
    } =
    selectBy selError selectionName objectFields >>= processDeprecation
    where
      selError = compileError $ "cant find field \"" <> pack (show objectFields) <> "\""
      processDeprecation FieldDefinition {fieldType = alias@TypeRef {typeConName}, fieldMeta} =
        checkDeprecated >> (trans <$> getType typeConName)
        where
          trans x =
            (x, alias {typeConName = typeFrom path x, typeArgs = Nothing})
          ------------------------------------------------------------------
          checkDeprecated :: Converter ()
          checkDeprecated = case fieldMeta >>= lookupDeprecated of
            Just deprecation -> Converter $ lift $ Success {result = (), warnings, events = []}
              where
                warnings =
                  deprecatedField
                    typeName
                    Ref {refName = selectionName, refPosition = selectionPosition}
                    (lookupDeprecatedReason deprecation)
            Nothing -> pure ()
getFieldType _ dt _ =
  failure (compileError $ "Type should be output Object \"" <> pack (show dt))
