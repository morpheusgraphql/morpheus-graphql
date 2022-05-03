{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Transform.Local
  ( toLocalDefinitions,
  )
where

import Control.Monad.Except (MonadError (throwError))
import Data.Morpheus.Client.Internal.Types
  ( ClientConstructorDefinition (..),
    ClientTypeDefinition (..),
    FetchDefinition (..),
    TypeNameTH (..),
  )
import Data.Morpheus.Client.Transform.Core (Converter (..), compileError, deprecationWarning, getType, typeFrom)
import Data.Morpheus.Client.Transform.Global (toArgumentsType)
import Data.Morpheus.Core (Config (..), VALIDATION_MODE (WITHOUT_VARIABLES), validateRequest)
import Data.Morpheus.Internal.Ext
  ( GQLResult,
  )
import Data.Morpheus.Internal.Utils
  ( empty,
    keyOf,
    selectBy,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    ExecutableDocument (..),
    FieldDefinition (..),
    FieldName,
    OUT,
    Operation (..),
    Ref (..),
    Schema (..),
    Selection (..),
    SelectionContent (..),
    SelectionSet,
    TRUE,
    TypeContent (..),
    TypeDefinition (..),
    TypeKind (..),
    TypeName,
    TypeRef (..),
    UnionTag (..),
    VALID,
    getOperationDataType,
    getOperationName,
    mkTypeRef,
    msg,
    toAny,
  )
import Relude hiding (empty, show)
import Prelude (show)

clientConfig :: Config
clientConfig =
  Config
    { debug = False,
      validationMode = WITHOUT_VARIABLES
    }

toLocalDefinitions ::
  ExecutableDocument ->
  Schema VALID ->
  GQLResult
    ( FetchDefinition,
      [ClientTypeDefinition]
    )
toLocalDefinitions request schema = do
  validOperation <- validateRequest clientConfig schema request
  flip runReaderT (schema, operationArguments $ operation request) $
    runConverter $ genOperation validOperation

genOperation :: Operation VALID -> Converter (FetchDefinition, [ClientTypeDefinition])
genOperation op@Operation {operationName, operationSelection} = do
  (schema, varDefs) <- asks id
  datatype <- getOperationDataType op schema
  let argumentsType = toArgumentsType (getOperationName operationName <> "Args") varDefs
  (rootType :| localTypes) <-
    genLocalTypes
      []
      (getOperationName operationName)
      (toAny datatype)
      operationSelection
  pure
    ( FetchDefinition
        { clientArgumentsTypeName = fmap clientTypeName argumentsType,
          rootTypeName = clientTypeName rootType
        },
      rootType : (localTypes <> maybeToList argumentsType)
    )

-------------------------------------------------------------------------
-- generates selection Object Types
genLocalTypes ::
  [FieldName] ->
  TypeName ->
  TypeDefinition ANY VALID ->
  SelectionSet VALID ->
  Converter (NonEmpty ClientTypeDefinition)
genLocalTypes path tName dataType recordSelSet = do
  (con, subTypes) <- toConstructorDefinition (if null path then [coerce tName] else path) tName dataType recordSelSet
  pure $
    ClientTypeDefinition
      { clientTypeName = TypeNameTH path tName,
        clientCons = [con],
        clientKind = KindObject Nothing
      }
      :| subTypes

toConstructorDefinition ::
  [FieldName] ->
  TypeName ->
  TypeDefinition ANY VALID ->
  SelectionSet VALID ->
  Converter (ClientConstructorDefinition, [ClientTypeDefinition])
toConstructorDefinition path cName datatype selSet = do
  (cFields, subTypes) <- unzip <$> traverse genField (toList selSet)
  pure (ClientConstructorDefinition {cName, cFields}, concat subTypes)
  where
    genField :: Selection VALID -> Converter (FieldDefinition ANY VALID, [ClientTypeDefinition])
    genField sel = do
      let fieldName = keyOf sel
      let fieldPath = path <> [fieldName]
      (fieldDataType, fieldType) <- getFieldType fieldPath datatype sel
      subTypes <- subTypesBySelection fieldPath fieldDataType sel
      pure
        ( FieldDefinition
            { fieldName,
              fieldType,
              fieldContent = Nothing,
              fieldDescription = Nothing,
              fieldDirectives = empty
            },
          subTypes
        )

------------------------------------------
subTypesBySelection ::
  [FieldName] ->
  TypeDefinition ANY VALID ->
  Selection VALID ->
  Converter [ClientTypeDefinition]
subTypesBySelection _ _ Selection {selectionContent = SelectionField} = pure []
subTypesBySelection path dType Selection {selectionContent = SelectionSet selectionSet} = do
  toList <$> genLocalTypes path (typeFrom [] dType) dType selectionSet
subTypesBySelection path dType Selection {selectionContent = UnionSelection interface unionSelections} =
  do
    let variants = UnionTag (typeName dType) interface : toList unionSelections
    (clientCons, subTypes) <- unzip <$> traverse (getVariantType path) variants
    pure
      ( ClientTypeDefinition
          { clientTypeName = TypeNameTH path (typeFrom [] dType),
            clientCons,
            clientKind = KindUnion
          } :
        concat subTypes
      )

getVariantType :: [FieldName] -> UnionTag -> Converter (ClientConstructorDefinition, [ClientTypeDefinition])
getVariantType path (UnionTag selectedTyName selectionVariant) = do
  conDatatype <- getType selectedTyName
  toConstructorDefinition path selectedTyName conDatatype selectionVariant

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
    } = toFieldDef typeContent >>= processFieldDefinition
    where
      toFieldDef :: TypeContent TRUE ANY VALID -> Converter (FieldDefinition OUT VALID)
      toFieldDef _
        | selectionName == "__typename" =
          pure
            FieldDefinition
              { fieldName = "__typename",
                fieldDescription = Nothing,
                fieldType = mkTypeRef "String",
                fieldDirectives = empty,
                fieldContent = Nothing
              }
      toFieldDef DataObject {objectFields} = selectBy selError selectionName objectFields
      toFieldDef DataInterface {interfaceFields} = selectBy selError selectionName interfaceFields
      toFieldDef dt = throwError (compileError $ "Type should be output Object \"" <> msg (show dt))
      selError = compileError $ "can't find field " <> msg selectionName <> " on type: " <> msg (show typeContent)
      --
      processFieldDefinition
        FieldDefinition
          { fieldType = TypeRef {..},
            fieldDirectives
          } =
          checkDeprecated *> (trans <$> getType typeConName)
          where
            trans x = (x, TypeRef {typeConName = typeFrom path x, ..})
            ------------------------------------------------------------------
            checkDeprecated :: Converter ()
            checkDeprecated = deprecationWarning fieldDirectives (coerce typeName, Ref selectionName selectionPosition)
