{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.CodeGen.Interpreting.Local
  ( toLocalDefinitions,
  )
where

import Data.Morpheus.Client.CodeGen.AST
  ( ClientDeclaration (..),
    ClientPreDeclaration (..),
    RequestTypeDefinition (..),
    UnionPat (..),
  )
import Data.Morpheus.Client.CodeGen.Interpreting.Arguments (genArguments)
import Data.Morpheus.Client.CodeGen.Interpreting.Core
  ( LocalContext (..),
    LocalM,
    clientConfig,
    defaultDerivations,
    deprecationWarning,
    existFragment,
    getNameByPath,
    getType,
    lookupField,
    lookupType,
    registerFragment,
    removeDuplicates,
    typeFrom,
    warning,
  )
import Data.Morpheus.Client.CodeGen.Interpreting.PreDeclarations
  ( mapPreDeclarations,
  )
import Data.Morpheus.CodeGen.Internal.AST
  ( CodeGenConstructor (..),
    CodeGenField (..),
    CodeGenType (..),
    CodeGenTypeName (..),
    FIELD_TYPE_WRAPPER (..),
    fromTypeName,
    getFullName,
  )
import Data.Morpheus.CodeGen.Utils
  ( Flags,
    langExtension,
    requireExternal,
    runCodeGenT,
  )
import Data.Morpheus.Core (validateRequest)
import Data.Morpheus.Error (deprecatedField)
import Data.Morpheus.Internal.Ext
  ( GQLResult,
  )
import Data.Morpheus.Internal.Utils
  ( keyOf,
    member,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    ExecutableDocument (..),
    FieldDefinition (..),
    FieldName,
    FragmentName,
    Operation (..),
    Position (..),
    PropName (..),
    Schema (..),
    Selection (..),
    SelectionContent (..),
    SelectionSet,
    TypeDefinition (..),
    TypeKind (..),
    TypeName,
    TypeRef (..),
    UnionTag (..),
    VALID,
    at,
    getOperationDataType,
    getOperationName,
    isNullable,
    kindOf,
    msg,
    toAny,
    unpackName,
    withPath,
  )
import qualified Data.Set as S
import qualified Data.Text as T
import Relude hiding (empty, show)

toLocalDefinitions :: (Text, ExecutableDocument) -> Schema VALID -> GQLResult ([ClientDeclaration], Flags)
toLocalDefinitions (query, request) ctxSchema = do
  validOperation <- validateRequest clientConfig ctxSchema request
  let context =
        LocalContext
          { ctxSchema,
            ctxVariables = operationArguments $ operation request,
            ctxPosition = Nothing,
            ctxFragments = mempty
          }
  (t, flags) <- runCodeGenT (genLocalDeclarations query validOperation) context
  types <- removeDuplicates <$> traverse mapPreDeclarations t
  pure (types, flags)

genLocalDeclarations :: Text -> Operation VALID -> LocalM [ClientPreDeclaration]
genLocalDeclarations query op@Operation {operationName, operationSelection, operationType} = do
  LocalContext {ctxSchema, ctxVariables} <- asks id
  datatype <- getOperationDataType op ctxSchema
  let operationTypeName = getOperationName operationName
  let (requestArgs, argTypes) = genArguments operationTypeName ctxVariables
  (rootTypeName, localTypes) <- genLocalTypes [coerce operationTypeName] operationTypeName (toAny datatype) operationSelection
  pure
    ( RequestTypeClass
        RequestTypeDefinition
          { requestArgs = requestArgs,
            requestName = typename rootTypeName,
            requestType = operationType,
            requestQuery = T.unpack query
          }
        : localTypes <> argTypes
    )

genLocalTypes ::
  [FieldName] ->
  TypeName ->
  TypeDefinition ANY VALID ->
  SelectionSet VALID ->
  LocalM (CodeGenTypeName, [ClientPreDeclaration])
genLocalTypes namespace tName = genObjectTypeWithFragment namespace (getNameByPath namespace tName)

subTypesBySelection ::
  TypeName ->
  [FieldName] ->
  TypeDefinition ANY VALID ->
  Selection VALID ->
  LocalM (CodeGenTypeName, [ClientPreDeclaration])
subTypesBySelection name _ _ Selection {selectionContent = SelectionField} = do
  kind <- fmap kindOf <$> lookupType name
  if null kind || kind == Just KIND_SCALAR
    then requireExternal (unpackName name) $> (fromTypeName name, [])
    else pure (fromTypeName name, [])
subTypesBySelection _ path dType Selection {selectionContent = SelectionSet selectionSet} =
  genLocalTypes path (getFullName $ typeFrom [] dType) dType selectionSet
subTypesBySelection _ namespace dType Selection {selectionPosition, selectionContent = UnionSelection interface unionSelections} =
  do
    let variants = toList unionSelections
    traverse_ (checkTypename selectionPosition namespace interface) variants
    let cgTypeName = typeFrom namespace dType
    (cons, subTypes) <- unzip <$> traverse (getVariant namespace) variants
    (fallbackCons, fallBackTypes) <- maybe (getEmptyFallback cgTypeName) (getVariant namespace . UnionTag (typeName dType)) interface
    let typeDef = CodeGenType {cgTypeName, cgConstructors = map buildVariantConstructor (cons <> [fallbackCons]), cgDerivations = defaultDerivations}
    langExtension "LambdaCase"
    pure (cgTypeName, [ClientType typeDef, FromJSONUnionClass cgTypeName (map tagConstructor cons <> [(UVar "_fallback", mapFallback fallbackCons)])] <> concat subTypes <> fallBackTypes)
  where
    tagConstructor (name, x) = (UString $ typename name, (name, fmap (const "v") x))
    mapFallback (x, y) = (x, fmap (const "v") y)

checkTypename :: Position -> [FieldName] -> Maybe (SelectionSet VALID) -> UnionTag -> LocalM ()
checkTypename pos path iFace UnionTag {..}
  | any (member "__typename") (unionTagSelection : toList iFace) = pure ()
  | otherwise =
      warning
        $ withPath
          ("missing \"__typename\" for selection " <> msg unionTagName <> ". this can lead to undesired behavior at runtime!")
          (map (PropName . unpackName) path)
        `at` pos

type Variant = (CodeGenTypeName, Maybe TypeName)

getEmptyFallback :: CodeGenTypeName -> LocalM (Variant, [ClientPreDeclaration])
getEmptyFallback name = pure ((name, Nothing), [])

buildVariantConstructor :: Variant -> CodeGenConstructor
buildVariantConstructor (conName, ref) =
  CodeGenConstructor
    { constructorName = conName,
      constructorFields =
        ( \fieldType ->
            CodeGenField
              { fieldName = "_",
                fieldType,
                wrappers = [],
                fieldIsNullable = False
              }
        )
          <$> toList ref
    }

getVariant :: [FieldName] -> UnionTag -> LocalM (Variant, [ClientPreDeclaration])
getVariant path (UnionTag selectedTyName selectionVariant) = do
  -- traceShow (map getSelectionOrigins variants)
  conDatatype <- getType selectedTyName
  let name = CodeGenTypeName path [] selectedTyName
  (n, types) <- genObjectTypeWithFragment path name conDatatype selectionVariant
  pure
    ( ( CodeGenTypeName (path <> ["variant"]) [] selectedTyName,
        Just (getFullName n)
      ),
      types
    )

getFragmentOrigin :: SelectionSet VALID -> Maybe FragmentName
getFragmentOrigin x = case toList (S.fromList (map selectionOrigin $ toList x)) of
  [Just name] -> Just name
  _ -> Nothing

genObjectTypeWithFragment ::
  [FieldName] ->
  CodeGenTypeName ->
  TypeDefinition ANY VALID ->
  SelectionSet VALID ->
  LocalM (CodeGenTypeName, [ClientPreDeclaration])
genObjectTypeWithFragment namespace cgTypeName dataType recordSelSet = do
  case getFragmentOrigin recordSelSet of
    Just name -> do
      exists <- existFragment name
      let tName = CodeGenTypeName [] [] ("Fragment" <> coerce name)
      if exists
        then pure (tName, [])
        else registerFragment name (genObjectType namespace tName dataType recordSelSet)
    Nothing -> genObjectType namespace cgTypeName dataType recordSelSet

genObjectType ::
  [FieldName] ->
  CodeGenTypeName ->
  TypeDefinition ANY VALID ->
  SelectionSet VALID ->
  LocalM (CodeGenTypeName, [ClientPreDeclaration])
genObjectType namespace cgTypeName datatype selSet = do
  (fields, subTypes) <- unzip <$> traverse (genField namespace datatype) (toList selSet)
  let constructor = CodeGenConstructor {constructorName = cgTypeName, constructorFields = fields}
  let definition = CodeGenType {cgTypeName, cgConstructors = [constructor], cgDerivations = defaultDerivations}
  pure (cgTypeName, [ClientType definition, FromJSONObjectClass cgTypeName constructor] <> concat subTypes)

genField :: [FieldName] -> TypeDefinition ANY VALID -> Selection VALID -> LocalM (CodeGenField, [ClientPreDeclaration])
genField path datatype sel = do
  let fieldName = keyOf sel
  let fieldPath = path <> [fieldName]
  (fieldDataType, TypeRef {..}) <- getFieldType fieldPath datatype sel
  (fieldTypeName, subTypes) <- subTypesBySelection typeConName fieldPath fieldDataType sel
  pure
    ( CodeGenField
        { fieldName,
          fieldType = getFullName fieldTypeName,
          wrappers = [GQL_WRAPPER typeWrappers],
          fieldIsNullable = isNullable typeWrappers
        },
      subTypes
    )

getFieldType ::
  [FieldName] ->
  TypeDefinition ANY VALID ->
  Selection VALID ->
  LocalM (TypeDefinition ANY VALID, TypeRef)
getFieldType
  path
  TypeDefinition {typeContent, typeName}
  Selection
    { selectionName,
      selectionPosition
    } = lookupField selectionName typeContent >>= processFieldDefinition
    where
      processFieldDefinition FieldDefinition {fieldType = TypeRef {..}, ..} = do
        deprecationWarning fieldWarnings fieldDirectives
        typeDef <- getType typeConName
        pure (typeDef, TypeRef {typeConName = getFullName (typeFrom path typeDef), ..})
        where
          fieldWarnings reason =
            ( deprecatedField typeName selectionName reason
                `at` selectionPosition
            )
              `withPath` map (PropName . unpackName) path
