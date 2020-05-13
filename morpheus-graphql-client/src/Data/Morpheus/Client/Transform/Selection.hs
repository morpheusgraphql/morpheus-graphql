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
import Data.Morpheus.Client.Transform.Core (Converter (..))
import Data.Morpheus.Client.Transform.Inputs (leafType, renderNonOutputTypes)
import Data.Morpheus.Error
  ( deprecatedField,
    globalErrorMessage,
  )
import Data.Morpheus.Internal.Utils
  ( nameSpaceType,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    ArgumentsDefinition (..),
    ClientType (..),
    ConsD (..),
    DataTypeKind (..),
    FieldDefinition (..),
    GQLErrors,
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
    Variable (..),
    VariableDefinitions,
    getOperationDataType,
    getOperationName,
    lookupDeprecated,
    lookupDeprecatedReason,
    toAny,
    typeFromScalar,
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

compileError :: Text -> GQLErrors
compileError x =
  globalErrorMessage $ "Unhandled Compile Time Error: \"" <> x <> "\" ;"

renderArguments :: VariableDefinitions RAW -> Text -> Maybe TypeD
renderArguments variables argsName
  | null variables = Nothing
  | otherwise = Just rootArgumentsType
  where
    rootArgumentsType :: TypeD
    rootArgumentsType =
      TypeD
        { tName = argsName,
          tNamespace = [],
          tCons = [ConsD {cName = argsName, cFields = map fieldD (toList variables)}],
          tMeta = Nothing
        }
      where
        fieldD :: Variable RAW -> FieldDefinition
        fieldD Variable {variableName, variableType} =
          FieldDefinition
            { fieldName = variableName,
              fieldArgs = NoArguments,
              fieldType = variableType,
              fieldMeta = Nothing
            }

renderOperationArguments :: Operation VALID -> Converter (Maybe TypeD)
renderOperationArguments Operation {operationName} = do
  variables <- asks snd
  pure $ renderArguments variables (getOperationName operationName <> "Args")

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
          lookupFieldType
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

lookupFieldType ::
  [Key] ->
  TypeDefinition ANY ->
  Selection VALID ->
  Converter (TypeDefinition ANY, TypeRef)
lookupFieldType
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
lookupFieldType _ dt _ =
  failure (compileError $ "Type should be output Object \"" <> pack (show dt))

getType :: Text -> Converter (TypeDefinition ANY)
getType typename = asks fst >>= selectBy (compileError $ " cant find Type" <> typename) typename

typeFrom :: [Name] -> TypeDefinition a -> Name
typeFrom path TypeDefinition {typeName, typeContent} = __typeFrom typeContent
  where
    __typeFrom DataScalar {} = typeFromScalar typeName
    __typeFrom DataObject {} = nameSpaceType path typeName
    __typeFrom DataUnion {} = nameSpaceType path typeName
    __typeFrom _ = typeName
