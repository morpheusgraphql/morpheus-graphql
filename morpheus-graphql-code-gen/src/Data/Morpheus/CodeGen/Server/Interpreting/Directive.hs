{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.CodeGen.Server.Interpreting.Directive
  ( getDirs,
    getNamespaceDirs,
    dirRename,
  )
where

import Data.Char (isUpper)
import Data.Morpheus.CodeGen.Internal.AST
  ( getFullName,
  )
import Data.Morpheus.CodeGen.Server.Internal.AST (ServerDirectiveUsage (..), TypeValue (..), unpackName)
import Data.Morpheus.CodeGen.Server.Interpreting.Utils
  ( CodeGenT,
    TypeContext (..),
    getEnumName,
    getFieldName,
    inType,
    lookupFieldType,
  )
import Data.Morpheus.Core (internalSchema, render)
import Data.Morpheus.Internal.Utils (IsMap, selectOr)
import Data.Morpheus.Types.Internal.AST
  ( Argument (..),
    ArgumentDefinition (..),
    CONST,
    DataEnumValue (..),
    Description,
    Directive (Directive, directiveArgs, directiveName),
    DirectiveDefinition (..),
    FieldDefinition (..),
    FieldName,
    FieldsDefinition,
    Name,
    ObjectEntry (..),
    TypeContent (..),
    TypeDefinition (..),
    TypeName,
    TypeRef (..),
  )
import qualified Data.Morpheus.Types.Internal.AST as AST
import Data.Text (head)
import Relude hiding (ByteString, get, head)

getNamespaceDirs :: MonadReader (TypeContext s) m => Text -> m [ServerDirectiveUsage]
getNamespaceDirs genTypeName = do
  namespaces <- asks hasNamespace
  pure [TypeDirectiveUsage (dirDropNamespace genTypeName) | namespaces]

descDirective :: Maybe Description -> [TypeValue]
descDirective desc = map describe (maybeToList desc)
  where
    describe x = TypeValueObject "Describe" [("text", TypeValueString x)]

dirDropNamespace :: Text -> TypeValue
dirDropNamespace name = TypeValueObject "DropNamespace" [("dropNamespace", TypeValueString name)]

dirRename :: Name t -> TypeValue
dirRename name = TypeValueObject "Rename" [("newName", TypeValueString (unpackName name))]

class Meta a where
  getDirs :: MonadFail m => a -> CodeGenT m [ServerDirectiveUsage]

instance (Meta a) => Meta (Maybe a) where
  getDirs (Just x) = getDirs x
  getDirs _ = pure []

instance Meta (TypeDefinition c CONST) where
  getDirs TypeDefinition {typeContent, typeDirectives, typeDescription} = do
    contentD <- getDirs typeContent
    typeD <- traverse transform (toList typeDirectives)
    pure (contentD <> typeD <> map TypeDirectiveUsage (descDirective typeDescription))
    where
      transform v = TypeDirectiveUsage <$> directiveTypeValue v

instance Meta (TypeContent a c CONST) where
  getDirs DataObject {objectFields} = getDirs objectFields
  getDirs DataInputObject {inputObjectFields} = getDirs inputObjectFields
  getDirs DataInterface {interfaceFields} = getDirs interfaceFields
  getDirs DataEnum {enumMembers} = concat <$> traverse getDirs enumMembers
  getDirs _ = pure []

instance Meta (DataEnumValue CONST) where
  getDirs DataEnumValue {enumName, enumDirectives, enumDescription} = do
    dirs <- traverse directiveTypeValue (toList enumDirectives)
    name <- getFullName <$> getEnumName enumName
    pure $ map (EnumDirectiveUsage name) (dirs <> descDirective enumDescription)

instance Meta (FieldsDefinition c CONST) where
  getDirs = fmap concat . traverse getDirs . toList

instance Meta (FieldDefinition c CONST) where
  getDirs FieldDefinition {fieldName, fieldDirectives, fieldDescription} = do
    dirs <- traverse directiveTypeValue (toList fieldDirectives)
    name <- getFieldName fieldName
    let renameField = [FieldDirectiveUsage name (dirRename fieldName) | isUpperCase fieldName]
    pure $ renameField <> map (FieldDirectiveUsage name) (dirs <> descDirective fieldDescription)

directiveTypeValue :: MonadFail m => Directive CONST -> CodeGenT m TypeValue
directiveTypeValue Directive {..} = inType typeContext $ do
  dirs <- getDirective directiveName
  TypeValueObject typename <$> traverse (renderArgumentValue directiveArgs) (toList $ directiveDefinitionArgs dirs)
  where
    (typeContext, typename) = renderDirectiveTypeName directiveName

nativeDirectives :: AST.DirectivesDefinition CONST
nativeDirectives = AST.directiveDefinitions internalSchema

isUpperCase :: FieldName -> Bool
isUpperCase = isUpper . head . unpackName

getDirective :: (MonadReader (TypeContext CONST) m, MonadFail m) => FieldName -> m (DirectiveDefinition CONST)
getDirective directiveName = do
  dirs <- asks directiveDefinitions
  case find (\DirectiveDefinition {directiveDefinitionName} -> directiveDefinitionName == directiveName) dirs of
    Just dir -> pure dir
    _ -> selectOr (fail $ "unknown directive" <> show directiveName) pure directiveName nativeDirectives

renderDirectiveTypeName :: FieldName -> (Maybe TypeName, TypeName)
renderDirectiveTypeName "deprecated" = (Nothing, "Deprecated")
renderDirectiveTypeName name = (Just (coerce name), coerce name)

renderArgumentValue ::
  (IsMap FieldName c, MonadFail m) =>
  c (Argument CONST) ->
  ArgumentDefinition s ->
  ReaderT (TypeContext CONST) m (FieldName, TypeValue)
renderArgumentValue args ArgumentDefinition {..} = do
  let dirName = AST.fieldName argument
  gqlValue <- selectOr (pure AST.Null) (pure . argumentValue) dirName args
  typeValue <- mapWrappedValue (AST.fieldType argument) gqlValue
  fName <- getFieldName dirName
  pure (fName, typeValue)

mapWrappedValue :: MonadFail m => TypeRef -> AST.Value CONST -> CodeGenT m TypeValue
mapWrappedValue (TypeRef name (AST.BaseType isRequired)) value
  | isRequired = mapValue name value
  | value == AST.Null = pure (TypedValueMaybe Nothing)
  | otherwise = TypedValueMaybe . Just <$> mapValue name value
mapWrappedValue (TypeRef name (AST.TypeList elems isRequired)) d = case d of
  AST.Null | not isRequired -> pure (TypedValueMaybe Nothing)
  (AST.List xs) -> TypedValueMaybe . Just . TypeValueList <$> traverse (mapWrappedValue (TypeRef name elems)) xs
  value -> expected "list" value

mapValue :: MonadFail m => TypeName -> AST.Value CONST -> CodeGenT m TypeValue
mapValue name (AST.List xs) = TypeValueList <$> traverse (mapValue name) xs
mapValue _ (AST.Enum name) = pure $ TypeValueObject name []
mapValue name (AST.Object fields) = TypeValueObject name <$> traverse (mapField name) (toList fields)
mapValue _ (AST.Scalar x) = mapScalarValue x
mapValue t v = expected (show t) v

mapScalarValue :: MonadFail m => AST.ScalarValue -> CodeGenT m TypeValue
mapScalarValue (AST.Int x) = pure $ TypeValueNumber (fromIntegral x)
mapScalarValue (AST.Float x) = pure $ TypeValueNumber x
mapScalarValue (AST.String x) = pure $ TypeValueString x
mapScalarValue (AST.Boolean x) = pure $ TypeValueBool x
mapScalarValue (AST.Value _) = fail "JSON objects are not supported!"

expected :: MonadFail m => String -> AST.Value CONST -> CodeGenT m TypeValue
expected typ value = fail ("expected " <> typ <> ", found " <> show (render value) <> "!")

mapField :: MonadFail m => TypeName -> ObjectEntry CONST -> CodeGenT m (FieldName, TypeValue)
mapField tName ObjectEntry {..} = do
  t <- lookupFieldType tName entryName
  value <- mapWrappedValue t entryValue
  pure (entryName, value)
