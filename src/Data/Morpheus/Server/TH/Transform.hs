{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.TH.Transform
  ( toTHDefinitions,
    TypeDec (..),
  )
where

import Data.Morpheus.Internal.Utils
  ( capitalTypeName,
    elems,
    empty,
    singleton,
  )
import Data.Morpheus.Server.Internal.TH.Types
  ( ServerConsD,
    ServerFieldDefinition (..),
    ServerTypeDefinition (..),
    toServerField,
  )
import Data.Morpheus.Server.Internal.TH.Utils (isParametrizedResolverType)
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    ArgumentDefinition (..),
    ArgumentsDefinition,
    ConsD (..),
    DataEnumValue (..),
    Description,
    Directives,
    FieldContent (..),
    FieldDefinition (..),
    FieldName (..),
    FieldsDefinition,
    IN,
    OUT,
    TRUE,
    Token,
    TypeContent (..),
    TypeDefinition (..),
    TypeKind (..),
    TypeName (..),
    TypeRef (..),
    UnionMember (..),
    Value,
    hsTypeName,
    isPossibleInterfaceType,
    kindOf,
    mkConsEnum,
    mkTypeRef,
    toFieldName,
  )
import Language.Haskell.TH
import Relude hiding (empty, get)

data TypeDec s
  = InputType
      (ServerTypeDefinition IN s)
  | OutputType (ServerTypeDefinition OUT s)

toTHDefinitions ::
  forall s.
  Bool ->
  [TypeDefinition ANY s] ->
  Q [TypeDec s]
toTHDefinitions namespace schema = concat <$> traverse generateTypes schema
  where
    --------------------------------------------
    generateTypes :: TypeDefinition ANY s -> Q [TypeDec s]
    generateTypes typeDef =
      runReaderT
        (genTypeDefinition typeDef)
        TypeContext
          { toArgsTypeName = mkArgsTypeName namespace (typeName typeDef),
            schema
          }

mkInterfaceName :: TypeName -> TypeName
mkInterfaceName = (<> "Interface")

genTypeDefinition :: TypeDefinition ANY s -> ServerQ s [TypeDec s]
genTypeDefinition
  typeDef@TypeDefinition
    { typeName = originalTypeName,
      typeContent,
      typeDescription
    } = withType <$> genTypeContent originalTypeName typeContent
    where
      typeName = case typeContent of
        DataInterface {} -> mkInterfaceName originalTypeName
        _ -> originalTypeName
      tKind = kindOf typeDef
      tName = hsTypeName typeName
      gqlTypeDescription = typeDescription
      gqlTypeDescriptions = getDesc typeDef
      gqlTypeDirectives = getDirs typeDef
      gqlTypeFieldContents =
        collectFieldValues
          (fmap getDefaultValue . fieldContent)
          (fmap getDefaultValue . fieldContent)
          typeDef
      -------------------------
      withType (ConsIN tCons) = [InputType ServerTypeDefinition {..}]
      withType (ConsOUT others tCons) = OutputType ServerTypeDefinition {..} : others

toHSTypeRef :: TypeRef -> TypeRef
toHSTypeRef TypeRef {typeConName, ..} = TypeRef {typeConName = hsTypeName typeConName, ..}

toHSFieldDefinition :: FieldDefinition cat s -> FieldDefinition cat s
toHSFieldDefinition field = field {fieldType = toHSTypeRef (fieldType field)}

toHSServerFieldDefinition :: ServerFieldDefinition cat s -> ServerFieldDefinition cat s
toHSServerFieldDefinition field = field {originalField = toHSFieldDefinition (originalField field)}

mkCons :: TypeName -> [ServerFieldDefinition cat s] -> ServerConsD cat s
mkCons typename fields =
  ConsD
    { cName = hsTypeName typename,
      cFields = fmap toHSServerFieldDefinition fields
    }

mkObjectCons :: TypeName -> [ServerFieldDefinition cat s] -> [ServerConsD cat s]
mkObjectCons typeName fields = [mkCons typeName fields]

mkArgsTypeName :: Bool -> TypeName -> FieldName -> TypeName
mkArgsTypeName namespace typeName fieldName
  | namespace = hsTypeName typeName <> argTName
  | otherwise = argTName
  where
    argTName = capitalTypeName (fieldName <> "Args")

mkObjectField ::
  FieldDefinition OUT s ->
  ServerQ s (ServerFieldDefinition OUT s)
mkObjectField
  originalField@FieldDefinition
    { fieldName,
      fieldContent,
      fieldType = TypeRef {typeConName}
    } = do
    isParametrized <- lift . isParametrizedResolverType typeConName =<< asks schema
    genName <- asks toArgsTypeName
    pure
      ServerFieldDefinition
        { argumentsTypeName = fieldContent >>= fieldCont genName,
          ..
        }
    where
      fieldCont ::
        (FieldName -> TypeName) ->
        FieldContent TRUE OUT s ->
        Maybe TypeName
      fieldCont genName (FieldArgs args)
        | not (null args) = Just (genName fieldName)
      fieldCont _ _ = Nothing

data BuildPlan s
  = ConsIN [ServerConsD IN s]
  | ConsOUT [TypeDec s] [ServerConsD OUT s]

genInterfaceUnion :: TypeName -> ServerQ s [TypeDec s]
genInterfaceUnion interfaceName = do
  schema' <- asks schema
  let members = map typeName $ catMaybes (isPossibleInterfaceType interfaceName <$> schema')
  pure $
    map
      OutputType
      [ ServerInterfaceDefinition interfaceName (mkInterfaceName interfaceName) tName,
        ServerTypeDefinition
          { tName,
            tCons = map unionCon members,
            tKind = KindUnion,
            gqlTypeDescription = Nothing,
            gqlTypeDescriptions = mempty,
            gqlTypeDirectives = mempty,
            gqlTypeFieldContents = mempty
          }
      ]
  where
    tName = interfaceName <> "PossibleTypes"
    unionCon memberName =
      mkCons
        cName
        $ singleton
          ServerFieldDefinition
            { isParametrized = True,
              argumentsTypeName = Nothing,
              originalField =
                FieldDefinition
                  { fieldName = "un" <> toFieldName cName,
                    fieldType = mkTypeRef utName,
                    fieldDescription = Nothing,
                    fieldDirectives = empty,
                    fieldContent = Nothing
                  }
            }
      where
        cName = hsTypeName tName <> utName
        utName = hsTypeName memberName

genTypeContent ::
  TypeName ->
  TypeContent TRUE ANY s ->
  ServerQ s (BuildPlan s)
genTypeContent _ DataScalar {} = pure (ConsIN [])
genTypeContent _ (DataEnum tags) = pure $ ConsIN (fmap mkConsEnum tags)
genTypeContent typeName (DataInputObject fields) =
  pure $ ConsIN $ mkObjectCons typeName $ map toServerField $ elems fields
genTypeContent _ DataInputUnion {} = fail "Input Unions not Supported"
genTypeContent typeName DataInterface {interfaceFields} =
  ConsOUT
    <$> ((<>) <$> genArgumentTypes interfaceFields <*> genInterfaceUnion typeName)
    <*> ( mkObjectCons (mkInterfaceName typeName)
            <$> traverse mkObjectField (elems interfaceFields)
        )
genTypeContent typeName DataObject {objectFields} =
  ConsOUT <$> genArgumentTypes objectFields
    <*> ( mkObjectCons typeName
            <$> traverse mkObjectField (elems objectFields)
        )
genTypeContent typeName (DataUnion members) =
  pure $ ConsOUT [] (fmap unionCon members)
  where
    unionCon UnionMember {memberName} =
      mkCons
        cName
        $ singleton
          ServerFieldDefinition
            { isParametrized = True,
              argumentsTypeName = Nothing,
              originalField =
                FieldDefinition
                  { fieldName = "un" <> toFieldName cName,
                    fieldType = mkTypeRef utName,
                    fieldDescription = Nothing,
                    fieldDirectives = empty,
                    fieldContent = Nothing
                  }
            }
      where
        cName = hsTypeName typeName <> utName
        utName = hsTypeName memberName

data TypeContext s = TypeContext
  { toArgsTypeName :: FieldName -> TypeName,
    schema :: [TypeDefinition ANY s]
  }

type ServerQ s = ReaderT (TypeContext s) Q

genArgumentTypes :: FieldsDefinition OUT s -> ServerQ s [TypeDec s]
genArgumentTypes = fmap concat . traverse genArgumentType . elems

genArgumentType :: FieldDefinition OUT s -> ServerQ s [TypeDec s]
genArgumentType
  FieldDefinition
    { fieldName,
      fieldContent = Just (FieldArgs arguments)
    }
    | not (null arguments) = do
      tName <- (fieldName &) <$> asks toArgsTypeName
      pure
        [ InputType $
            ServerTypeDefinition
              { tName,
                tCons = [mkCons tName $ map (toServerField . argument) (elems arguments)],
                tKind = KindInputObject,
                gqlTypeDescription = Nothing,
                gqlTypeDescriptions = mempty,
                gqlTypeDirectives = mempty,
                gqlTypeFieldContents = mempty
              }
        ]
genArgumentType _ = pure []

---

getDesc :: TypeDefinition c s -> Map Token Description
getDesc = fromList . get

getDirs :: TypeDefinition c s -> Map Token (Directives s)
getDirs = fromList . get

class Meta a v where
  get :: a -> [(Token, v)]

instance (Meta a v) => Meta (Maybe a) v where
  get (Just x) = get x
  get _ = []

instance
  ( Meta (FieldsDefinition IN s) v,
    Meta (FieldsDefinition OUT s) v,
    Meta (DataEnumValue s) v
  ) =>
  Meta (TypeDefinition c s) v
  where
  get TypeDefinition {typeContent} = get typeContent

instance
  ( Meta (FieldsDefinition IN s) v,
    Meta (FieldsDefinition OUT s) v,
    Meta (DataEnumValue s) v
  ) =>
  Meta (TypeContent a c s) v
  where
  get DataObject {objectFields} = get objectFields
  get DataInputObject {inputObjectFields} = get inputObjectFields
  get DataInterface {interfaceFields} = get interfaceFields
  get DataEnum {enumMembers} = concatMap get enumMembers
  get _ = []

instance Meta (DataEnumValue s) Description where
  get DataEnumValue {enumName, enumDescription = Just x} = [(readTypeName enumName, x)]
  get _ = []

instance Meta (DataEnumValue s) (Directives s) where
  get DataEnumValue {enumName, enumDirectives}
    | null enumDirectives = []
    | otherwise = [(readTypeName enumName, enumDirectives)]

instance
  Meta (FieldDefinition c s) v =>
  Meta (FieldsDefinition c s) v
  where
  get = concatMap get . elems

instance Meta (FieldDefinition c s) Description where
  get FieldDefinition {fieldName, fieldDescription = Just x} = [(readName fieldName, x)]
  get _ = []

instance Meta (FieldDefinition c s) (Directives s) where
  get FieldDefinition {fieldName, fieldDirectives}
    | null fieldDirectives = []
    | otherwise = [(readName fieldName, fieldDirectives)]

collectFieldValues ::
  (FieldDefinition IN s -> Maybe a) ->
  (FieldDefinition OUT s -> Maybe a) ->
  TypeDefinition c s ->
  Map FieldName a
collectFieldValues _ g TypeDefinition {typeContent = DataObject {objectFields}} = getFieldValues g objectFields
collectFieldValues f _ TypeDefinition {typeContent = DataInputObject {inputObjectFields}} = getFieldValues f inputObjectFields
collectFieldValues _ g TypeDefinition {typeContent = DataInterface {interfaceFields}} = getFieldValues g interfaceFields
collectFieldValues _ _ _ = mempty

getFieldValues :: (FieldDefinition c s -> Maybe a) -> FieldsDefinition c s -> Map FieldName a
getFieldValues f = fromList . notNulls . fmap (getFieldValue f) . elems

notNulls :: [(k, Maybe a)] -> [(k, a)]
notNulls [] = []
notNulls ((_, Nothing) : xs) = notNulls xs
notNulls ((name, Just x) : xs) = (name, x) : notNulls xs

getFieldValue :: (FieldDefinition c s -> Maybe a) -> FieldDefinition c s -> (FieldName, Maybe a)
getFieldValue f field = (fieldName field, f field)

getDefaultValue :: FieldContent TRUE c s -> (Maybe (Value s), Maybe (ArgumentsDefinition s))
getDefaultValue DefaultInputValue {defaultInputValue} = (Just defaultInputValue, Nothing)
getDefaultValue (FieldArgs args) = (Nothing, Just args)
