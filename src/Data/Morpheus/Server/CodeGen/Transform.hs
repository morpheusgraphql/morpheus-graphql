{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.CodeGen.Transform
  ( parseServerTypeDefinitions,
  )
where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Morpheus.Core
  ( parseTypeDefinitions,
  )
import Data.Morpheus.Error (gqlWarnings, renderGQLErrors)
import Data.Morpheus.Internal.Ext (Result (..))
import Data.Morpheus.Internal.TH (camelCaseFieldName, toName)
import Data.Morpheus.Internal.Utils
  ( camelCaseTypeName,
  )
import Data.Morpheus.Server.CodeGen.Types
  ( FIELD_TYPE_WRAPPER (..),
    GQLTypeDefinition (..),
    ServerConstructorDefinition (..),
    ServerDecContext (..),
    ServerFieldDefinition (..),
    ServerTypeDefinition (..),
  )
import Data.Morpheus.Server.TH.Utils
  ( isParametrizedHaskellType,
    kindName,
    m_,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    ArgumentDefinition (..),
    CONST,
    DataEnumValue (..),
    Description,
    Directives,
    FieldContent (..),
    FieldDefinition (..),
    FieldName,
    FieldsDefinition,
    GQLError,
    IN,
    OUT,
    TRUE,
    Token,
    TypeContent (..),
    TypeDefinition (..),
    TypeKind (..),
    TypeName,
    TypeRef (..),
    UnionMember (..),
    Value,
    isPossibleInterfaceType,
    isResolverType,
    kindOf,
    lookupWith,
    mkTypeRef,
    unpackName,
  )
import Language.Haskell.TH
import Relude hiding (ByteString, empty, get)

type ServerQ m s = ReaderT (TypeContext s) m

class (Monad m, MonadFail m) => CodeGenMonad m where
  isParametrizedType :: TypeName -> m Bool
  printWarnings :: [GQLError] -> m ()

instance CodeGenMonad Q where
  isParametrizedType name = isParametrizedHaskellType <$> reify (toName name)
  printWarnings = gqlWarnings

instance CodeGenMonad (Either String) where
  isParametrizedType _ = pure False
  printWarnings _ = pure ()

data TypeContext s = TypeContext
  { toArgsTypeName :: FieldName -> TypeName,
    schema :: [TypeDefinition ANY s],
    currentTypeName :: TypeName,
    hasNamespace :: Bool,
    currentKind :: Maybe TypeKind
  }

parseServerTypeDefinitions :: CodeGenMonad m => ServerDecContext -> ByteString -> m [ServerTypeDefinition CONST]
parseServerTypeDefinitions ctx txt =
  case parseTypeDefinitions txt of
    Failure errors -> fail (renderGQLErrors errors)
    Success {result = schema, warnings} -> printWarnings warnings >> toTHDefinitions (namespace ctx) schema

toTHDefinitions ::
  forall s m.
  CodeGenMonad m =>
  Bool ->
  [TypeDefinition ANY s] ->
  m [ServerTypeDefinition s]
toTHDefinitions namespace schema = concat <$> traverse generateTypes schema
  where
    --------------------------------------------
    generateTypes :: TypeDefinition ANY s -> m [ServerTypeDefinition s]
    generateTypes typeDef =
      runReaderT
        (genTypeDefinition typeDef)
        TypeContext
          { toArgsTypeName = mkArgsTypeName namespace (typeName typeDef),
            schema,
            currentTypeName = typeName typeDef,
            currentKind = Just (kindOf typeDef),
            hasNamespace = namespace
          }

inType :: MonadReader (TypeContext s) m => TypeName -> m a -> m a
inType currentTypeName = local (\x -> x {currentTypeName, currentKind = Nothing})

mkInterfaceName :: TypeName -> TypeName
mkInterfaceName = ("Interface" <>)

mkPossibleTypesName :: TypeName -> TypeName
mkPossibleTypesName = ("PossibleTypes" <>)

genTypeDefinition ::
  CodeGenMonad m =>
  TypeDefinition ANY s ->
  ServerQ m s [ServerTypeDefinition s]
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
      tName = typeName
      gql =
        Just
          GQLTypeDefinition
            { gqlTypeDescription = typeDescription,
              gqlTypeDescriptions = getDesc typeDef,
              gqlTypeDirectives = getDirs typeDef,
              gqlKind = kindName tKind,
              gqlTypeDefaultValues =
                fromList
                  $ mapMaybe getDefaultValue
                  $ getInputFields typeDef
            }
      typeParameters
        | isResolverType tKind = [m_]
        | otherwise = []
      derives = derivesClasses tKind
      -------------------------
      withType (ConsIN tCons) = [ServerTypeDefinition {..}]
      withType (ConsOUT others tCons) = ServerTypeDefinition {..} : others

derivesClasses :: TypeKind -> [Name]
derivesClasses tKind = ''Generic : derivingList
  where
    derivingList
      | isResolverType tKind = []
      | otherwise = [''Show]

mkObjectCons :: TypeName -> [ServerFieldDefinition] -> [ServerConstructorDefinition]
mkObjectCons name = pure . ServerConstructorDefinition name

mkArgsTypeName :: Bool -> TypeName -> FieldName -> TypeName
mkArgsTypeName namespace typeName fieldName
  | namespace = typeName <> argTName
  | otherwise = argTName
  where
    argTName = camelCaseTypeName [fieldName] "Args"

isParametrizedResolverType :: CodeGenMonad m => TypeName -> [TypeDefinition ANY s] -> m Bool
isParametrizedResolverType "__TypeKind" _ = pure False
isParametrizedResolverType "Boolean" _ = pure False
isParametrizedResolverType "String" _ = pure False
isParametrizedResolverType "Int" _ = pure False
isParametrizedResolverType "Float" _ = pure False
isParametrizedResolverType name lib = case lookupWith typeName name lib of
  Just x -> pure (isResolverType x)
  Nothing -> isParametrizedType name

mkObjectField ::
  CodeGenMonad m =>
  FieldDefinition OUT s ->
  ServerQ m s ServerFieldDefinition
mkObjectField
  FieldDefinition
    { fieldName = fName,
      fieldContent,
      fieldType
    } = do
    isParametrized <- lift . isParametrizedResolverType (typeConName fieldType) =<< asks schema
    genName <- asks toArgsTypeName
    let wrappers = [MONAD]
    fieldName <- genFieldName fName
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
        | not (null args) = Just (genName fName)
      fieldCont _ _ = Nothing

data BuildPlan s
  = ConsIN [ServerConstructorDefinition]
  | ConsOUT [ServerTypeDefinition s] [ServerConstructorDefinition]

genInterfaceUnion :: Monad m => TypeName -> ServerQ m s [ServerTypeDefinition s]
genInterfaceUnion interfaceName =
  mkInterface . map typeName . mapMaybe (isPossibleInterfaceType interfaceName)
    <$> asks schema
  where
    tKind = KindUnion
    mkInterface [] = []
    mkInterface [possibleTypeName] = [mkGuardWithPossibleType possibleTypeName]
    mkInterface members =
      [ mkGuardWithPossibleType tName,
        ServerTypeDefinition
          { tName,
            tCons = map (mkUnionFieldDefinition tName) members,
            tKind,
            typeParameters = [m_],
            derives = derivesClasses tKind,
            gql = Nothing
          }
      ]
    mkGuardWithPossibleType = ServerInterfaceDefinition interfaceName (mkInterfaceName interfaceName)
    tName = mkPossibleTypesName interfaceName

genFieldName :: Monad m => FieldName -> ServerQ m s FieldName
genFieldName fieldName = do
  TypeContext {hasNamespace, currentTypeName} <- ask
  pure $
    if hasNamespace
      then camelCaseFieldName currentTypeName fieldName
      else fieldName

mkConsEnum :: Monad m => TypeName -> DataEnumValue s -> ServerQ m s ServerConstructorDefinition
mkConsEnum name DataEnumValue {enumName} = do
  namespace <- asks hasNamespace
  pure
    ServerConstructorDefinition
      { constructorName =
          if namespace
            then camelCaseTypeName [name] enumName
            else enumName,
        constructorFields = []
      }

toNonResolverServerField :: Monad m => FieldDefinition c s -> ServerQ m s ServerFieldDefinition
toNonResolverServerField FieldDefinition {fieldType, fieldName = fName} = do
  fieldName <- genFieldName fName
  pure $
    ServerFieldDefinition
      { isParametrized = False,
        argumentsTypeName = Nothing,
        fieldType,
        fieldName,
        wrappers = []
      }

genTypeContent ::
  CodeGenMonad m =>
  TypeName ->
  TypeContent TRUE ANY s ->
  ServerQ m s (BuildPlan s)
genTypeContent _ DataScalar {} = pure (ConsIN [])
genTypeContent typeName (DataEnum tags) = ConsIN <$> traverse (mkConsEnum typeName) tags
genTypeContent typeName (DataInputObject fields) =
  ConsIN . mkObjectCons typeName <$> traverse toNonResolverServerField (toList fields)
genTypeContent _ DataInputUnion {} = fail "Input Unions not Supported"
genTypeContent typeName DataInterface {interfaceFields} =
  ConsOUT
    <$> ((<>) <$> genArgumentTypes interfaceFields <*> genInterfaceUnion typeName)
    <*> ( do
            let interfaceName = mkInterfaceName typeName
            inType
              interfaceName
              ( mkObjectCons interfaceName
                  <$> traverse mkObjectField (toList interfaceFields)
              )
        )
genTypeContent typeName DataObject {objectFields} =
  ConsOUT <$> genArgumentTypes objectFields
    <*> ( mkObjectCons typeName
            <$> traverse mkObjectField (toList objectFields)
        )
genTypeContent typeName (DataUnion members) =
  pure $ ConsOUT [] (unionCon <$> toList members)
  where
    unionCon UnionMember {memberName} = mkUnionFieldDefinition typeName memberName

mkUnionFieldDefinition :: TypeName -> TypeName -> ServerConstructorDefinition
mkUnionFieldDefinition typeName memberName =
  ServerConstructorDefinition
    { constructorName,
      constructorFields =
        [ ServerFieldDefinition
            { isParametrized = True,
              argumentsTypeName = Nothing,
              fieldName = coerce ("un" <> constructorName),
              fieldType = mkTypeRef memberName,
              wrappers = []
            }
        ]
    }
  where
    constructorName = camelCaseTypeName [typeName] memberName

genArgumentTypes :: Monad m => FieldsDefinition OUT s -> ServerQ m s [ServerTypeDefinition s]
genArgumentTypes = fmap concat . traverse genArgumentType . toList

genArgumentType :: Monad m => FieldDefinition OUT s -> ServerQ m s [ServerTypeDefinition s]
genArgumentType
  FieldDefinition
    { fieldName,
      fieldContent = Just (FieldArgs arguments)
    }
    | not (null arguments) = do
      tName <- (fieldName &) <$> asks toArgsTypeName
      inType tName $ do
        let argumentFields = argument <$> toList arguments
        fields <- traverse toNonResolverServerField argumentFields
        let tKind = KindInputObject
        pure
          [ ServerTypeDefinition
              { tName,
                tKind,
                tCons = mkObjectCons tName fields,
                derives = derivesClasses tKind,
                typeParameters = [],
                gql =
                  Just
                    ( GQLTypeDefinition
                        { gqlKind = kindName KindInputObject,
                          gqlTypeDescription = Nothing,
                          gqlTypeDescriptions = fromList (mapMaybe mkFieldDescription argumentFields),
                          gqlTypeDirectives = fromList (mkFieldDirective <$> argumentFields),
                          gqlTypeDefaultValues = fromList (mapMaybe getDefaultValue argumentFields)
                        }
                    )
              }
          ]
genArgumentType _ = pure []

mkFieldDescription :: FieldDefinition cat s -> Maybe (Text, Description)
mkFieldDescription FieldDefinition {..} = (unpackName fieldName,) <$> fieldDescription

mkFieldDirective :: FieldDefinition cat s -> (Text, Directives s)
mkFieldDirective FieldDefinition {..} = (unpackName fieldName, fieldDirectives)

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
  get DataEnumValue {enumName, enumDescription = Just x} = [(unpackName enumName, x)]
  get _ = []

instance Meta (DataEnumValue s) (Directives s) where
  get DataEnumValue {enumName, enumDirectives}
    | null enumDirectives = []
    | otherwise = [(unpackName enumName, enumDirectives)]

instance
  Meta (FieldDefinition c s) v =>
  Meta (FieldsDefinition c s) v
  where
  get = concatMap get . toList

instance Meta (FieldDefinition c s) Description where
  get FieldDefinition {fieldName, fieldDescription = Just x} = [(unpackName fieldName, x)]
  get _ = []

instance Meta (FieldDefinition c s) (Directives s) where
  get FieldDefinition {fieldName, fieldDirectives}
    | null fieldDirectives = []
    | otherwise = [(unpackName fieldName, fieldDirectives)]

getInputFields :: TypeDefinition c s -> [FieldDefinition IN s]
getInputFields TypeDefinition {typeContent = DataInputObject {inputObjectFields}} = toList inputObjectFields
getInputFields _ = []

getDefaultValue :: FieldDefinition c s -> Maybe (Text, Value s)
getDefaultValue
  FieldDefinition
    { fieldName,
      fieldContent = Just DefaultInputValue {defaultInputValue}
    } = Just (unpackName fieldName, defaultInputValue)
getDefaultValue _ = Nothing
