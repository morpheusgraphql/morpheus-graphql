{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.TH.Transform
  ( toTHDefinitions,
  )
where

import Data.Morpheus.Internal.Utils
  ( capitalTypeName,
    elems,
    singleton,
  )
import Data.Morpheus.Server.Internal.TH.Types
  ( GQLTypeDefinition (..),
    ServerConsD,
    ServerFieldDefinition (..),
    ServerTypeDefinition (..),
    toServerField,
  )
import Data.Morpheus.Server.Internal.TH.Utils
  ( isParametrizedResolverType,
    kindName,
    m_,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    ArgumentDefinition (..),
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
    isResolverType,
    kindOf,
    mkConsEnum,
    mkTypeRef,
    toFieldName,
  )
import Language.Haskell.TH
import Relude hiding (empty, get)

toTHDefinitions ::
  forall s.
  Bool ->
  [TypeDefinition ANY s] ->
  Q [ServerTypeDefinition s]
toTHDefinitions namespace schema = concat <$> traverse generateTypes schema
  where
    --------------------------------------------
    generateTypes :: TypeDefinition ANY s -> Q [ServerTypeDefinition s]
    generateTypes typeDef =
      runReaderT
        (genTypeDefinition typeDef)
        TypeContext
          { toArgsTypeName = mkArgsTypeName namespace (typeName typeDef),
            schema
          }

mkInterfaceName :: TypeName -> TypeName
mkInterfaceName = ("Interface" <>)

mkPossibleTypesName :: TypeName -> TypeName
mkPossibleTypesName = ("PossibleTypes" <>)

genTypeDefinition :: TypeDefinition ANY s -> ServerQ s [ServerTypeDefinition s]
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
      -------------------------
      withType (ConsIN tCons) = [ServerTypeDefinition {..}]
      withType (ConsOUT others tCons) = ServerTypeDefinition {..} : others

toHSTypeRef :: TypeRef -> TypeRef
toHSTypeRef TypeRef {typeConName, ..} = TypeRef {typeConName = hsTypeName typeConName, ..}

toHSServerFieldDefinition :: ServerFieldDefinition -> ServerFieldDefinition
toHSServerFieldDefinition ServerFieldDefinition {..} = ServerFieldDefinition {fieldType = toHSTypeRef fieldType, ..}

mkCons :: TypeName -> [ServerFieldDefinition] -> ServerConsD
mkCons typename fields =
  ConsD
    { cName = hsTypeName typename,
      cFields = fmap toHSServerFieldDefinition fields
    }

mkObjectCons :: TypeName -> [ServerFieldDefinition] -> [ServerConsD]
mkObjectCons typeName fields = [mkCons typeName fields]

mkArgsTypeName :: Bool -> TypeName -> FieldName -> TypeName
mkArgsTypeName namespace typeName fieldName
  | namespace = hsTypeName typeName <> argTName
  | otherwise = argTName
  where
    argTName = capitalTypeName (fieldName <> "Args")

mkObjectField ::
  FieldDefinition OUT s ->
  ServerQ s ServerFieldDefinition
mkObjectField
  FieldDefinition
    { fieldName,
      fieldContent,
      fieldType
    } = do
    isParametrized <- lift . isParametrizedResolverType (typeConName fieldType) =<< asks schema
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
  = ConsIN [ServerConsD]
  | ConsOUT [ServerTypeDefinition s] [ServerConsD]

genInterfaceUnion :: TypeName -> ServerQ s [ServerTypeDefinition s]
genInterfaceUnion interfaceName =
  mkInterface . map typeName . mapMaybe (isPossibleInterfaceType interfaceName)
    <$> asks schema
  where
    mkInterface [] = []
    mkInterface [possibleTypeName] = [mkGuardWithPossibleType possibleTypeName]
    mkInterface members =
      [ mkGuardWithPossibleType tName,
        ServerTypeDefinition
          { tName,
            tCons = map unionCon members,
            tKind = KindUnion,
            typeParameters = [m_],
            gql = Nothing
          }
      ]
    mkGuardWithPossibleType = ServerInterfaceDefinition interfaceName (mkInterfaceName interfaceName)
    tName = mkPossibleTypesName interfaceName
    unionCon memberName =
      mkCons
        cName
        $ singleton
          ServerFieldDefinition
            { isParametrized = True,
              argumentsTypeName = Nothing,
              fieldName = "un" <> toFieldName cName,
              fieldType = mkTypeRef utName
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
              fieldName = "un" <> toFieldName cName,
              fieldType = mkTypeRef utName
            }
      where
        cName = hsTypeName typeName <> utName
        utName = hsTypeName memberName

data TypeContext s = TypeContext
  { toArgsTypeName :: FieldName -> TypeName,
    schema :: [TypeDefinition ANY s]
  }

type ServerQ s = ReaderT (TypeContext s) Q

genArgumentTypes :: FieldsDefinition OUT s -> ServerQ s [ServerTypeDefinition s]
genArgumentTypes = fmap concat . traverse genArgumentType . elems

genArgumentType :: FieldDefinition OUT s -> ServerQ s [ServerTypeDefinition s]
genArgumentType
  FieldDefinition
    { fieldName,
      fieldContent = Just (FieldArgs arguments)
    }
    | not (null arguments) = do
      tName <- (fieldName &) <$> asks toArgsTypeName
      let argumentFields = argument <$> elems arguments
      pure
        [ ServerTypeDefinition
            { tName,
              tCons = [mkCons tName (toServerField <$> argumentFields)],
              tKind = KindInputObject,
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
mkFieldDescription FieldDefinition {..} = (readName fieldName,) <$> fieldDescription

mkFieldDirective :: FieldDefinition cat s -> (Text, Directives s)
mkFieldDirective FieldDefinition {..} = (readName fieldName, fieldDirectives)

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

getInputFields :: TypeDefinition c s -> [FieldDefinition IN s]
getInputFields TypeDefinition {typeContent = DataInputObject {inputObjectFields}} = elems inputObjectFields
getInputFields _ = []

getDefaultValue :: FieldDefinition c s -> Maybe (Text, Value s)
getDefaultValue FieldDefinition {fieldName, fieldContent = Just DefaultInputValue {defaultInputValue}} = Just (readName fieldName, defaultInputValue)
getDefaultValue _ = Nothing
