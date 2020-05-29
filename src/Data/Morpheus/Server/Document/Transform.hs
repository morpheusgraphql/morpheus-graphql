{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Data.Morpheus.Server.Document.Transform
  ( toTHDefinitions,
  )
where

-- MORPHEUS

import Data.Maybe (catMaybes)
import Data.Morpheus.Internal.TH
  ( infoTyVars,
    mkTypeName,
  )
import Data.Morpheus.Internal.Utils
  ( capitalTypeName,
    elems,
    empty,
    singleton,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    ArgumentsDefinition (..),
    ConsD,
    FieldContent (..),
    FieldDefinition (..),
    FieldName,
    Fields (..),
    FieldsDefinition,
    GQLTypeD (..),
    OUT,
    TRUE,
    TypeContent (..),
    TypeD (..),
    TypeDefinition (..),
    TypeKind (..),
    TypeName,
    TypeRef (..),
    hasArguments,
    hsTypeName,
    kindOf,
    lookupWith,
    mkCons,
    mkConsEnum,
    toFieldName,
  )
import Data.Semigroup ((<>))
import Language.Haskell.TH

m_ :: TypeName
m_ = "m"

getTypeArgs :: TypeName -> [TypeDefinition ANY] -> Q (Maybe TypeName)
getTypeArgs "__TypeKind" _ = pure Nothing
getTypeArgs "Boolean" _ = pure Nothing
getTypeArgs "String" _ = pure Nothing
getTypeArgs "Int" _ = pure Nothing
getTypeArgs "Float" _ = pure Nothing
getTypeArgs key lib = case typeContent <$> lookupWith typeName key lib of
  Just x -> pure (kindToTyArgs x)
  Nothing -> getTyArgs <$> reify (mkTypeName key)

getTyArgs :: Info -> Maybe TypeName
getTyArgs x
  | null (infoTyVars x) = Nothing
  | otherwise = Just m_

kindToTyArgs :: TypeContent TRUE ANY -> Maybe TypeName
kindToTyArgs DataObject {} = Just m_
kindToTyArgs DataUnion {} = Just m_
kindToTyArgs DataInterface {} = Just m_
kindToTyArgs _ = Nothing

toTHDefinitions :: Bool -> [TypeDefinition ANY] -> Q [GQLTypeD]
toTHDefinitions namespace schema = catMaybes <$> traverse renderTHType schema
  where
    renderTHType :: TypeDefinition ANY -> Q (Maybe GQLTypeD)
    renderTHType x = generateType x
      where
        toArgsTypeName :: FieldName -> TypeName
        toArgsTypeName = mkArgsTypeName namespace (typeName x)
        --------------------------------------------
        generateType :: TypeDefinition ANY -> Q (Maybe GQLTypeD)
        generateType
          typeOriginal@TypeDefinition
            { typeName,
              typeContent,
              typeDescription,
              typeDirectives
            } =
            fmap defType <$> genTypeContent schema toArgsTypeName typeName typeContent
            where
              -----------------------
              defType (typeArgD, tCons) =
                GQLTypeD
                  { typeD =
                      TypeD
                        { tName = hsTypeName typeName,
                          tDescription = typeDescription,
                          tNamespace = [],
                          tCons,
                          tKind = kindOf typeOriginal,
                          tDirectives = typeDirectives
                        },
                    typeArgD,
                    ..
                  }

mkObjectCons :: TypeName -> FieldsDefinition cat -> [ConsD]
mkObjectCons typeName fields = [mkCons typeName fields]

mkArgsTypeName :: Bool -> TypeName -> FieldName -> TypeName
mkArgsTypeName namespace typeName fieldName
  | namespace = hsTypeName typeName <> argTName
  | otherwise = argTName
  where
    argTName = capitalTypeName (fieldName <> "Args")

mkField :: [TypeDefinition ANY] -> (FieldName -> TypeName) -> FieldDefinition OUT -> Q (FieldDefinition OUT)
mkField schema genArgsTypeName field@FieldDefinition {fieldName, fieldContent = cont, fieldType = typeRef@TypeRef {typeConName}} =
  do
    typeArgs <- getTypeArgs typeConName schema
    pure $
      field
        { fieldType = typeRef {typeConName = hsTypeName typeConName, typeArgs},
          fieldContent = fieldCont cont
        }
  where
    fieldCont (FieldArgs args)
      | hasArguments args = FieldArgs $ args {argumentsTypename = Just $ genArgsTypeName fieldName}
      | otherwise = cont
    fieldCont _ = cont

genTypeContent ::
  [TypeDefinition ANY] ->
  (FieldName -> TypeName) ->
  TypeName ->
  TypeContent TRUE ANY ->
  Q (Maybe ([TypeD], [ConsD]))
genTypeContent _ _ _ DataScalar {} = pure $ Just ([], [])
genTypeContent _ _ _ (DataEnum tags) = pure $ Just ([], map mkConsEnum tags)
genTypeContent _ _ typeName (DataInputObject fields) = pure $ Just ([], mkObjectCons typeName fields)
genTypeContent _ _ _ DataInputUnion {} = fail "Input Unions not Supported"
genTypeContent schema toArgsTyName typeName DataInterface {interfaceFields} = do
  typeArgD <- genArgumentTypes toArgsTyName interfaceFields
  objCons <- mkObjectCons typeName <$> traverse (mkField schema toArgsTyName) interfaceFields
  pure $ Just (typeArgD, objCons)
genTypeContent schema toArgsTyName typeName DataObject {objectFields} = do
  typeArgD <- genArgumentTypes toArgsTyName objectFields
  objCons <- mkObjectCons typeName <$> traverse (mkField schema toArgsTyName) objectFields
  pure $ Just (typeArgD, objCons)
genTypeContent _ _ typeName (DataUnion members) = pure $ Just ([], map unionCon members)
  where
    unionCon memberName =
      mkCons
        cName
        ( singleton
            FieldDefinition
              { fieldName = "un" <> toFieldName cName,
                fieldType =
                  TypeRef
                    { typeConName = utName,
                      typeArgs = Just m_,
                      typeWrappers = []
                    },
                fieldDescription = Nothing,
                fieldDirectives = empty,
                fieldContent = NoContent
              }
        )
      where
        cName = hsTypeName typeName <> utName
        utName = hsTypeName memberName

genArgumentTypes :: (FieldName -> TypeName) -> FieldsDefinition OUT -> Q [TypeD]
genArgumentTypes genArgsTypeName fields =
  concat <$> traverse (genArgumentType genArgsTypeName) (elems fields)

genArgumentType :: (FieldName -> TypeName) -> FieldDefinition OUT -> Q [TypeD]
genArgumentType namespaceWith FieldDefinition {fieldName, fieldContent = FieldArgs ArgumentsDefinition {arguments}} =
  pure
    [ TypeD
        { tName,
          tNamespace = empty,
          tCons = [mkCons tName (Fields arguments)],
          tDescription = Nothing,
          tDirectives = empty,
          tKind = KindInputObject
        }
    ]
  where
    tName = hsTypeName (namespaceWith fieldName)
genArgumentType _ _ = pure []
