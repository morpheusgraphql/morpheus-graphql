{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Data.Morpheus.Rendering.RenderIntrospection
  ( render,
    createObjectType,
  )
where

import Data.Maybe (isJust)
-- Morpheus

import Data.Morpheus.Schema.Schema
import Data.Morpheus.Schema.TypeKind (TypeKind (..))
import Data.Morpheus.Types.Internal.AST
  ( ArgumentsDefinition (..),
    DataEnumValue (..),
    DataInputUnion,
    DataInputUnion,
    DataTypeKind (..),
    DataTypeWrapper (..),
    DataUnion,
    Directive,
    FieldDefinition (..),
    Meta (..),
    Name,
    QUERY,
    ScalarValue (..),
    Schema,
    TypeContent (..),
    TypeDefinition (..),
    TypeRef (..),
    convertToJSONName,
    createInputUnionFields,
    fieldVisibility,
    kindOf,
    lookupDataType,
    lookupDeprecated,
    lookupDeprecatedReason,
    toGQLWrapper,
  )
import Data.Morpheus.Types.Internal.Operation
  ( Listable (..),
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Failure (..),
    ObjectResModel (..),
    ResModel (..),
    Resolver,
  )
import Data.Semigroup ((<>))
import Data.Text (Text, pack)

constRes :: Applicative m => a -> b -> m a
constRes = const . pure

type Result e m a = Schema -> Resolver QUERY e m a

class RenderSchema a where
  render :: (Monad m) => a -> Schema -> Resolver QUERY e m (ResModel QUERY e m)

instance RenderSchema TypeDefinition where
  render TypeDefinition {typeName, typeMeta, typeContent} = __render typeContent
    where
      __render ::
        (Monad m) => TypeContent -> Schema -> Resolver QUERY e m (ResModel QUERY e m)
      __render DataScalar {} =
        constRes $ createLeafType SCALAR typeName typeMeta Nothing
      __render (DataEnum enums) =
        constRes $
          createLeafType ENUM typeName typeMeta (Just $ map createEnumValue enums)
      __render (DataInputObject fields) = \lib ->
        createInputObject typeName typeMeta
          <$> traverse (`renderinputValue` lib) (toList fields)
      __render DataObject {objectFields} = \lib ->
        createObjectType typeName (typeMeta >>= metaDescription)
          <$> (Just <$> traverse (`render` lib) (filter fieldVisibility $ toList objectFields))
      __render (DataUnion union) =
        constRes $ typeFromUnion (typeName, typeMeta, union)
      __render (DataInputUnion members) =
        renderInputUnion (typeName, typeMeta, members)
      __render _ = undefined --TODO.

createEnumValue :: Monad m => DataEnumValue -> ResModel QUERY e m
createEnumValue DataEnumValue {enumName, enumMeta} =
  ResObject $
    ObjectResModel
      { __typename = "__Field",
        objectFields =
          [ ("name", resString enumName),
            description enumMeta
          ]
            <> renderDeprecated enumMeta
      }

renderDeprecated ::
  (Monad m) =>
  Maybe Meta ->
  [(Name, Resolver QUERY e m (ResModel QUERY e m))]
renderDeprecated meta =
  [ ("isDeprecated", pure $ ResScalar $ Boolean (isJust $ meta >>= lookupDeprecated)),
    ("deprecationReason", opt resString (meta >>= lookupDeprecated >>= lookupDeprecatedReason))
  ]

description :: Monad m => Maybe Meta -> (Name, Resolver QUERY e m (ResModel QUERY e m))
description enumMeta = ("description", opt resString (enumMeta >>= metaDescription))

string :: Text -> ResModel o e m
string = ResScalar . String

-- renderArguments :: (Monad m, Failure Text m) => ArgumentsDefinition -> Schema -> Resolver QUERY e m [ResModel QUERY e m]
-- renderArguments ArgumentsDefinition {arguments} lib = traverse (`renderinputValue` lib) $ toList arguments
-- renderArguments NoArguments _ = pure []

instance RenderSchema FieldDefinition where
  render field@FieldDefinition {fieldName, fieldType = TypeRef {typeConName}, fieldArgs, fieldMeta} lib =
    do
      kind <- renderTypeKind <$> lookupKind typeConName lib
      pure $
        ResObject
          ( ObjectResModel
              { __typename = "__Field",
                objectFields =
                  [ ("name", resString (convertToJSONName fieldName)),
                    ("description", opt resString (fieldMeta >>= metaDescription))
                    -- s__FieldArgs = renderArguments fieldArgs lib,
                    -- s__FieldType' =
                    --   pure (applyTypeWrapper field $ createType kind typeConName Nothing $ Just []),
                  ]
                    <> renderDeprecated fieldMeta
              }
          )

renderTypeKind :: DataTypeKind -> TypeKind
renderTypeKind KindScalar = SCALAR
renderTypeKind (KindObject _) = OBJECT
renderTypeKind KindUnion = UNION
renderTypeKind KindInputUnion = INPUT_OBJECT
renderTypeKind KindEnum = ENUM
renderTypeKind KindInputObject = INPUT_OBJECT
renderTypeKind KindList = LIST
renderTypeKind KindNonNull = NON_NULL

applyTypeWrapper :: Monad m => FieldDefinition -> ResModel QUERY e m -> ResModel QUERY e m
applyTypeWrapper FieldDefinition {fieldType = TypeRef {typeWrappers}} typ =
  foldr wrapByTypeWrapper typ (toGQLWrapper typeWrappers)

wrapByTypeWrapper :: Monad m => DataTypeWrapper -> ResModel QUERY e m -> ResModel QUERY e m
wrapByTypeWrapper ListType = wrapAs LIST
wrapByTypeWrapper NonNullType = wrapAs NON_NULL

lookupKind :: (Monad m) => Text -> Result e m DataTypeKind
lookupKind name lib = case lookupDataType name lib of
  Nothing -> failure $ "Kind Not Found: " <> name
  Just value -> pure (kindOf value)

renderinputValue ::
  (Monad m) =>
  FieldDefinition ->
  Result e m (ResModel QUERY e m)
renderinputValue input = fmap (createInputValueWith (fieldName input) (fieldMeta input)) . createInputObjectType input

createInputObjectType ::
  (Monad m) => FieldDefinition -> Result e m (ResModel QUERY e m)
createInputObjectType field@FieldDefinition {fieldType = TypeRef {typeConName}} lib =
  do
    kind <- renderTypeKind <$> lookupKind typeConName lib
    pure $ applyTypeWrapper field $ createType kind typeConName Nothing $ Just []

renderInputUnion ::
  (Monad m) =>
  (Text, Maybe Meta, DataInputUnion) ->
  Result e m (ResModel QUERY e m)
renderInputUnion (key, meta, fields) lib =
  createInputObject key meta
    <$> traverse
      createField
      (createInputUnionFields key $ map fst $ filter snd fields)
  where
    createField field =
      createInputValueWith (fieldName field) Nothing <$> createInputObjectType field lib

createLeafType ::
  Monad m =>
  TypeKind ->
  Text ->
  Maybe Meta ->
  Maybe [ResModel QUERY e m] ->
  ResModel QUERY e m
createLeafType kind name meta enums =
  ResObject
    ( ObjectResModel
        { __typename = "__Type",
          objectFields =
            [ ("kind", renderKind kind),
              ("name", resString name),
              description meta,
              ("enumValues", optList enums)
            ]
        }
    )

typeFromUnion :: Monad m => (Text, Maybe Meta, DataUnion) -> ResModel QUERY e m
typeFromUnion (name, typeMeta, typeContent) =
  ResObject
    ( ObjectResModel
        { __typename = "__Type",
          objectFields =
            [ ("kind", renderKind UNION),
              ("name", resString name),
              description typeMeta
              --   s__TypePossibleTypes =
              --     pure $ Just (map (\x -> createObjectType x Nothing $ Just []) typeContent),
            ]
        }
    )

createObjectType ::
  Monad m => Text -> Maybe Text -> Maybe [ResModel QUERY e m] -> ResModel QUERY e m
createObjectType name desc fields =
  ResObject
    ( ObjectResModel
        { __typename = "__Type",
          objectFields =
            [ ("kind", renderKind OBJECT),
              ("name", resString name),
              ("description", opt resString desc),
              ("fields", optList fields),
              ("interfaces", pure $ ResList [])
            ]
        }
    )

optList :: Monad m => Maybe [ResModel QUERY e m] -> Resolver QUERY e m (ResModel QUERY e m)
optList = pure . maybe ResNull ResList

createInputObject ::
  Monad m => Text -> Maybe Meta -> [ResModel QUERY e m] -> ResModel QUERY e m
createInputObject name meta fields =
  ResObject
    ( ObjectResModel
        { __typename = "__Type",
          objectFields =
            [ ("kind", resString "INPUT_OBJECT"),
              ("name", resString name),
              description meta,
              ("inputFields", pure $ ResList fields)
            ]
        }
    )

createType ::
  Monad m =>
  TypeKind ->
  Text ->
  Maybe Text ->
  Maybe [ResModel QUERY e m] ->
  ResModel QUERY e m
createType kind name desc fields =
  ResObject
    ( ObjectResModel
        { __typename = "__Type",
          objectFields =
            [ ("kind", renderKind kind),
              ("name", resString name),
              ("description", opt resString desc),
              ("fields", pure $ maybe ResNull ResList fields),
              ("enumValues", pure $ ResList [])
            ]
        }
    )

opt :: Monad m => (a -> Resolver QUERY e m (ResModel QUERY e m)) -> Maybe a -> Resolver QUERY e m (ResModel QUERY e m)
opt f (Just x) = f x
opt _ Nothing = pure ResNull

resString :: Monad m => Text -> Resolver QUERY e m (ResModel QUERY e m)
resString = pure . string

renderKind :: Monad m => TypeKind -> Resolver QUERY e m (ResModel QUERY e m)
renderKind = resString . pack . show

wrapAs :: Monad m => TypeKind -> ResModel QUERY e m -> ResModel QUERY e m
wrapAs kind contentType =
  ResObject
    ( ObjectResModel
        { __typename = "__Type",
          objectFields =
            [ ("kind", renderKind kind),
              ("ofType", pure contentType)
            ]
        }
    )

createInputValueWith ::
  Monad m => Text -> Maybe Meta -> ResModel QUERY e m -> ResModel QUERY e m
createInputValueWith name meta ivType =
  ResObject
    ( ObjectResModel
        { __typename = "__InputValue",
          objectFields =
            [ ("name", resString $ convertToJSONName name),
              description meta,
              ("type", pure ivType)
            ]
        }
    )
