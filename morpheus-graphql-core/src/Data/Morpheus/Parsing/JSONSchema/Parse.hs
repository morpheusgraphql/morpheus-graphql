{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Parsing.JSONSchema.Parse
  ( decodeIntrospection,
  )
where

import Control.Applicative (pure)
import Control.Monad ((>>=))
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Either (Either (..))
import Data.Functor ((<$>), fmap)
import Data.List (concat)
import Data.Maybe (Maybe (..))
import Data.Morpheus.Error.Internal (internalError)
import Data.Morpheus.Internal.Utils
  ( fromElems,
  )
import Data.Morpheus.Parsing.JSONSchema.Types
  ( EnumValue (..),
    Field (..),
    InputValue (..),
    Introspection (..),
    Schema (..),
    Type (..),
  )
import Data.Morpheus.Schema.TypeKind (TypeKind (..))
import Data.Morpheus.Types.IO (JSONResponse (..))
import qualified Data.Morpheus.Types.Internal.AST as AST
  ( Schema,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    ArgumentsDefinition (..),
    CONST,
    DataTypeWrapper (..),
    FieldDefinition,
    IN,
    OUT,
    TypeContent (..),
    TypeDefinition (..),
    TypeName,
    TypeWrapper,
    createScalarType,
    mkEnumContent,
    mkInputValue,
    mkObjectField,
    mkType,
    mkUnionContent,
    msg,
    toAny,
    toHSWrappers,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
  )
import Data.Semigroup ((<>))
import Data.String (String)
import Data.Traversable (traverse)
import Prelude (($), (.), Show (..), uncurry)

decodeIntrospection :: ByteString -> Eventless (AST.Schema CONST)
decodeIntrospection jsonDoc = case jsonSchema of
  Left errors -> internalError $ msg errors
  Right JSONResponse {responseData = Just Introspection {__schema = Schema {types}}} ->
    traverse parse types >>= fromElems . concat
  Right res -> internalError (msg $ show res)
  where
    jsonSchema :: Either String (JSONResponse Introspection)
    jsonSchema = eitherDecode jsonDoc

class ParseJSONSchema a b where
  parse :: a -> Eventless b

instance ParseJSONSchema Type [TypeDefinition ANY CONST] where
  parse Type {name = Just typeName, kind = SCALAR} =
    pure [createScalarType typeName]
  parse Type {name = Just typeName, kind = ENUM, enumValues = Just enums} =
    pure [mkType typeName $ mkEnumContent (fmap enumName enums)]
  parse Type {name = Just typeName, kind = UNION, possibleTypes = Just unions} =
    case traverse name unions of
      Nothing -> internalError "ERROR: GQL ERROR"
      Just uni -> pure [toAny $ mkType typeName $ mkUnionContent uni]
  parse Type {name = Just typeName, kind = INPUT_OBJECT, inputFields = Just iFields} =
    do
      (fields :: [FieldDefinition IN CONST]) <- traverse parse iFields
      fs <- fromElems fields
      pure [mkType typeName $ DataInputObject fs]
  parse Type {name = Just typeName, kind = OBJECT, fields = Just oFields} =
    do
      (fields :: [FieldDefinition OUT CONST]) <- traverse parse oFields
      fs <- fromElems fields
      pure [mkType typeName $ DataObject [] fs]
  parse _ = pure []

instance ParseJSONSchema Field (FieldDefinition OUT CONST) where
  parse Field {fieldName, fieldArgs, fieldType} = do
    (wrappers, typename) <- fieldTypeFromJSON fieldType
    args <- traverse genArg fieldArgs >>= fromElems
    pure $ mkObjectField (ArgumentsDefinition Nothing args) fieldName wrappers typename
    where
      genArg InputValue {inputName = argName, inputType = argType} =
        uncurry (mkInputValue argName) <$> fieldTypeFromJSON argType

instance ParseJSONSchema InputValue (FieldDefinition IN CONST) where
  parse InputValue {inputName, inputType} = uncurry (mkInputValue inputName) <$> fieldTypeFromJSON inputType

fieldTypeFromJSON :: Type -> Eventless ([TypeWrapper], TypeName)
fieldTypeFromJSON = fmap toHs . fieldTypeRec []
  where
    toHs (w, t) = (toHSWrappers w, t)
    fieldTypeRec ::
      [DataTypeWrapper] -> Type -> Eventless ([DataTypeWrapper], TypeName)
    fieldTypeRec acc Type {kind = LIST, ofType = Just ofType} =
      fieldTypeRec (ListType : acc) ofType
    fieldTypeRec acc Type {kind = NON_NULL, ofType = Just ofType} =
      fieldTypeRec (NonNullType : acc) ofType
    fieldTypeRec acc Type {name = Just name} = pure (acc, name)
    fieldTypeRec _ x = internalError $ "Unsuported Field" <> msg (show x)
