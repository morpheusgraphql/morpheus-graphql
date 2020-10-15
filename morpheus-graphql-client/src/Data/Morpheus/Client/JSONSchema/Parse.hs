{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Morpheus.Client.JSONSchema.Parse
  ( decodeIntrospection,
  )
where

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Morpheus.Client.JSONSchema.TypeKind (TypeKind (..))
import qualified Data.Morpheus.Client.JSONSchema.TypeRef as Ref
import Data.Morpheus.Client.JSONSchema.Types
  ( EnumValue (..),
    Field (..),
    InputValue (..),
    Introspection (..),
    Schema (..),
    Type (..),
  )
import Data.Morpheus.Core
  ( defaultConfig,
    validateSchema,
  )
import Data.Morpheus.Error (globalErrorMessage)
import Data.Morpheus.Internal.Utils
  ( fromElems,
  )
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
    Message,
    OUT,
    TypeContent (..),
    TypeDefinition (..),
    TypeName,
    TypeWrapper,
    VALID,
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
    failure,
  )
import Relude hiding
  ( ByteString,
    Type,
    show,
  )
import Prelude (show)

decoderError :: Message -> Eventless a
decoderError = failure . globalErrorMessage

decodeIntrospection :: ByteString -> Eventless (AST.Schema VALID)
decodeIntrospection jsonDoc = case jsonSchema of
  Left errors -> decoderError $ msg errors
  Right
    JSONResponse
      { responseData =
          Just
            Introspection
              { __schema =
                  Schema
                    { types,
                      queryType,
                      mutationType,
                      subscriptionType
                    }
              }
      } ->
      traverse parse types >>= fromElems . concat >>= validate
  Right res -> decoderError (msg $ show res)
  where
    validate :: AST.Schema CONST -> Eventless (AST.Schema VALID)
    validate = validateSchema False defaultConfig
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
      Nothing -> decoderError "ERROR: GQL ERROR"
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
    fieldTypeRec _ x = decoderError $ "Unsuported Field" <> msg (show x)
