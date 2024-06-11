{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.CodeGen.Interpreting.PreDeclarations
  ( mapPreDeclarations,
  )
where

import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toJSON),
  )
import Data.Aeson.Types ((.:), (.:?), (.=))
import Data.Morpheus.Client.CodeGen.AST
  ( AesonField,
    ClientDeclaration (..),
    ClientMethod (..),
    ClientPreDeclaration (..),
    DERIVING_MODE (..),
    MValue (..),
    RequestTypeDefinition (..),
    UnionPat (..),
  )
import Data.Morpheus.Client.CodeGen.Internal (invalidConstructorError, omitNulls)
import Data.Morpheus.Client.Fetch.RequestType
  ( RequestType (..),
  )
import Data.Morpheus.CodeGen.Internal.AST
  ( AssociatedType (AssociatedTypeName),
    CodeGenConstructor (..),
    CodeGenField (..),
    CodeGenType (..),
    CodeGenTypeName (typename),
    MethodArgument (..),
    PrintableValue (..),
    TypeClassInstance (..),
    fromTypeName,
    getFullName,
  )
import Data.Morpheus.CodeGen.TH
  ( ToName (toName),
  )
import Data.Morpheus.CodeGen.Utils (camelCaseFieldName)
import Data.Morpheus.Types.GQLScalar
  ( scalarFromJSON,
    scalarToJSON,
  )
import Data.Morpheus.Types.Internal.AST (Msg (..), internal)
import Language.Haskell.TH.Syntax (Name)
import Relude hiding (ToString, Type, toString)

mapPreDeclarations :: (MonadFail m) => ClientPreDeclaration -> m ClientDeclaration
mapPreDeclarations (FromJSONClass mode dec) = InstanceDeclaration mode <$> deriveFromJSON mode dec
mapPreDeclarations (FromJSONObjectClass cType CodeGenConstructor {..}) =
  pure $ InstanceDeclaration TYPE_MODE $ mkFromJSON cType $ FromJSONObjectMethod (getFullName constructorName) (map defField constructorFields)
mapPreDeclarations (FromJSONUnionClass cType constructors) = pure $ InstanceDeclaration TYPE_MODE $ mkFromJSON cType $ FromJSONUnionMethod $ map mkMatch constructors
  where
    mkMatch (tag, (consName, typeName)) = ([tag, UVar $ if isJust typeName then "v" else "_"], (toName consName, fmap toName typeName))
mapPreDeclarations (ToJSONClass mode clientDef) = InstanceDeclaration mode <$> deriveToJSON mode clientDef
mapPreDeclarations (ClientType c) = pure $ ClientTypeDeclaration c
mapPreDeclarations (RequestTypeClass req) = pure $ InstanceDeclaration TYPE_MODE (getRequestInstance req)

getRequestInstance :: RequestTypeDefinition -> TypeClassInstance ClientMethod
getRequestInstance RequestTypeDefinition {..} =
  TypeClassInstance
    { typeClassName = ''RequestType,
      typeClassContext = [],
      typeClassTarget = fromTypeName requestName,
      assoc = [(''RequestArgs, AssociatedTypeName $ toName requestArgs)],
      typeClassMethods
    }
  where
    typeClassMethods =
      [ ('__name, ProxyArgument, PrintableMethod $ PrintableValue requestName),
        ('__query, ProxyArgument, PrintableMethod $ PrintableValue requestQuery),
        ('__type, ProxyArgument, PrintableMethod $ PrintableValue requestType)
      ]

-- FromJSON
deriveFromJSONMethod :: (MonadFail m) => DERIVING_MODE -> CodeGenType -> m ClientMethod
deriveFromJSONMethod SCALAR_MODE _ = pure $ FunctionNameMethod 'scalarFromJSON
deriveFromJSONMethod ENUM_MODE CodeGenType {..} =
  pure
    $ MatchMethod
    $ map (fromJSONEnum . constructorName) cgConstructors
    <> [MFunction "v" 'invalidConstructorError]
deriveFromJSONMethod _ CodeGenType {..} = emptyTypeError cgTypeName

defField :: CodeGenField -> AesonField
defField CodeGenField {..} = (toName ("v" :: String), bindField fieldIsNullable, fieldName)

bindField :: Bool -> Name
bindField nullable
  | nullable = '(.:?)
  | otherwise = '(.:)

deriveToJSONMethod :: (MonadFail m) => DERIVING_MODE -> CodeGenType -> m (MethodArgument, ClientMethod)
deriveToJSONMethod SCALAR_MODE _ = pure (NoArgument, FunctionNameMethod 'scalarToJSON)
deriveToJSONMethod _ CodeGenType {cgConstructors = [], ..} = emptyTypeError cgTypeName
deriveToJSONMethod ENUM_MODE CodeGenType {cgConstructors} =
  pure
    ( NoArgument,
      MatchMethod $ map (toJSONEnum . constructorName) cgConstructors
    )
deriveToJSONMethod _ CodeGenType {cgConstructors = [CodeGenConstructor {..}]} =
  pure
    ( DestructArgument (toName constructorName) (map (\(_, _, v) -> v) entries),
      ToJSONObjectMethod 'omitNulls entries
    )
  where
    entries = map mkEntry constructorFields
    mkEntry CodeGenField {fieldName} =
      ( fieldName,
        '(.=),
        toName $ camelCaseFieldName (getFullName constructorName) fieldName
      )
deriveToJSONMethod _ _ = fail "Input Unions are not yet supported"

toJSONEnum :: CodeGenTypeName -> MValue
toJSONEnum name = MTo (getFullName name) (typename name)

fromJSONEnum :: CodeGenTypeName -> MValue
fromJSONEnum name = MFrom (typename name) (getFullName name)

deriveToJSON :: (MonadFail m) => DERIVING_MODE -> CodeGenType -> m (TypeClassInstance ClientMethod)
deriveToJSON mode cType = do
  (args, expr) <- deriveToJSONMethod mode cType
  pure
    TypeClassInstance
      { typeClassName = ''ToJSON,
        typeClassContext = [],
        typeClassTarget = cgTypeName cType,
        assoc = [],
        typeClassMethods = [('toJSON, args, expr)]
      }

mkFromJSON :: CodeGenTypeName -> body -> TypeClassInstance body
mkFromJSON typeClassTarget expr =
  TypeClassInstance
    { typeClassName = ''FromJSON,
      typeClassContext = [],
      typeClassTarget = typeClassTarget,
      assoc = [],
      typeClassMethods = [('parseJSON, NoArgument, expr)]
    }

deriveFromJSON :: (MonadFail m) => DERIVING_MODE -> CodeGenType -> m (TypeClassInstance ClientMethod)
deriveFromJSON mode cType = mkFromJSON (cgTypeName cType) <$> deriveFromJSONMethod mode cType

emptyTypeError :: (MonadFail m) => CodeGenTypeName -> m a
emptyTypeError name = fail $ show $ internal ("Type " <> msg (getFullName name) <> " Should Have at least one Constructor")
