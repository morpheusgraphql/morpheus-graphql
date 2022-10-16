{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Transform.PreDeclarations
  ( mapPreDeclarations,
  )
where

import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toJSON),
  )
import Data.Morpheus.Client.Fetch.RequestType
  ( RequestType (..),
  )
import Data.Morpheus.Client.Internal.AST
  ( ClientDeclaration (..),
    ClientMethod (..),
    ClientPreDeclaration (..),
    DERIVING_MODE (..),
    Printable (..),
    RequestTypeDefinition (..),
  )
import Data.Morpheus.Client.Internal.TH
  ( MValue (..),
  )
import Data.Morpheus.Client.Internal.Utils
  ( emptyTypeError,
    invalidConstructorError,
  )
import Data.Morpheus.CodeGen.Internal.AST
  ( AssociatedType (AssociatedTypeName),
    CodeGenConstructor (constructorName),
    CodeGenType (..),
    CodeGenTypeName (typename),
    MethodArgument (..),
    TypeClassInstance (..),
    fromTypeName,
    getFullName,
  )
import Data.Morpheus.CodeGen.TH
  ( ToName (toName),
  )
import Data.Morpheus.Types.GQLScalar
  ( scalarFromJSON,
    scalarToJSON,
  )
import Relude hiding (ToString, Type, toString)

mapPreDeclarations :: MonadFail m => ClientPreDeclaration -> m ClientDeclaration
mapPreDeclarations (FromJSONClass mode dec) = InstanceDeclaration <$> deriveFromJSON mode dec
mapPreDeclarations (ToJSONClass mode clientDef) = InstanceDeclaration <$> deriveToJSON mode clientDef
mapPreDeclarations (ClientType c) = pure $ ClientTypeDeclaration c
mapPreDeclarations (RequestTypeClass req) = pure $ InstanceDeclaration (getRequestInstance req)

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
      [ ('__name, ProxyArgument, PrintableMethod $ Printable requestName),
        ('__query, ProxyArgument, PrintableMethod $ Printable requestQuery),
        ('__type, ProxyArgument, PrintableMethod $ Printable requestType)
      ]

-- FromJSON
deriveFromJSONMethod :: MonadFail m => DERIVING_MODE -> CodeGenType -> m ClientMethod
deriveFromJSONMethod SCALAR_MODE _ = pure $ FunctionNameMethod 'scalarFromJSON
deriveFromJSONMethod _ CodeGenType {cgConstructors = [], ..} = emptyTypeError cgTypeName
deriveFromJSONMethod ENUM_MODE CodeGenType {..} =
  pure $
    MatchMethod $
      map (fromJSONEnum . constructorName) cgConstructors
        <> [MFunction "v" 'invalidConstructorError]
deriveFromJSONMethod _ CodeGenType {cgConstructors = [cons]} = pure $ FromJSONObjectMethod cons
deriveFromJSONMethod _ typeD = pure $ FromJSONUnionMethod typeD

deriveToJSONMethod :: MonadFail m => DERIVING_MODE -> CodeGenType -> m (MethodArgument, ClientMethod)
deriveToJSONMethod SCALAR_MODE _ = pure (NoArgument, FunctionNameMethod 'scalarToJSON)
deriveToJSONMethod _ CodeGenType {cgConstructors = [], ..} = emptyTypeError cgTypeName
deriveToJSONMethod ENUM_MODE CodeGenType {cgConstructors} =
  pure
    ( NoArgument,
      MatchMethod $ map (toJSONEnum . constructorName) cgConstructors
    )
deriveToJSONMethod _ CodeGenType {cgConstructors = [cons]} = pure (DestructArgument cons, ToJSONObjectMethod cons)
deriveToJSONMethod _ _ = fail "Input Unions are not yet supported"

toJSONEnum :: CodeGenTypeName -> MValue
toJSONEnum name = MTo (getFullName name) (typename name)

fromJSONEnum :: CodeGenTypeName -> MValue
fromJSONEnum name = MFrom (typename name) (getFullName name)

deriveToJSON :: MonadFail m => DERIVING_MODE -> CodeGenType -> m (TypeClassInstance ClientMethod)
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

deriveFromJSON :: MonadFail m => DERIVING_MODE -> CodeGenType -> m (TypeClassInstance ClientMethod)
deriveFromJSON mode cType = do
  expr <- deriveFromJSONMethod mode cType
  pure
    TypeClassInstance
      { typeClassName = ''FromJSON,
        typeClassContext = [],
        typeClassTarget = cgTypeName cType,
        assoc = [],
        typeClassMethods = [('parseJSON, NoArgument, expr)]
      }
