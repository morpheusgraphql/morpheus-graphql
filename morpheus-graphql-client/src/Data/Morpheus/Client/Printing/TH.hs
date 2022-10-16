{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Printing.TH
  ( printDeclarations,
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
    RequestTypeDefinition (..),
  )
import Data.Morpheus.Client.Internal.TH
  ( declareIfNotDeclared,
    deriveIfNotDefined,
    fromJSONEnumMethod,
    fromJSONObjectMethod,
    fromJSONUnionMethod,
  )
import Data.Morpheus.Client.Internal.Utils
  ( emptyTypeError,
  )
import Data.Morpheus.CodeGen.Internal.AST
  ( AssociatedType (AssociatedTypeName),
    CodeGenType (..),
    MethodArgument (..),
    TypeClassInstance (..),
    fromTypeName,
  )
import Data.Morpheus.CodeGen.TH
  ( PrintDec (printDec),
    ToName (toName),
  )
import Data.Morpheus.Types.GQLScalar
  ( scalarFromJSON,
    scalarToJSON,
  )
import Language.Haskell.TH (Dec, Q)
import Relude hiding (ToString, Type, toString)

printDeclarations :: [ClientDeclaration] -> Q [Dec]
printDeclarations clientType = concat <$> traverse typeDeclarations clientType

typeDeclarations :: ClientDeclaration -> Q [Dec]
typeDeclarations (InstanceDeclaration dec) = deriveIfNotDefined printDec dec
typeDeclarations (ClientTypeDeclaration c) = declareIfNotDeclared printDec c

transformDeclarations :: MonadFail m => ClientPreDeclaration -> m ClientDeclaration
transformDeclarations (FromJSONClass mode dec) = InstanceDeclaration <$> deriveFromJSON mode dec
transformDeclarations (ToJSONClass mode clientDef) = InstanceDeclaration <$> deriveToJSON mode clientDef
transformDeclarations (ClientType c) = pure $ ClientTypeDeclaration c
transformDeclarations (RequestTypeClass req) = pure $ InstanceDeclaration (getRequestInstance req)

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
      [ ('__name, ProxyArgument, ClientMethodExp [|requestName|]),
        ('__query, ProxyArgument, ClientMethodExp [|requestQuery|]),
        ('__type, ProxyArgument, ClientMethodExp [|requestType|])
      ]

-- FromJSON
deriveFromJSONMethod :: MonadFail m => DERIVING_MODE -> CodeGenType -> m ClientMethod
deriveFromJSONMethod SCALAR_MODE _ = pure $ ClientMethodExp [|scalarFromJSON|]
deriveFromJSONMethod _ CodeGenType {cgConstructors = [], ..} = emptyTypeError cgTypeName
deriveFromJSONMethod ENUM_MODE CodeGenType {..} = pure $ ClientMethodExp (fromJSONEnumMethod cgConstructors)
deriveFromJSONMethod _ CodeGenType {cgConstructors = [cons]} = pure $ ClientMethodExp (fromJSONObjectMethod cons)
deriveFromJSONMethod _ typeD = pure $ ClientMethodExp (fromJSONUnionMethod typeD)

deriveToJSONMethod :: MonadFail m => DERIVING_MODE -> CodeGenType -> m (MethodArgument, ClientMethod)
deriveToJSONMethod SCALAR_MODE _ = pure (NoArgument, ClientMethodExp [|scalarToJSON|])
deriveToJSONMethod _ CodeGenType {cgConstructors = [], ..} = emptyTypeError cgTypeName
deriveToJSONMethod ENUM_MODE CodeGenType {cgConstructors} = pure (NoArgument, ToJSONEnumMethod cgConstructors)
deriveToJSONMethod _ CodeGenType {cgConstructors = [cons]} = pure (DestructArgument cons, ToJSONObjectMethod cons)
deriveToJSONMethod _ _ = fail "Input Unions are not yet supported"

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
