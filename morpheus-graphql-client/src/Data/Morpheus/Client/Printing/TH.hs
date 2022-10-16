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
    CodeGenTypeName (..),
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
typeDeclarations (FromJSONClass mode clientDef) = deriveIfNotDefined (deriveFromJSON mode >=> printDec) ''FromJSON clientDef
typeDeclarations (ToJSONClass mode clientDef) = deriveIfNotDefined (deriveToJSON mode >=> printDec) ''ToJSON clientDef
typeDeclarations (ClientType c) = declareIfNotDeclared printDec c
typeDeclarations (RequestTypeClass RequestTypeDefinition {..}) = do
  pure
    <$> printDec
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

mkFromJSON :: CodeGenTypeName -> ClientMethod -> TypeClassInstance ClientMethod
mkFromJSON name expr =
  TypeClassInstance
    { typeClassName = ''FromJSON,
      typeClassContext = [],
      typeClassTarget = name,
      assoc = [],
      typeClassMethods = [('parseJSON, NoArgument, expr)]
    }

mkToJSON :: CodeGenTypeName -> (MethodArgument, ClientMethod) -> TypeClassInstance ClientMethod
mkToJSON name (args, expr) =
  TypeClassInstance
    { typeClassName = ''ToJSON,
      typeClassContext = [],
      typeClassTarget = name,
      assoc = [],
      typeClassMethods = [('toJSON, args, expr)]
    }

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
deriveToJSON mode cType = mkToJSON (cgTypeName cType) <$> deriveToJSONMethod mode cType

deriveFromJSON :: MonadFail m => DERIVING_MODE -> CodeGenType -> m (TypeClassInstance ClientMethod)
deriveFromJSON mode cType = mkFromJSON (cgTypeName cType) <$> deriveFromJSONMethod mode cType
