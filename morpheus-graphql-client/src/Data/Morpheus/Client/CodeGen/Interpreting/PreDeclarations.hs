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
    Printable (..),
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
deriveFromJSONMethod _ CodeGenType {cgConstructors = [CodeGenConstructor {..}]} = pure $ FromJSONObjectMethod (getFullName constructorName) (map defField constructorFields)
deriveFromJSONMethod _ CodeGenType {..} = pure $ FromJSONUnionMethod $ map f cgConstructors <> elseCondition
  where
    interfaceConstructor = map genObj (maybeToList $ find ((typename cgTypeName ==) . typename . constructorName) cgConstructors)
    elseCondition = map ([UVar "_", UVar "v"],) interfaceConstructor
    f cons@CodeGenConstructor {..} = ([UString $ typename constructorName, if null constructorFields then UVar "_" else UVar "v"], genObj cons)

genObj :: CodeGenConstructor -> (Name, [AesonField])
genObj CodeGenConstructor {..} = (toName constructorName, map defField constructorFields)

defField :: CodeGenField -> AesonField
defField CodeGenField {..} = (toName ("v" :: String), bindField fieldIsNullable, fieldName)

bindField :: Bool -> Name
bindField nullable
  | nullable = '(.:?)
  | otherwise = '(.:)

deriveToJSONMethod :: MonadFail m => DERIVING_MODE -> CodeGenType -> m (MethodArgument, ClientMethod)
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

emptyTypeError :: MonadFail m => CodeGenTypeName -> m a
emptyTypeError name = fail $ show $ internal ("Type " <> msg (getFullName name) <> " Should Have at least one Constructor")
